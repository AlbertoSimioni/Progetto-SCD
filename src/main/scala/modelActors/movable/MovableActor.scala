package modelActors.movable

import scala.concurrent.duration._

import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.PoisonPill
import akka.contrib.pattern.ShardRegion
import akka.persistence.PersistentActor
import akka.persistence.SaveSnapshotSuccess
import akka.persistence.SaveSnapshotFailure
import akka.persistence.SnapshotOffer
import akka.persistence.RecoveryFailure
import akka.persistence.RecoveryCompleted
import akka.persistence.PersistenceFailure
import akka.contrib.pattern.ClusterSharding
import akka.persistence.AtLeastOnceDelivery
import akka.contrib.pattern.DistributedPubSubMediator
import akka.contrib.pattern.DistributedPubSubExtension
import DistributedPubSubMediator.Subscribe
import DistributedPubSubMediator.Unsubscribe
import DistributedPubSubMediator.SubscribeAck
import DistributedPubSubMediator.UnsubscribeAck

import modelActors.immovable.ImmovableActor
import modelActors.Messages._
import common.CommonMessages._
import common.ToPersistentMessages
import common.ToNonPersistentMessages
import time.TimeCostraints._
import time.TimeMessages._
import map.JSONReader
import map.PointsSequence._
import map.Domain._
import map.Domain.category._
import map.Routes._
import MovableState._
import pubsub.PublisherInstance
import pubsub.Messages._
import pubsub.Utility._

/**
 * @author Matteo Pozza
 * 
 */
object MovableActor {
  
  // props appropriato
  def props(id: String): Props = Props(new MovableActor(id))
  
  // PERSISTENCE
  // Messaggio per il salvataggio e la cancellazione di uno snapshot
  case object SaveSnapshot
  case class DeleteSnapshot(sequenceNr : Long, timestamp : Long)
  
  // TIME
  // Utilizzato per gestire gli avanzamenti negli spostamenti
  case object VelocityTick
  
  // SHUTDOWN
  // La gestione della poison pill è ambigua, utilizzo un messaggio dedicato:
  // http://getakka.net/docs/persistence/persistent-actors
  case object Shutdown
  
}

class MovableActor(id : String) extends PersistentActor with AtLeastOnceDelivery {
  
  import context.dispatcher
  
  import MovableActor._
  
  // GUI
  // riferimento per la pubblicazione di eventi grafici
  val publisherGuiHandler = PublisherInstance.getPublisherModelEvents(context.system)
  
  // PERSISTENCE
  override def persistenceId: String = "MovableActor-" + id
  
  // PERSISTENCE
  // Permette di effettuare il salvataggio dello snapshot ogni 10 secondi
  val snapshotTimer = context.system.scheduler.schedule(0 millis, 10000 millis, self, SaveSnapshot)
  
  // SHARDING
  // Permette di comunicare con altri ImmovableActor utilizzando il loro identificativo invece che il loro indirizzo
  val shardRegion = ClusterSharding(context.system).shardRegion(ImmovableActor.typeOfEntries)
  
  // TIME
  // Recupera l'attore che gli permette di ricevere gli eventi temporali e si sottoscrive immediatamente
  val mediator = DistributedPubSubExtension(context.system).mediator
  mediator ! Subscribe(timeMessage, self)
  
  // TIME
  // Tick utilizzato solo negli avanzamenti
  val velocityTimer = context.system.scheduler.schedule(0 millis, Duration(getVelocityTickInterval(id), "millis"), self, VelocityTick)
  
  // VELOCITY
  // interruttore per l'interessamento ai tick di velocità o meno
  var interestedInVelocityTick = false
  
  // DOMINIO
  // Riferimento al veicolo successivo
  var nextVehicle : ActorRef = null
  // ultima posizione del veicolo successivo
  var nextVehicleLastPosition = point(-1, -1)
  // Riferimento al veicolo precedente
  var previousVehicle : ActorRef = null
  
  //Id della lane precedente
  var previousLaneId : String = null
  
  // variabile che modella dove siamo arrivati nel percorso
  var pathPhase = 0
  
  // NON-PERSISTENT DATA
  // lista di punti e indice di avanzamento per gli step che non sono da rendere persistenti
  var currentNonPersistentPointsSequence : List[List[point]] = null
  var currentNonPersistentPointIndex : Int = 0
  
  // PERSISTENCE
  // Lo stato dell'attore deve essere modellato da un var
  var state = new MovableState()
  
  // PERSISTENCE
  // numero e timestamp del precedente snapshot
  var previousSequenceNr = -1l
  var previousTimestamp = -1l
  
  // PERSISTENCE
  // Sostituisce la receive normale
  override def receiveCommand: Receive = {
    
    case ToMovable(destinationRef, toMovableMessage) => toMovableMessage match {
      case ToPersistentMessages.FromImmovable(senderId, payload) => payload match {
        case Request(deliveryId, command) =>
          // manda ack al mittente
          shardRegion ! ToImmovable(senderId, ToPersistentMessages.FromMovable(id, self, Ack(deliveryId)))
          // controlla che il messaggio non sia duplicato
          if(state.isNewMessage(senderId, deliveryId) == true) {
            // messaggio nuovo
            persist(NoDuplicate(senderId, deliveryId)) { msg => }
            // persist body begin
            state.updateFilter(senderId, deliveryId)
            // persist body end
            // gestione vera e propria del messaggio
            if(senderId == "CAR0000007" || senderId == "CAR0000008") {
              printMessage(senderId, id, command)
            }
            command match {
              case IpResponse(ipAddress) =>
                if(isLocal(ipAddress)) {
                  // effettua il cambio gestione
                  sendToImmovable(id, self, senderId, MobileEntityAdd(id))
                  val previousHandler = state.getPreviousStepId
                  sendToImmovable(id, self, previousHandler, MobileEntityRemove(id))
                  // esegui lo step corrente
                  sendToMovable(id, self, self, ExecuteCurrentStep)
                }
                else {
                  // trasferimento di nodo di calcolo, con cambio di gestione
                  sendToImmovable(id, self, senderId, MobileEntityAdd(id))
                  sendToImmovable(id, self, senderId, ReCreateMe(id))
                  // informa il precedente gestore
                  val previousHandler = state.getPreviousStepId
                  sendToImmovable(id, self, previousHandler, MobileEntityRemove(id))
                  // ammazzati
                  shutdown()
                }
              case Route(route) =>
                // è arrivato il percorso
                persist(RouteArrived(route)) { evt => }
                // persist body begin
                state.handleRoute(route)
                // persist body end
              case ResumeExecution =>
                // comincia (o riprendi) l'esecuzione
                sendToImmovable(id, self, state.getCurrentStepId, IpRequest)
              case MovableActorResponse(id, ref) =>
                // per il momento, l'unico caso in cui possiamo ricevere questa risposta è per la richiesta del previousVehicle
                assert(id == state.previousVehicleId)
                previousVehicle = ref
                // fai ripartire l'esecuzione
                sendToMovable(this.id, self, self, ExecuteCurrentStep)
                
              case ToPedestrian(command) =>
                Pedestrian.fromImmovableHandler(this, id, senderId, command)
              case ToCar(command) =>
                Car.fromImmovableHandler(this, id, senderId, command)
              case ToBus(command) =>
                Bus.fromImmovableHandler(this, id, senderId, command)
              case ToTram(command) =>
                Tram.fromImmovableHandler(this, id, senderId, command)
              case _ =>
                println("We should not be here!")
            }
          }
        case Ack(deliveryId) =>
          // ack ricevuto, messaggio è stato consegnato con successo
          val ackAboutNewMessage = confirmDelivery(deliveryId)
      }
      case ToPersistentMessages.FromMovable(senderId, senderRef, payload) => payload match {
        case Request(deliveryId, command) =>
          // manda ack al mittente
          senderRef ! ToMovable(senderRef, ToPersistentMessages.FromMovable(id, self, Ack(deliveryId)))
          // controlla che il messaggio non sia duplicato
          if(state.isNewMessage(senderId, deliveryId) == true) {
            // messaggio nuovo
            persist(NoDuplicate(senderId, deliveryId)) { msg => }
            // persist body begin
            state.updateFilter(senderId, deliveryId)
            // persist body end
            // handling vero e proprio del messaggio
            if(senderId == "CAR0000007" || senderId == "CAR0000008") {
              printMessage(senderId, id, command)
            }
            command match {
              case ExecuteCurrentStep =>
                val stepSequence = state.getStepSequence()
                state.currentRoute(state.index) match {
                  case road_step(road, direction) =>
                    // solo un pedone può avere road_step
                    if(state.beginOfTheStep) {
                      val currentPointsSequence = getPointsSequence(id, stepSequence)
                      persist(BeginOfTheStep(currentPointsSequence)) { evt => }
                      // persist body begin
                      state.currentPointsSequence = currentPointsSequence
                      state.currentPointIndex = 0
                      state.beginOfTheStep = false
                      // persist body end
                    }
                    // attiva l'interessamento agli eventi di avanzamento
                    interestedInVelocityTick = true
                  case lane_step(lane, direction) =>
                    // sto approciando una lane
                    // potrei prtire da una zona e non aver cominciato il percorso
                    if(state.fromZone() == true && state.beginOfTheStep == true) {
                      // in tal caso, fai la richiesta alla lane
                      val firstPoint = getPointsSequence(id, stepSequence)(0)(0)
                      sendToImmovable(id, self, lane.id, envelope(id, lane.id, LaneAccessRequest(firstPoint, direction)))
                    }
                    else {
                      // potremmo essere nel caso in cui non siamo riusciti a mandare PredecessorGone prima del crash del nodo in cui siamo
                      // la situazione è segnalata dal flag booleano predecessorGoneSent
                      // in questo caso, bisogna recuperare il riferimento al veicolo precedente prima di procedere
                      if(previousVehicle == null && state.predecessorGoneSent == false) {
                        // l'unico caso in cui possiamo essere è che non siamo riusciti ad inviare PredecessorGone prima di morire
                        // recupera id della lane precedente
                        var previousLaneId : String = null
                        if(JSONReader.getLane(current_map, state.getStepIdAt(-2)).isEmpty == false) {
                          previousLaneId = state.getStepIdAt(-2)
                        }
                        else {
                          // avevamo due incroci prima
                          previousLaneId = state.getStepIdAt(-3)
                        }
                        // manda un messaggio di richiesta del veicolo in questione
                        sendToImmovable(id, self, previousLaneId, MovableActorRequest(state.previousVehicleId))
                      }
                      else {
                        // per prima cosa, qualora vi fosse un veicolo predecessore, informalo che non deve più seguirci
                        if(previousVehicle != null) {
                          sendToMovable(id, self, previousVehicle, envelope(id, state.previousVehicleId, PredecessorGone))
                          persist(PredecessorGoneSent) { evt => }
                          // persist body begin
                          state.previousVehicleId = null
                          state.predecessorGoneSent = true
                          // persist body end
                          previousVehicle = null
                        }
                        // se c'è una qualche previousLane, avvisala di modificare i campi lastVehicle (qualora fossimo stati l'unico veicolo)
                        if(previousLaneId != null) {
                          sendToImmovable(id, self, previousLaneId, envelope(id, previousLaneId, HandleLastVehicle))
                          previousLaneId = null
                        }
                        // a prescindere da primo approccio allo step o ripristino, l'unico dato di cui dispongo
                        // è l'eventuale id del next vehicle
                        if(state.nextVehicleId == null) {
                          // dobbiamo ancora iniziare
                          // avvisa l'entità precedente (incrocio/ strisce pedonali, etc.) che sei ufficialmente sulla nuova lane
                          // in questo modo, l'entità in questione rende persistente l'occupazione della lane considerata.
                          var previousId = state.getPreviousStepId
                          // recupera anche l'id della lane da cui provenivi
                          var previousLaneId : String = null
                          if(JSONReader.getLane(current_map, state.getStepIdAt(-2)).isEmpty == false) {
                            previousLaneId = state.getStepIdAt(-2)
                          }
                          else {
                            // avevamo due incroci prima
                            val previousCrossroad = JSONReader.getCrossroad(current_map, previousId).get
                            val targetCrossroad = getClassicCrossroad(previousCrossroad)
                            previousId = targetCrossroad.id
                            previousLaneId = state.getStepIdAt(-3)
                          }
                          sendToImmovable(id, self, previousId, envelope(id, previousId, VehicleBusy(previousLaneId)))
                          // manda un messaggio alla lane corrente per ricevere il ref
                          sendToImmovable(id, self, lane.id, envelope(id, lane.id, NextVehicleFirstRequest))
                        }
                        else {
                          // sicuramente siamo in ripristino
                          // recuperiamo il ref
                          // controlliamo se sono l'ultimo della lane o no
                          var flag = false
                          if(state.previousVehicleId == null || state.predecessorGoneSent == false) {
                            flag = true
                          }
                          sendToImmovable(id, self, lane.id, envelope(id, lane.id, NextVehicleRequest(state.nextVehicleId, flag)))
                        }
                      }
                    }
                    
                  case crossroad_step(crossroad, direction) =>
                    if(state.fromCrossroad() && getMyLength() != pedestrian_length) {
                      // siamo un veicolo e siamo appena stati su un altro incrocio
                      // fai solo persist and next step
                      sendToMovable(id, self, self, PersistAndNextStep)
                    }
                    else {
                      // se siamo ad un incrocio, vi sono due possibilità:
                      // - sono un pedone -> carico il percorso e procedo senza indugio
                      // - sono un veicolo -> faccio vehicle_in, quando ricevo vehicle_out procedo senza indugio
                      pathPhase = 0
                      currentNonPersistentPointsSequence = getPointsSequence(id, stepSequence)
                      currentNonPersistentPointIndex = 0
                      if(getMyLength() == pedestrian_length) {
                        interestedInVelocityTick = true
                      }
                      else {
                        if(state.toCrossroad()) {
                          // siamo un veicolo e stiamo affrontando un incrocio doppio
                          // vogli fare la richiesta all'incrocio classic
                          val target = getClassicCrossroad(crossroad)
                          // recupera l'id della lane precedente
                          val previousId = state.getPreviousStepId
                          // richiesta all'incrocio
                          sendToImmovable(id, self, target.id, envelope(id, target.id, Vehicle_In(previousId)))
                        }
                        else {
                          if(crossroad.category == `nil` || crossroad.category == `angle`) {
                            interestedInVelocityTick = true
                          }
                          else {
                            // recupera l'id della lane precedente
                            val previousId = state.getPreviousStepId
                            // richiesta all'incrocio
                            sendToImmovable(id, self, crossroad.id, envelope(id, crossroad.id, Vehicle_In(previousId)))
                          }
                        }
                      }
                    }
                    
                  case pedestrian_crossroad_step(pedestrian_crossroad, direction) =>
                    pathPhase = 0
                    // vi sono due possibilità: sono un pedone, o sono un veicolo
                    // se sono un pedone, vi sono due possibilità: o le ignoro (ho un solo percorso), o le affronto (ho 3 percorsi)
                    // se sono un veicolo, ho un solo percorso e le devo affrontare
                    // ricordiamoci che non dobbiamo salvare nulla
                    currentNonPersistentPointsSequence = getPointsSequence(id, stepSequence)
                    currentNonPersistentPointIndex = 0
                    if(currentNonPersistentPointsSequence.length > 1) {
                      // siamo sicuramente un pedone che deve affrontare le strisce
                      // possiamo avanzare liberamente fino alla fine della prima sequenza di punti
                      interestedInVelocityTick = true
                    }
                    else {
                      // possiamo essere un pedone che ignora o un veicolo
                      // test balordo per capire chi siamo
                      if(getMyLength() == pedestrian_length) {
                        // attiva semplicemente l'interesse per i velocity tick
                        interestedInVelocityTick = true
                      }
                      else {
                        // recupera l'id della lane precedente
                        val previousId = state.getPreviousStepId
                        // richiesta alle strisce
                        sendToImmovable(id, self, pedestrian_crossroad.id, envelope(id, pedestrian_crossroad.id, Vehicle_In(previousId)))
                      }
                    }
                  case bus_stop_step(bus_stop, direction, ignore) =>
                    pathPhase = 0
                    // sto affontando una fermata del bus
                    // se sono un pedone, a prescindere se le ignoro o meno, posso procedere fino alla fine del percorso
                    // se sono un veicolo, a prescindere se le ignoro o meno, devo fare la richiesta vehicle_in
                    currentNonPersistentPointsSequence = getPointsSequence(id, stepSequence)
                    currentNonPersistentPointIndex = 0
                    // test balordo per capire se siamo un pedone o no
                    if(getMyLength() == pedestrian_length) {
                      interestedInVelocityTick = true
                    }
                    else {
                      // recupera l'id della lane precedente
                      val previousId = state.getPreviousStepId
                      // richiesta alle fermata del bus
                      sendToImmovable(id, self, bus_stop.id, envelope(id, bus_stop.id, Vehicle_In(previousId)))
                    }
                  case tram_stop_step(tram_stop, direction, ignore) =>
                    pathPhase = 0
                    // sto affontando una fermata del tram
                    // se sono un pedone, a prescindere se le ignoro o meno, posso procedere fino alla fine del percorso
                    // se sono un veicolo, a prescindere se le ignoro o meno, devo fare la richiesta vehicle_in
                    currentNonPersistentPointsSequence = getPointsSequence(id, stepSequence)
                    currentNonPersistentPointIndex = 0
                    // test balordo per capire se siamo un pedone o no
                    if(getMyLength() == pedestrian_length) {
                      interestedInVelocityTick = true
                    }
                    else {
                      // recupera l'id della lane precedente
                      val previousId = state.getPreviousStepId
                      // richiesta alle fermata del bus
                      sendToImmovable(id, self, tram_stop.id, envelope(id, tram_stop.id, Vehicle_In(previousId)))
                    }
                  case zone_step(zone, direction) =>
                    // PRECONDIZIONE: solo un pedone o un veicolo possono avere uno zone_step nel loro percorso
                    // siamo dunque autorizzati ad attuare la logica di dormi/veglia
                    //
                    // per quanto riguarda l'interfaccia grafica, noi siamo scomparsi, a prescindere se siamo in ritardo o in anticipo
                    // lancia evento per l'interfaccia grafica
                    if(getMyLength() == pedestrian_length) {
                      publisherGuiHandler ! hidePedestrian(id, zone.id, false)
                    }
                    else {
                      assert(getMyLength() == car_length)
                      publisherGuiHandler ! hideCar(id, zone.id)
                    }
                    // prima cosa: capisci di che zona si tratta
                    var comparedTime : TimeValue = null
                    zone.variety match {
                      case variety.`houseplace` =>
                        if(state.pedestrianRoute != null) {
                          comparedTime = state.pedestrianRoute.houseEndTime
                        }
                        else {
                          comparedTime = state.carRoute.houseEndTime
                        }
                      case variety.`workplace` =>
                        if(state.pedestrianRoute != null) {
                          comparedTime = state.pedestrianRoute.workEndTime
                        }
                        else {
                          comparedTime = state.carRoute.workEndTime
                        }
                      case variety.`funplace` =>
                        if(state.pedestrianRoute != null) {
                          comparedTime = state.pedestrianRoute.funEndTime
                        }
                        else {
                          comparedTime = state.carRoute.funEndTime
                        }
                    }
                    // seconda cosa: compara il tempo attuale con il tempo corrispondente
                    if(isLate(comparedTime, state.currentTime)) {
                      // se in ritardo, vai al prossimo step
                      sendToMovable(id, self, self, PersistAndNextStep)
                      // evento grafico associato
                      publisherGuiHandler ! entityAwaked(id, zone.id)
                    }
                    else {
                      // se in anticipo, vai a dormire
                      val immovableActorId = state.getCurrentStepId
                      sendToImmovable(id, self, immovableActorId, PauseExecution(comparedTime))
                      shutdown()
                    }
                    
                }
              case PersistAndNextStep =>
                // memorizza
                persist(NextStepEvent) { evt => }
                // persist body begin
                state.index = state.index + 1
                if(state.index >= state.currentRoute.length) {
                  state.handleIndexOverrun
                }
                // preoccupati anche del flag di inizio step
                state.beginOfTheStep = true
                // persist body end
                // avanti col prossimo step
                sendToImmovable(id, self, state.getCurrentStepId, IpRequest)
              
              case ToPedestrian(command) =>
                Pedestrian.fromMovableHandler(this, id, senderId, senderRef, command)
              case ToCar(command) =>
                Car.fromMovableHandler(this, id, senderId, senderRef, command)
              case ToBus(command) =>
                Bus.fromMovableHandler(this, id, senderId, senderRef, command)
              case ToTram(command) =>
                Tram.fromMovableHandler(this, id, senderId, senderRef, command)
              case _ =>
                println("We should not be here!")
            }
          }
        case Ack(deliveryId) =>
          // ack ricevuto, messaggio è stato consegnato con successo
          val ackAboutNewMessage = confirmDelivery(deliveryId)
      }
      case ToPersistentMessages.FromNonPersistent(senderRef, command) =>
        //
    }
    
    // PERSISTENCE
    case SaveSnapshot =>
      //state.deliveryState = getDeliverySnapshot
      //saveSnapshot(state)
    case SaveSnapshotSuccess(metadata) =>
      println("Snapshot stored successfully")
      val prevS = previousSequenceNr
      val prevT = previousTimestamp
      previousSequenceNr = metadata.sequenceNr
      previousTimestamp = metadata.timestamp
      if(prevS != -1 && prevT != -1) {
        self ! DeleteSnapshot(prevS, prevT)
      }
    case SaveSnapshotFailure(metadata, reason) =>
      println("Failed to store snapshot")
    case DeleteSnapshot(sequenceNr, timestamp) =>
      //deleteSnapshot(sequenceNr, timestamp)
      
    case PersistenceFailure(payload, sequenceNr, cause) =>
      println("Failed to persist an event!")
      
    // TIME
    case SubscribeAck =>
      println("Successfully subscribed to time events")
    case UnsubscribeAck =>
      println("Successfully unsubscribed from time events")
    case TimeCommand(timeValue) =>
      persist(TimeEvent(timeValue)) { evt => }
      // persist body begin
      state.currentTime = timeValue
      // persist body end
      
    // SHUTDOWN
    case Shutdown =>
      context.stop(self)
      
    // VELOCITY
    case VelocityTick =>
      if(interestedInVelocityTick) {
        state.currentRoute(state.index) match {
          case road_step(road, direction) =>
            // LANCIA EVENTO LEGATO AL PUNTO CORRENTE
            val currentPoint = state.currentPointsSequence(0)(state.currentPointIndex)
            publisherGuiHandler ! pedestrianPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(direction))
            
            // esamina dove siamo nella sequenza di punti
            if(state.currentPointIndex == state.currentPointsSequence(0).length-1) {
              // procedi al prossimo step
              sendToMovable(id, self, self, PersistAndNextStep)
              // fine del componente che stiamo percorrendo, spegni l'interruttore
              interestedInVelocityTick = false
            }
            else {
              persist(IncrementPointIndex) {evt => }
              // persist body begin
              state.currentPointIndex = state.currentPointIndex + 1 
              // persist body end
            }
          case lane_step(lane, direction) =>
            // LANCIA EVENTO LEGATO AL PUNTO CORRENTE
            val currentPoint = state.currentPointsSequence(0)(state.currentPointIndex)
            if(getMyLength() == car_length) {
              publisherGuiHandler ! carPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(direction))
            }
            else if(getMyLength() == bus_length) {
              publisherGuiHandler ! busPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(direction))
            }
            else {
              // tram
              publisherGuiHandler ! tramPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(direction))
            }
            
            // manda l'update della posizione alla lane
            sendToImmovable(id, self, lane.id, envelope(id, lane.id, Advanced(currentPoint)))
            // c'è un veicolo dietro?
            if(previousVehicle != null) {
              // c'è qualcuno, manda update della posizione
              sendToMovable(id, self, previousVehicle, envelope(id, state.previousVehicleId, Advanced(currentPoint)))
            }
            // qualora sia stata raggiunta la vehicle length + 1, manda all'entità precedente un messaggio vehicle free
            // per approccio conservativo, utilizziamo la bus_length (nella tratta del tram gira un solo tram sempre)
            // attenzione: se partivamo da una zona, NON dobbiamo mandare la vehicle free
            // attenzione: se siamo usciti da un incrocio nil o angle, NON dobbiamo mandare la vehicle free
            // attenzione: se l'incrocio angle era parte di un incrocio doppio, la regola fa eccezione :)
            var nilOrAngleCrossroad = false
            if(state.fromCrossroad()) {
              val crossroad = JSONReader.getCrossroad(current_map, state.getPreviousStepId).get
              if(crossroad.category == `nil` || crossroad.category == `angle`) {
                nilOrAngleCrossroad = true
                // controlla se confinava con un'altro incrocio
                val potentialCrossroad = JSONReader.getCrossroad(current_map, state.getStepIdAt(-2))
                if(potentialCrossroad.isDefined) {
                  // significa che il nostro incrocio angle onfina con un'altro incrocio, che deve essere classic
                  // in questo caso abbiamo bisogno che venga mandata la vehiclefree
                  nilOrAngleCrossroad = false
                }
              }
            }
            if(state.fromZone() == false && nilOrAngleCrossroad == false && state.currentPointIndex == bus_length + 1) {
              var previousStepId = state.getPreviousStepId
              // recupera anche l'id della lane da cui provenivamo
              var previousLaneId : String = null
              if(JSONReader.getLane(current_map, state.getStepIdAt(-2)).isEmpty == false) {
                previousLaneId = state.getStepIdAt(-2)
              }
              else {
                // avevamo due incroci prima
                val previousCrossroad = JSONReader.getCrossroad(current_map, previousStepId).get
                val targetCrossroad = getClassicCrossroad(previousCrossroad)
                previousStepId = targetCrossroad.id
                previousLaneId = state.getStepIdAt(-3)
              }
              sendToImmovable(id, self, previousStepId, envelope(id, previousStepId, VehicleFree(previousLaneId)))
            }
            // GESTIONE PUNTO SUCCESSIVO
            // flag che ci segnala se possiamo avanzare o no
            var go = false
            // c'è un veicolo davanti?
            if(nextVehicle == null) {
              // non vi è nessuno, posso avanzare liberamente
              go = true
            }
            else {
              // c'è qualcuno, devo calibrare il mio avanzamento sulla base dei suoi spostamenti
              // vi è la possibilità che l'ultima posizione ricevuta sia ancora quella di default (-1, -1)
              // ciò significa che dobbiamo ancora ricevere una posizione valida, dunque NON avanziamo
              if(nextVehicleLastPosition.x == -1 && nextVehicleLastPosition.y == -1) {
                // approccio conservativo: non facciamo nulla, non possiamo avanzare
                go = false
              }
              else {
                // ho una lastPosition valida, devo capire se posso avanzare di 1
                assert(state.currentPointIndex < state.currentPointsSequence(0).length - 1)
                val targetPoint = state.currentPointsSequence(0)(state.currentPointIndex + 1)
                val distance = getDistance(targetPoint, nextVehicleLastPosition)
                if(distance > getMyLength()) {
                  // posso avanzare di 1
                  go = true
                }
                else {
                  // non posso avanzare
                  go = false
                }
              }
            }
            // dopo aver effettuato gli opportuni controlli, vediamo se possiamo avanzare o meno
            if(go == true) {
              if(state.currentPointIndex == state.currentPointsSequence(0).length-1) {
                // abbiamo finito la lane
                // potremmo essere nel caso in cui la prossima entità è attaccata all'incrocio e non abbiamo avuto spazio sufficiente per lanciare la vehicle_free
                // in tal caso, prima di passare allo step successivo, devo mandarla
                if(state.currentPointIndex < bus_length + 1) {
                  var previousStepId = state.getPreviousStepId
                  // recupera anche l'id della lane da cui provenivamo
                  var previousLaneId : String = null
                  if(JSONReader.getLane(current_map, state.getStepIdAt(-2)).isEmpty == false) {
                    previousLaneId = state.getStepIdAt(-2)
                  }
                  else {
                    // avevamo due incroci prima
                    val previousCrossroad = JSONReader.getCrossroad(current_map, previousStepId).get
                    val targetCrossroad = getClassicCrossroad(previousCrossroad)
                    previousStepId = targetCrossroad.id
                    previousLaneId = state.getStepIdAt(-3)
                  }
                  sendToImmovable(id, self, previousStepId, envelope(id, previousStepId, VehicleFree(previousLaneId)))
                }
                if(state.toZone() == false) {
                  if(previousVehicle != null) {
                    persist(PredecessorGoneNotSentYet) { evt => }
                    // persist body begin
                    state.predecessorGoneSent = false
                    // persist body end
                  }
                  // la lane corrente diventa la nostra previousLane
                  previousLaneId = lane.id
                }
                else {
                  // dobbiamo avvisare eventuali predecessore e successore che ce ne stiamo andando
                  if(previousVehicle != null && nextVehicle != null) {
                    // avvisa il predecessore di aspettarsi gli update dal nuovo successore
                    sendToMovable(id, self, previousVehicle, envelope(id, state.previousVehicleId, PredecessorChanged(state.nextVehicleId, nextVehicle)))
                    // avvisa il successore di mandare gli update al nuovo predecessore
                    sendToMovable(id, self, nextVehicle, envelope(id, state.nextVehicleId, SuccessorChanged(state.previousVehicleId, previousVehicle)))
                  }
                  else if(previousVehicle != null && nextVehicle == null) {
                    // avvisa il predecessore che può procedere liberamente
                    sendToMovable(id, self, previousVehicle, envelope(id, state.previousVehicleId, PredecessorGone))
                  }
                  else if(previousVehicle == null && nextVehicle != null) {
                    // avvisa il successore che non deve più mandare update a nessuno
                    sendToMovable(id, self, nextVehicle, envelope(id, state.nextVehicleId, SuccessorGone))
                  }
                  else {
                    // nessuno da avvisare
                    
                  }
                  // notifichiamo la lane che gestisca la nostra uscita di scena
                  sendToImmovable(id, self, lane.id, envelope(id, lane.id, HandleLastVehicle))
                  previousLaneId = null
                  // dal momento che stiamo andando in una zone, azzeriamo il nostro stato
                  persist(PredecessorGoneSent) { evt => }
                  // persist body begin
                  state.previousVehicleId = null
                  // persist body end
                  previousVehicle = null
                  if(getMyLength() == car_length) {
                    persist(CarEvent(NextVehicleGone)) { evt => }
                  }
                  else if (getMyLength() == bus_length) {
                    persist(BusEvent(NextVehicleGone)) { evt => }
                  }
                  else {
                    // tram
                    persist(TramEvent(NextVehicleGone)) { evt => }
                  }
                  // persist body begin
                  state.nextVehicleId = null
                  // persist body end
                  nextVehicle = null
                }
                // procedi al prossimo step
                sendToMovable(id, self, self, PersistAndNextStep)
                // fine del componente che stiamo percorrendo, spegni l'interruttore
                interestedInVelocityTick = false
              }
              else {
                persist(IncrementPointIndex) {evt => }
                // persist body begin
                state.currentPointIndex = state.currentPointIndex + 1
                // persist body end
              }
            }
          case crossroad_step(crossroad, direction) =>
            // il comportamento è molto semplice: vai avanti fino a che non hai esaurito tutti i pezzi di percorso
            // LANCIA EVENTO LEGATO AL PUNTO CORRENTE
            // dal momento che vi è una direzione diversa per ogni pezzo di percorso, la calcoliamo a partire dai primi due punti del percorso in questione
            // questo implicitamente assume che un pezzo di percorso sia composto almeno da due punti
            // ATTENZIONE: quando si ha un pedone che attraversa un incrocio che ha a che fare con tre corsie da un lato e due da un altro,
            // non è vero che i primi due punti hanno stessa coordinata x o stessa coordinata y
            // dunque, se siamo un pedone, prendiamo il secondo e il terzo
            
            // ATTENZIONE: potremmo essere nel caso in cui il primo pezzo di percorso è vuoto
            // questo succede in taluni casi con i pedoni
            // in tal caso, si fa avanzare pathPhase finche la lunghezza del percorso non è di almeno 1
            while(pathPhase < currentNonPersistentPointsSequence.length && currentNonPersistentPointsSequence(pathPhase).length < 1) {
              pathPhase = pathPhase + 1
            }
            if(pathPhase == currentNonPersistentPointsSequence.length) {
              // l'ultimo pezzo di percorso è di lunghezza 0
              // dunque persist and next step
              sendToMovable(id, self, self, PersistAndNextStep)
              // fine del componente che stiamo percorrendo, spegni l'interruttore
              interestedInVelocityTick = false
            }
            else {
              val currentPoint = currentNonPersistentPointsSequence(pathPhase)(currentNonPersistentPointIndex)
              if(getMyLength() == pedestrian_length) {
                publisherGuiHandler ! pedestrianPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(currentNonPersistentPointsSequence(pathPhase)))
              }
              else if(getMyLength() == car_length) {
                publisherGuiHandler ! carPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(currentNonPersistentPointsSequence(pathPhase)))
              }
              else if(getMyLength() == bus_length) {
                publisherGuiHandler ! busPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(currentNonPersistentPointsSequence(pathPhase)))
              }
              else {
                // tram
                publisherGuiHandler ! tramPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(currentNonPersistentPointsSequence(pathPhase)))
              }
              
              if(currentNonPersistentPointIndex == currentNonPersistentPointsSequence(pathPhase).length - 1) {
                if(pathPhase == currentNonPersistentPointsSequence.length - 1) {
                  // siamo alla fine
                  sendToMovable(id, self, self, PersistAndNextStep)
                  // fine del componente che stiamo percorrendo, spegni l'interruttore
                  interestedInVelocityTick = false
                }
                else {
                  // abbiamo ancora dei pezzi di percorso
                  pathPhase = pathPhase + 1
                  currentNonPersistentPointIndex = 0
                }
              }
              else {
                currentNonPersistentPointIndex = currentNonPersistentPointIndex + 1
              }
            }
            
          case pedestrian_crossroad_step(pedestrian_crossroad, direction) =>
            // possiamo essere un pedone o un veicolo
            // in ogni caso, possiamo procedere senza problemi fino alla fine della (prima) sequenza di punti
            // LANCIA EVENTO LEGATO AL PUNTO CORRENTE
            val currentPoint = currentNonPersistentPointsSequence(pathPhase)(currentNonPersistentPointIndex)
            if(getMyLength() == pedestrian_length) {
              publisherGuiHandler ! pedestrianPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(currentNonPersistentPointsSequence(pathPhase)))
            }
            else if(getMyLength() == car_length) {
              publisherGuiHandler ! carPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(currentNonPersistentPointsSequence(pathPhase)))
            }
            else if(getMyLength() == bus_length) {
              publisherGuiHandler ! busPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(currentNonPersistentPointsSequence(pathPhase)))
            }
            else {
              // tram
              publisherGuiHandler ! tramPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(currentNonPersistentPointsSequence(pathPhase)))
            }
            
            // esamina dove siamo nella sequenza di punti
            if(currentNonPersistentPointIndex == currentNonPersistentPointsSequence(pathPhase).length-1) {
              if(pathPhase == 0) {
                // controlliamo il numero di sequenze che abbiamo
                if(currentNonPersistentPointsSequence.length == 1) {
                  // veicolo o pedone che ignora le strisce
                  // procedi al prossimo step
                  sendToMovable(id, self, self, PersistAndNextStep)
                  // fine del componente che stiamo percorrendo, spegni l'interruttore
                  interestedInVelocityTick = false
                }
                else {
                  // siamo un pedone che è arrivato a dover attraversare effettiavmente le strisce
                  // avanza di fase
                  pathPhase = pathPhase + 1
                  // azzera l'indice
                  currentNonPersistentPointIndex = 0
                  // disabilita l'interessamento ai velocity tick
                  interestedInVelocityTick = false
                  // manifesta la volontà di attraversare
                  sendToImmovable(id, self, pedestrian_crossroad.id, envelope(id, pedestrian_crossroad.id, Cross_In))
                }
              }
              else if(pathPhase == 1) {
                // siamo arrivati alla fine dell'attraversamento vero e proprio
                // mantieni l'interessamento agli eventi di avanzamento
                // avanza di fase
                pathPhase = pathPhase + 1
                // azzera l'indice
                currentNonPersistentPointIndex = 0
                // informa le strisce
                sendToImmovable(id, self, pedestrian_crossroad.id, envelope(id, pedestrian_crossroad.id, CrossFree))
              }
              else {
                // procedi al prossimo step
                sendToMovable(id, self, self, PersistAndNextStep)
                // fine del componente che stiamo percorrendo, spegni l'interruttore
                interestedInVelocityTick = false
              }
            }
            else {
              currentNonPersistentPointIndex = currentNonPersistentPointIndex + 1
            }
            
          case bus_stop_step(bus_stop, direction, ignore) =>
            // a prescindere dall'entità che sono, devo procedere fino alla fine del percorso
            // LANCIA EVENTO LEGATO AL PUNTO CORRENTE
            val currentPoint = currentNonPersistentPointsSequence(pathPhase)(currentNonPersistentPointIndex)
            if(getMyLength() == pedestrian_length) {
              publisherGuiHandler ! pedestrianPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(currentNonPersistentPointsSequence(pathPhase)))
            }
            else if(getMyLength() == car_length) {
              publisherGuiHandler ! carPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(currentNonPersistentPointsSequence(pathPhase)))
            }
            else if(getMyLength() == bus_length) {
              publisherGuiHandler ! busPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(currentNonPersistentPointsSequence(pathPhase)))
            }
            else {
              // tram
              publisherGuiHandler ! tramPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(currentNonPersistentPointsSequence(pathPhase)))
            }
            
            if(currentNonPersistentPointIndex == currentNonPersistentPointsSequence(pathPhase).length-1) {
              // a prescindere da chi siamo, spegni l'interruttore
              interestedInVelocityTick = false
              if(ignore == true) {
                // siamo arrivati alla fine del percorso
                // procedi al prossimo step
                sendToMovable(id, self, self, PersistAndNextStep)
              }
              else {
                if(getMyLength() == pedestrian_length) {
                  // siamo un pedone che vuole prendere il bus, oppure che ha finito la seconda bus stop
                  if(state.fromBusStop() == true) {
                    // procedi al prossimo step
                    sendToMovable(id, self, self, PersistAndNextStep)
                  }
                  else {
                    // recupera l'id della fermata del bus a cui vuole scendere
                    val nextStop = state.getNextStepId
                    assert(nextStop.charAt(0) == 'B')
                    // passa al prossimo step senza IpRequest
                    persist(NextStepEvent) { evt => }
                    // persist body begin
                    state.index = state.index + 1
                    if(state.index >= state.currentRoute.length) {
                      state.handleIndexOverrun
                    }
                    // preoccupati anche del flag di inizio step
                    state.beginOfTheStep = true
                    // persist body end
                    // ora accodati
                    sendToImmovable(id, self, bus_stop.id, envelope(id, bus_stop.id, WaitForPublicTransport(nextStop)))
                    // ammazzati
                    shutdown()
                  }
                }
                else {
                  // siamo un bus che deve far salire e scendere i pedoni, oppure che ha terminato il suo percorso
                  if(pathPhase == 0) {
                    // seleziona chi deve scendere
                    var goingOff = List[String]()
                    for(entry <- state.travellers) {
                      if(entry._2 == bus_stop.id) {
                        goingOff = goingOff :+ entry._1
                      }
                    }
                    // rendi persistente la rimozione
                    persist(BusEvent(TravellersGoneOff(goingOff))) { evt => }
                    // persist body begin
                    for(traveller <- goingOff) {
                      state.travellers = state.travellers - traveller
                    }
                    // persist body end
                    // invia i viaggiatori scesi e il numero corrente di passeggeri alla bus stop
                    // ATTENZIONE: fault tolerance non garantita
                    // in caso di crash, gli identificativi potrebbero essere ora irrimediabilmente persi
                    sendToImmovable(id, self, bus_stop.id, envelope(id, bus_stop.id, GetOut(goingOff, state.travellers.size)))
                  }
                  else {
                    // abbiamo terminato il percorso
                    // procedi al prossimo step
                    sendToMovable(id, self, self, PersistAndNextStep)
                  }
                  
                }
              }
            }
            else {
              currentNonPersistentPointIndex = currentNonPersistentPointIndex + 1
            }
          case tram_stop_step(tram_stop, direction, ignore) =>
            // a prescindere dall'entità che sono, devo procedere fino alla fine del percorso
            // LANCIA EVENTO LEGATO AL PUNTO CORRENTE
            val currentPoint = currentNonPersistentPointsSequence(pathPhase)(currentNonPersistentPointIndex)
            if(getMyLength() == pedestrian_length) {
              publisherGuiHandler ! pedestrianPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(currentNonPersistentPointsSequence(pathPhase)))
            }
            else if(getMyLength() == car_length) {
              publisherGuiHandler ! carPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(currentNonPersistentPointsSequence(pathPhase)))
            }
            else if(getMyLength() == bus_length) {
              publisherGuiHandler ! busPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(currentNonPersistentPointsSequence(pathPhase)))
            }
            else {
              // tram
              publisherGuiHandler ! tramPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(currentNonPersistentPointsSequence(pathPhase)))
            }
            
            if(currentNonPersistentPointIndex == currentNonPersistentPointsSequence(pathPhase).length-1) {
              // a prescindere da chi siamo, spegni l'interruttore
              interestedInVelocityTick = false
              if(ignore == true) {
                // siamo arrivati alla fine del percorso
                // procedi al prossimo step
                sendToMovable(id, self, self, PersistAndNextStep)
              }
              else {
                if(getMyLength() == pedestrian_length) {
                  // siamo un pedone che vuole prendere il tram, oppure che ha finito la seconda tram stop
                  if(state.fromTramStop() == true) {
                    // procedi al prossimo step
                    sendToMovable(id, self, self, PersistAndNextStep)
                  }
                  else {
                    // recupera l'id della fermata del tram a cui vuole scendere
                    val nextStop = state.getNextStepId
                    assert(nextStop.charAt(0) == 'T')
                    // passa al prossimo step senza IpRequest
                    persist(NextStepEvent) { evt => }
                    // persist body begin
                    state.index = state.index + 1
                    if(state.index >= state.currentRoute.length) {
                      state.handleIndexOverrun
                    }
                    // preoccupati anche del flag di inizio step
                    state.beginOfTheStep = true
                    // persist body end
                    // ora accodati
                    sendToImmovable(id, self, tram_stop.id, envelope(id, tram_stop.id, WaitForPublicTransport(nextStop)))
                    // ammazzati
                    shutdown()
                  }
                }
                else {
                  // siamo un tram che deve far salire e scendere i pedoni, oppure che ha terminato il suo percorso
                  if(pathPhase == 0) {
                    // seleziona chi deve scendere
                    var goingOff = List[String]()
                    for(entry <- state.travellers) {
                      if(entry._2 == tram_stop.id) {
                        goingOff = goingOff :+ entry._1
                      }
                    }
                    // rendi persistente la rimozione
                    persist(TramEvent(TravellersGoneOff(goingOff))) { evt => }
                    // persist body begin
                    for(traveller <- goingOff) {
                      state.travellers = state.travellers - traveller
                    }
                    // persist body end
                    // invia i viaggiatori scesi e il numero corrente di passeggeri alla tram stop
                    // ATTENZIONE: fault tolerance non garantita
                    // in caso di crash, gli identificativi potrebbero essere ora irrimediabilmente persi
                    sendToImmovable(id, self, tram_stop.id, envelope(id, tram_stop.id, GetOut(goingOff, state.travellers.size)))
                  }
                  else {
                    // abbiamo terminato il percorso
                    // procedi al prossimo step
                    sendToMovable(id, self, self, PersistAndNextStep)
                  }
                  
                }
              }
            }
            else {
              currentNonPersistentPointIndex = currentNonPersistentPointIndex + 1
            }
          case zone_step(zone, direction) =>
            println("We should not be here!")
        }
      }
  }
  
  override def receiveRecover: Receive = {
    
    case evt : Event => evt match {
      case NoDuplicate(senderId, deliveryId) =>
        state.updateFilter(senderId, deliveryId)
      case NextStepEvent =>
        state.index = state.index + 1
        if(state.index >= state.currentRoute.length) {
          state.handleIndexOverrun
        }
        // gestisci anche il flag di inzio step
        state.beginOfTheStep = true
      case RouteArrived(route) =>
        state.handleRoute(route)
      // VELOCITY
      case BeginOfTheStep(currentPointsSequence) =>
        state.currentPointsSequence = currentPointsSequence
        state.currentPointIndex = 0
        state.beginOfTheStep = false
      case IncrementPointIndex =>
        state.currentPointIndex = state.currentPointIndex + 1
      // LANE
      case PredecessorArrived(id) =>
        state.previousVehicleId = id
      case PredecessorGoneNotSentYet =>
        state.predecessorGoneSent = false
      case PredecessorGoneSent =>
        state.previousVehicleId = null
        state.predecessorGoneSent = true
        
      // AT LEAST ONCE
      case PersistDeliveryId(deliveryId) =>
        state.deliveryId = deliveryId
        
      case PedestrianEvent(event) =>
        Pedestrian.eventHandler(event, state)
      case CarEvent(event) =>
        Car.eventHandler(event, state)
      case BusEvent(event) =>
        Bus.eventHandler(event, state)
      case TramEvent(event) =>
        Tram.eventHandler(event, state)
    }
    
    case RecoveryFailure(cause) =>
      println("Recovery fallita!")
    
    case SnapshotOffer(metadata, offeredSnapshot) =>
      offeredSnapshot match {
        case Some(snapshot : MovableState) =>
          state = snapshot
          //setDeliverySnapshot(state.deliveryState)
          println("State recovered from snapshot successfully")
      }
      
    // TIME
    case TimeEvent(timeValue) =>
      state.currentTime = timeValue
      
  }
  
  // UTILITY
  // Testa se il richiedente è locale o remoto
  def isLocal(requesterIp : String) : Boolean = {
    val responderIp = context.system.settings.config.getString("akka.remote.netty.tcp.hostname")
    return (requesterIp == responderIp)
  }
  
  // UTILITY
  // Funzione che permette l'invio di un messaggio ad un attore immobile, effettuando l'enveloping adeguato
  def sendToImmovable(senderId : String, senderRef : ActorRef, destinationId : String, command : Command) : Unit = {
    deliver(shardRegion.path, deliveryId => {
      if(deliveryId >= state.deliveryId) {
        persist(PersistDeliveryId(deliveryId)) { evt => }
        state.deliveryId = deliveryId
      }
      else {
        // potremmo essere dopo un ripristino
        persist(PersistDeliveryId(state.deliveryId + deliveryId)) { evt => }
        state.deliveryId = state.deliveryId + deliveryId
      }
      ToImmovable(destinationId, ToPersistentMessages.FromMovable(senderId, senderRef, Request(state.deliveryId, command)))
    })
  }
  
  // UTILITY
  // Funzione che permette l'invio di un messaggio ad un attore mobile, effettuando l'enveloping adeguato
  def sendToMovable(senderId : String, senderRef : ActorRef, destinationRef : ActorRef, command : Command) : Unit = {
    deliver(destinationRef.path, deliveryId => {
      if(deliveryId >= state.deliveryId) {
        persist(PersistDeliveryId(deliveryId)) { evt => }
        state.deliveryId = deliveryId
      }
      else {
        // potremmo essere dopo un ripristino
        persist(PersistDeliveryId(state.deliveryId + deliveryId)) { evt => }
        state.deliveryId = state.deliveryId + deliveryId
      }
      ToMovable(destinationRef, ToPersistentMessages.FromMovable(senderId, senderRef, Request(state.deliveryId, command)))
    })
  }
  
  // UTILITY
  // Funzione che permette l'invio di un messaggio ad un attore non persistente, effettuando l'enveloping adeguato
  def sendToNonPersistent(senderId : String, destinationRef : ActorRef, command : Command) : Unit = {
    destinationRef ! ToNonPersistent(destinationRef, ToNonPersistentMessages.FromImmovable(senderId, command))
  }
  
  // UTILITY
  // Racchiude le cose da fare al fine di chiudersi propriamente
  def shutdown() : Unit = {
    // disiscriviti dai messaggi temporali generali
    mediator ! Unsubscribe(timeMessage, self)
    // smetti di mandarti messaggi per la regolazione della velocità
    velocityTimer.cancel()
    // smetti di mandarti messaggi per l'esecuzione di snapshot
    snapshotTimer.cancel()
    // ammazzati
    self ! Shutdown
  }
  
  // UTILITY
  // Recupera la lunghezza dell'entità rappresentata
  def getMyLength() : Int = {
    getLengthFromId(id)
  }
  
}