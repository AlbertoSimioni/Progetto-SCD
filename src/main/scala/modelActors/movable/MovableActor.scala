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
import map.Domain.position._
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
  
  // TIME
  // Utilizzato per gestire gli avanzamenti negli spostamenti
  case object VelocityTick
  
  // SHUTDOWN
  // La gestione della poison pill è ambigua, utilizzo un messaggio dedicato:
  // http://getakka.net/docs/persistence/persistent-actors
  case object Shutdown
  
  // DEFER
  // serve per garantire che i persistAsync siano stati tutti effettuati prima di procedere
  // case object DeferObject


}

class MovableActor(id : String) extends PersistentActor with AtLeastOnceDelivery {
  
  import context.dispatcher
  
  import MovableActor._
  
  // GUI
  // riferimento per la pubblicazione di eventi grafici
  val publisherGuiHandler = PublisherInstance.getPublisherModelEvents(context.system)
  
  // PERSISTENCE
  override def persistenceId: String = "MovableActor-" + id

  // SHARDING
  // Permette di comunicare con altri ImmovableActor utilizzando il loro identificativo invece che il loro indirizzo
  val shardRegion = ClusterSharding(context.system).shardRegion(ImmovableActor.typeOfEntries)

  // PERSISTENCE
  // Permette di effettuare il salvataggio dello snapshot ogni 10 secondi
  // val snapshotTimer = context.system.scheduler.schedule(60 seconds, 10 seconds, self, SaveSnapshot)
  

  
  // TIME
  // Recupera l'attore che gli permette di ricevere gli eventi temporali e si sottoscrive immediatamente
  val mediator = DistributedPubSubExtension(context.system).mediator
  mediator ! Subscribe(timeMessage, self)
  
  // TIME
  // Tick utilizzato solo negli avanzamenti
  // val velocityTimer = context.system.scheduler.schedule(0 millis, Duration(getVelocityTickInterval(id), "millis"), self, VelocityTick)
  
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
  
  // variabile che modella dove siamo arrivati nel percorso
  var pathPhase = 0
  
  // NON-PERSISTENT DATA
  // lista di punti e indice di avanzamento per gli step che non sono da rendere persistenti
  var currentNonPersistentPointsSequence : List[List[point]] = null
  var currentNonPersistentPointIndex : Int = 0
  
  // RIFERIMENTI REMOTI
  // la variabile permette di chiedere alla lane precedente se il nostro riferimento precedente alla nostra ricreazione è stato dato a qualcuno
  // in tal caso, la lane ci manda indietro l'identità di chi ha ricevuto il nostro vecchio riferimento
  // la variabile è di default a false, il che significa che viene eseguita solo la prima volta ad una ricreazione dell'attore mobile
  var predecessorRetrieved = false
  
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
            // persistAsync(NoDuplicate(senderId, deliveryId)) { msg => }
            // persist body begin
            state.updateFilter(senderId, deliveryId)
            // persist body end
            // gestione vera e propria del messaggio
            printMessage(senderId, id, command)
            command match {
              case IpResponse(ipAddress) =>
                if(isLocal(ipAddress)) {
                  val previousHandler = state.getPreviousStepId
                  sendToImmovable(id, self, previousHandler, MobileEntityRemove(id))
                  // effettua il cambio gestione
                  sendToImmovable(id, self, senderId, MobileEntityAdd(id))
                  // esegui lo step corrente
                  sendToMovable(id, self, self, ExecuteCurrentStep)
                }
                else {
                  // informa il precedente gestore
                  val previousHandler = state.getPreviousStepId
                  sendToImmovable(id, self, previousHandler, MobileEntityRemove(id))
                  // trasferimento di nodo di calcolo, con cambio di gestione
                  sendToImmovable(id, self, senderId, MobileEntityAdd(id))
                  // prima di dare il comando di ricreazione al destinatario, assicuriamoci di aver completato tutti i persistAsync
                  sendToImmovable(id, self, senderId, ReCreateMe(id, state.getSnapshot()))
                  // ammazzati
                  shutdown()
                  /*
                  defer(DeferObject) { evt =>
                    sendToImmovable(id, self, senderId, ReCreateMe(id, state.getSnapshot()))
                    // ammazzati
                    shutdown()
                  }
                  */
                }
              case Route(route) =>
                // è arrivato il percorso
                // persistAsync(RouteArrived(route)) { evt => }
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
              case PreviousVehicleResponse(id, ref) =>
                // abbiamo ricevuto la risposta, impostiamo il flag
                predecessorRetrieved = true
                // impostiamo i dati
                // myRef.persistAsync(PreviousVehicleIdArrived(senderId)) { evt => }
                // persist body begin
                state.previousVehicleId = id
                // persist body end
                previousVehicle = ref
                // fai ripartire l'esecuzione
                sendToMovable(this.id, self, self, ExecuteCurrentStep)
              case MovableStateSnapshotOffer(snapshot) =>
                state.setSnapshot(snapshot)
                
              case ToPedestrian(command) =>
                Pedestrian.fromImmovableHandler(this, id, senderId, command)
              case ToCar(command) =>
                Car.fromImmovableHandler(this, id, senderId, command)
              case ToBus(command) =>
                Bus.fromImmovableHandler(this, id, senderId, command)
              case ToTram(command) =>
                Tram.fromImmovableHandler(this, id, senderId, command)
              case _ =>
                println("ERRORE: comando ricevuto da entità immobile non previsto")
                println("Comando: " + command)
                assert(false)
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
          val actorPath = senderRef.path.toSerializationFormat
          if(state.isNewMessage(actorPath, deliveryId) == true) {
            // messaggio nuovo
            // persistAsync(NoDuplicate(actorPath, deliveryId)) { msg => }
            // persist body begin
            state.updateFilter(actorPath, deliveryId)
            // persist body end
            // handling vero e proprio del messaggio
            printMessage(senderId, id, command)
            command match {
              case ExecuteCurrentStep =>
                val stepSequence = state.getStepSequence()
                state.currentRoute(state.index) match {
                  case road_step(road, direction) =>
                    // solo un pedone può avere road_step
                    if(state.beginOfTheStep) {
                      val currentPointsSequence = getPointsSequence(id, stepSequence)
                      // persistAsync(BeginOfTheStep(currentPointsSequence)) { evt => }
                      // persist body begin
                      state.currentPointsSequence = currentPointsSequence
                      state.currentPointIndex = 0
                      state.previousPointIndex = -1
                      state.beginOfTheStep = false
                      // persist body end
                    }
                    // attiva l'interessamento agli eventi di avanzamento
                    interestedInVelocityTick = true
                    // sendToMovable(id, self, self, VelocityTick)
                    self ! VelocityTick
                  case lane_step(lane, direction) =>
                    // sto approciando una lane
                    // potrei partire da una zona e non aver cominciato il percorso
                    if(state.fromZone() == true && state.beginOfTheStep == true) {
                      // in tal caso, fai la richiesta alla lane
                      val firstPoint = getPointsSequence(id, stepSequence)(0)(0)
                      sendToImmovable(id, self, lane.id, envelope(id, lane.id, LaneAccessRequest(firstPoint, direction)))
                    }
                    else {
                      // potremmo essere nel caso in cui non siamo riusciti a mandare PredecessorGone prima del crash/cambio del nodo in cui siamo
                      // la situazione è segnalata dal flag booleano predecessorGoneSent
                      // in questo caso, bisogna recuperare il riferimento al veicolo precedente prima di procedere
                      if(previousVehicle == null && state.previousVehicleId != null && state.predecessorGoneSent == false) {
                        // l'unico caso in cui possiamo essere è che non siamo riusciti ad inviare PredecessorGone prima di morire
                        // recupera id della lane precedente
                        var previousLaneId : String = null
                        if(state.fromDoubleCrossroad() == false) {
                          previousLaneId = state.getStepIdAt(-2)
                        }
                        else {
                          // avevamo due incroci prima
                          previousLaneId = state.getStepIdAt(-3)
                        }
                        // manda un messaggio di richiesta del veicolo in questione
                        sendToImmovable(id, self, previousLaneId, MovableActorRequest(state.previousVehicleId))
                      }
                      // potremmo essere nel caso in cui il nostro riferimento vecchio è stato fornito a qualcuno, che sta cercando di utilizzarlo invano
                      // in tal caso, recuperiamo dalla lane precedente il riferimento a questo qualcuno, in modo tale da avvisarlo poi 
                      else if(state.previousVehicleId == null && predecessorRetrieved == false) {
                        // recupera id della lane precedente
                        var previousLaneId : String = null
                        if(state.fromDoubleCrossroad() == false) {
                          previousLaneId = state.getStepIdAt(-2)
                        }
                        else {
                          // avevamo due incroci prima
                          previousLaneId = state.getStepIdAt(-3)
                        }
                        // manda un messaggio di richiesta del veicolo in questione
                        sendToImmovable(id, self, previousLaneId, PreviousVehicleRequest)
                      }
                      else {
                        // per prima cosa, qualora vi fosse un veicolo predecessore, informalo che non deve più seguirci
                        if(previousVehicle != null) {
                          if(state.previousLaneId != null) {
                            sendToMovable(id, self, previousVehicle, envelope(id, state.previousVehicleId, PredecessorGone(state.previousLaneId)))
                          }
                          else {
                            // recupera id della lane precedente
                            var previousLaneId : String = null
                            if(state.fromDoubleCrossroad() == false) {
                              previousLaneId = state.getStepIdAt(-2)
                            }
                            else {
                              // avevamo due incroci prima
                              previousLaneId = state.getStepIdAt(-3)
                            }
                            sendToMovable(id, self, previousVehicle, envelope(id, state.previousVehicleId, PredecessorGone(previousLaneId)))
                          }
                          // persistAsync(PredecessorGoneSent) { evt => }
                          // persist body begin
                          state.previousVehicleId = null
                          state.predecessorGoneSent = true
                          // persist body end
                          previousVehicle = null
                        }
                        // se c'è una qualche previousLane, avvisala di modificare i campi lastVehicle (qualora fossimo stati l'unico veicolo) e di cancellare la nostra posizione
                        if(state.previousLaneId != null) {
                          sendToImmovable(id, self, state.previousLaneId, envelope(id, state.previousLaneId, RemovePosition))
                          sendToImmovable(id, self, state.previousLaneId, envelope(id, state.previousLaneId, HandleLastVehicle(null, null)))
                          // persistAsync(PreviousLaneChanged(null)) { evt => }
                          // persist body begin
                          state.previousLaneId = null
                          // persist body end
                        }
                        if(state.beginOfTheStep == true) {
                          // dobbiamo ancora iniziare
                          // avvisa l'entità precedente (incrocio/ strisce pedonali, etc.) che sei ufficialmente sulla nuova lane
                          // in questo modo, l'entità in questione rende persistente l'occupazione della lane considerata.
                          var previousId = state.getPreviousStepId
                          // recupera anche l'id della lane da cui provenivi
                          var previousLaneId : String = null
                          if(state.fromDoubleCrossroad() == false) {
                            previousLaneId = state.getStepIdAt(-2)
                          }
                          else {
                            // avevamo due incroci prima
                            val previousCrossroad = JSONReader.getCrossroad(current_map, previousId).get
                            val targetCrossroad = getClassicCrossroad(previousCrossroad)
                            previousId = targetCrossroad.id
                            previousLaneId = state.getStepIdAt(-3)
                          }
                          sendToImmovable(id, self, previousId, envelope(id, previousId, VehicleBusy(previousLaneId, lane.id)))
                          // manda un messaggio alla lane corrente per ricevere il ref
                          sendToImmovable(id, self, lane.id, envelope(id, lane.id, NextVehicleFirstRequest))
                        }
                        else {
                          // sicuramente siamo in ripristino
                          // recuperiamo il ref
                          // controlliamo se sono l'ultimo della lane o no
                          var flag = false
                          if(state.previousVehicleId == null/* || state.predecessorGoneSent == false*/) {
                            flag = true
                          }
                          sendToImmovable(id, self, lane.id, envelope(id, lane.id, NextVehicleRequest(state.nextVehicleId, flag)))
                        }
                      }
                    }
                    
                  case crossroad_step(crossroad, direction) =>
                    if(state.fromCrossroad()) {
                      // siamo appena stati su un altro incrocio
                      // il percorso e la logica calcolati in precedenza ci hanno già portati al termine del secondo incrocio
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
                        // sendToMovable(id, self, self, VelocityTick)
                        self ! VelocityTick
                      }
                      else {
                        if(state.toCrossroad()) {
                          // siamo un veicolo e stiamo affrontando un incrocio doppio
                          // vogli fare la richiesta all'incrocio classic
                          val target = getClassicCrossroad(crossroad)
                          // recupera l'id della lane precedente
                          val previousId = state.getPreviousStepId
                          // recupera l'id della lane successiva
                          val nextId = state.getStepIdAt(2)
                          // richiesta all'incrocio
                          sendToImmovable(id, self, target.id, envelope(id, target.id, Vehicle_In(previousId, nextId)))
                        }
                        else {
                          // recupera l'id della lane precedente
                          val previousId = state.getPreviousStepId
                          // recuper l'id della lane successiva
                          val nextId = state.getNextStepId
                          // richiesta all'incrocio
                          sendToImmovable(id, self, crossroad.id, envelope(id, crossroad.id, Vehicle_In(previousId, nextId)))
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
                      // sendToMovable(id, self, self, VelocityTick)
                      self ! VelocityTick
                    }
                    else {
                      // possiamo essere un pedone che ignora o un veicolo
                      // test balordo per capire chi siamo
                      if(getMyLength() == pedestrian_length) {
                        // attiva semplicemente l'interesse per i velocity tick
                        interestedInVelocityTick = true
                        // sendToMovable(id, self, self, VelocityTick)
                        self ! VelocityTick
                      }
                      else {
                        // recupera l'id della lane precedente
                        val previousId = state.getPreviousStepId
                        // recupera l'id della lane successiva
                        val nextId = state.getNextStepId
                        // richiesta alle strisce
                        sendToImmovable(id, self, pedestrian_crossroad.id, envelope(id, pedestrian_crossroad.id, Vehicle_In(previousId, nextId)))
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
                      // sendToMovable(id, self, self, VelocityTick)
                      self ! VelocityTick
                    }
                    else {
                      // recupera l'id della lane precedente
                      val previousId = state.getPreviousStepId
                      // recupera l'id della lane successiva
                      val nextId = state.getNextStepId
                      // richiesta alle fermata del bus
                      sendToImmovable(id, self, bus_stop.id, envelope(id, bus_stop.id, Vehicle_In(previousId, nextId)))
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
                      // sendToMovable(id, self, self, VelocityTick)
                      self ! VelocityTick
                    }
                    else {
                      // recupera l'id della lane precedente
                      val previousId = state.getPreviousStepId
                      // recupera l'id della lane successiva
                      val nextId = state.getNextStepId
                      // richiesta alle fermata del bus
                      sendToImmovable(id, self, tram_stop.id, envelope(id, tram_stop.id, Vehicle_In(previousId, nextId)))
                    }
                  case zone_step(zone, direction) =>
                    // PRECONDIZIONE: solo un pedone o un veicolo possono avere uno zone_step nel loro percorso
                    // siamo dunque autorizzati ad attuare la logica di dormi/veglia
                    //
                    // per quanto riguarda l'interfaccia grafica, noi siamo scomparsi, a prescindere se siamo in ritardo o in anticipo
                    // lancia evento per l'interfaccia grafica
                    if(state.alreadyHidden == false) {
                      if(getMyLength() == pedestrian_length) {
                        publisherGuiHandler ! hidePedestrian(id, zone.id, false)
                      }
                      else {
                        assert(getMyLength() == car_length)
                        publisherGuiHandler ! hideCar(id, zone.id)
                      }
                      // persistAsync(SleepingStatusChange(true)) { evt => }
                      // persist body begin
                      state.alreadyHidden = true
                      // persist body end
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
                      //println(id + ": sono in ritardo")
                      sendToMovable(id, self, self, PersistAndNextStep)
                      // evento grafico associato
                      if(state.alreadyHidden == true) {
                        publisherGuiHandler ! entityAwaked(id, zone.id)
                        // persistAsync(SleepingStatusChange(false)) { evt => }
                        // persist body begin
                        state.alreadyHidden = false
                        // persist body end
                      }
                    }
                    else {
                      // se in anticipo, vai a dormire
                      // assicuriamoci di aver completato le persistAsync prima di procedere
                      //println(id + ": sono in anticipo")
                      //println(id + ": e ora me ne vado a dormire")
                      val immovableActorId = state.getCurrentStepId
                      sendToImmovable(id, self, immovableActorId, PauseExecution(comparedTime, state.getSnapshot()))
                      shutdown()
                      /*
                      defer(DeferObject) { evt =>
                        println(id + ": e ora me ne vado a dormire")
                        val immovableActorId = state.getCurrentStepId
                        sendToImmovable(id, self, immovableActorId, PauseExecution(comparedTime, state.getSnapshot()))
                        shutdown()
                      }
                      */
                    }
                    
                }
              case PersistAndNextStep =>
                // memorizza
                // persistAsync(NextStepEvent) { evt => }
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
                println("ERRORE: comando ricevuto da entità mobile non previsto")
                println("Comando: " + command)
                assert(false)
            }
          }
        case Ack(deliveryId) =>
          // ack ricevuto, messaggio è stato consegnato con successo
          val ackAboutNewMessage = confirmDelivery(deliveryId)
      }
      case ToPersistentMessages.FromNonPersistent(senderRef, command) =>
        //
    }
    
    // VELOCITY
    case VelocityTick =>
      if(interestedInVelocityTick) {
        val timestamp = java.lang.System.currentTimeMillis()
        state.currentRoute(state.index) match {
          case road_step(road, direction) =>
            if(state.previousPointIndex != state.currentPointIndex) {
              state.previousPointIndex = state.currentPointIndex
              // LANCIA EVENTO LEGATO AL PUNTO CORRENTE
              val currentPoint = state.currentPointsSequence(0)(state.currentPointIndex)
              publisherGuiHandler ! pedestrianPosition(id, currentPoint.x, currentPoint.y, getGuiDirection(direction))
            }
            
            // esamina dove siamo nella sequenza di punti
            if(state.currentPointIndex == state.currentPointsSequence(0).length-1) {
              // procedi al prossimo step
              sendToMovable(id, self, self, PersistAndNextStep)
              // fine del componente che stiamo percorrendo, spegni l'interruttore
              interestedInVelocityTick = false
            }
            else {
              // persistAsync(IncrementPointIndex) {evt => }
              // persist body begin
              state.currentPointIndex = state.currentPointIndex + 1 
              // persist body end
            }
          case lane_step(lane, direction) =>
            if(state.previousPointIndex != state.currentPointIndex) {
              state.previousPointIndex = state.currentPointIndex
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
              sendToImmovable(id, self, lane.id, envelope(id, lane.id, Advanced(lane.id, currentPoint)))
              // c'è un veicolo dietro?
              if(previousVehicle != null) {
                // c'è qualcuno, manda update della posizione
                sendToMovable(id, self, previousVehicle, envelope(id, state.previousVehicleId, Advanced(lane.id, currentPoint)))
              }
              // qualora sia stata raggiunta la vehicle length + 1, manda all'entità precedente un messaggio vehicle free
              // per approccio conservativo, utilizziamo la bus_length (nella tratta del tram gira un solo tram sempre)
              // attenzione: se partivamo da una zona, NON dobbiamo mandare la vehicle free
              // attenzione: se siamo usciti da un incrocio nil o angle, dobbiamo mandare la vehicle free come in tutti gli altri casi
              // attenzione: se l'incrocio angle era parte di un incrocio doppio, anche
              if(state.fromZone() == false && state.currentPointIndex == bus_length + 1) {
                var previousStepId = state.getPreviousStepId
                // recupera anche l'id della lane da cui provenivamo
                var previousLaneId : String = null
                if(state.fromDoubleCrossroad() == false) {
                  previousLaneId = state.getStepIdAt(-2)
                }
                else {
                  // avevamo due incroci prima
                  val previousCrossroad = JSONReader.getCrossroad(current_map, previousStepId).get
                  val targetCrossroad = getClassicCrossroad(previousCrossroad)
                  previousStepId = targetCrossroad.id
                  previousLaneId = state.getStepIdAt(-3)
                }
                sendToImmovable(id, self, previousStepId, envelope(id, previousStepId, VehicleFree(previousLaneId, lane.id)))
              }
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
                // ATTENZIONE: potrebbe essere che il mio percorso termini su di una zona, ma che l'advanced sia ricevuto da più avanti nella lane
                // non è dunque vero che la fine del mio percorso implica l'aver ricevuto un predecessor gone (cioè il non avere più nessuno davanti)
                // questo ha senso solo in una lane dove nessuno si ferma su di una zona
                // non è dunque nemmeno vero che il ricevere una lastposition valida significhi avere ancora del percorso da fare
                if(state.currentPointIndex < state.currentPointsSequence(0).length - 1) {
                  // vi è ancora da percorrere, dunque vediamo se possiamo avanzare di 1
                  val targetPoint = state.currentPointsSequence(0)(state.currentPointIndex + 1)
                  val distance = getDistance(targetPoint, nextVehicleLastPosition)
                  // si tenga presente che il controllo da fare dipende dalla direzione in cui stiamo andando
                  direction.position match {
                    case `up` =>
                      if(direction.beginToEnd == true) {
                        println("ERRORE: direzione di percorrenza della lane contraria al senso di marcia")
                        println("Direzione: " + direction)
                        assert(false)
                      }
                      else {
                        // controllo con lunghezza del nextvehicle
                        if(distance > getLengthFromId(state.nextVehicleId)) {
                          // posso avanzare di 1
                          go = true
                        }
                        else {
                          // non posso avanzare
                          go = false
                        }
                      }
                    case `down` =>
                      if(direction.beginToEnd == true) {
                        // controllo con lunghezza del nostro veicolo
                        if(distance > getMyLength()) {
                          // posso avanzare di 1
                          go = true
                        }
                        else {
                          // non posso avanzare
                          go = false
                        }
                      }
                      else {
                        println("ERRORE: direzione di percorrenza della lane contraria al senso di marcia")
                        println("Direzione: " + direction)
                        assert(false)
                      }
                    case `left` =>
                      if(direction.beginToEnd == true) {
                        println("ERRORE: direzione di percorrenza della lane contraria al senso di marcia")
                        println("Direzione: " + direction)
                        assert(false)
                      }
                      else {
                        // controllo con lunghezza del nextvehicle
                        if(distance > getLengthFromId(state.nextVehicleId)) {
                          // posso avanzare di 1
                          go = true
                        }
                        else {
                          // non posso avanzare
                          go = false
                        }
                      }
                    case `right` =>
                      if(direction.beginToEnd == true) {
                        // controllo con lunghezza del nostro veicolo
                        if(distance > getMyLength()) {
                          // posso avanzare di 1
                          go = true
                        }
                        else {
                          // non posso avanzare
                          go = false
                        }
                      }
                      else {
                        println("ERRORE: direzione di percorrenza della lane contraria al senso di marcia")
                        println("Direzione: " + direction)
                        assert(false)
                      }
                  }
                }
                else {
                  // abbiamo finito il percorso e abbiamo una lastposition valida
                  assert(state.currentPointIndex == state.currentPointsSequence(0).length - 1)
                  // in tal caso, ci è sufficiente impostare il flag a true
                  go = true
                }
              }
            }
            // dopo aver effettuato gli opportuni controlli, vediamo se possiamo avanzare o meno
            if(go == true) {
              if(state.currentPointIndex == state.currentPointsSequence(0).length-1) {
                // abbiamo finito la lane
                // potremmo essere nel caso in cui la prossima entità è attaccata all'incrocio e non abbiamo avuto spazio sufficiente per lanciare la vehicle_free
                // in tal caso, prima di passare allo step successivo, devo mandarla
                if(state.currentPointIndex < bus_length + 1 && state.fromZone() == false) {
                  var previousStepId = state.getPreviousStepId
                  // recupera anche l'id della lane da cui provenivamo
                  var previousLaneId : String = null
                  if(state.fromDoubleCrossroad() == false) {
                    previousLaneId = state.getStepIdAt(-2)
                  }
                  else {
                    // avevamo due incroci prima
                    val previousCrossroad = JSONReader.getCrossroad(current_map, previousStepId).get
                    val targetCrossroad = getClassicCrossroad(previousCrossroad)
                    previousStepId = targetCrossroad.id
                    previousLaneId = state.getStepIdAt(-3)
                  }
                  sendToImmovable(id, self, previousStepId, envelope(id, previousStepId, VehicleFree(previousLaneId, lane.id)))
                }
                // se non stiamo andando in una zona
                if(state.toZone() == false) {
                  // la lane corrente diventa la nostra previousLane
                  // persistAsync(PreviousLaneChanged(lane.id)) { evt => }
                  // persist body begin
                  state.previousLaneId = lane.id
                  // persist body end
                }
                else {
                  // dobbiamo avvisare eventuali predecessore e successore che ce ne stiamo andando
                  if(previousVehicle != null && nextVehicle != null) {
                    // avvisa il predecessore di aspettarsi gli update dal nuovo successore
                    sendToMovable(id, self, previousVehicle, envelope(id, state.previousVehicleId, PredecessorChanged(lane.id, state.nextVehicleId, nextVehicle)))
                    // avvisa il successore di mandare gli update al nuovo predecessore
                    sendToMovable(id, self, nextVehicle, envelope(id, state.nextVehicleId, SuccessorChanged(lane.id, state.previousVehicleId, previousVehicle)))
                  }
                  else if(previousVehicle != null && nextVehicle == null) {
                    // avvisa il predecessore che può procedere liberamente
                    sendToMovable(id, self, previousVehicle, envelope(id, state.previousVehicleId, PredecessorGone(lane.id)))
                  }
                  else if(previousVehicle == null && nextVehicle != null) {
                    // avvisa il successore che non deve più mandare update a nessuno
                    sendToMovable(id, self, nextVehicle, envelope(id, state.nextVehicleId, SuccessorGone(lane.id)))
                  }
                  else {
                    // nessuno da avvisare
                    
                  }
                  // notifichiamo la lane che gestisca la nostra uscita di scena
                  if(nextVehicle != null) {
                    sendToImmovable(id, self, lane.id, envelope(id, lane.id, HandleLastVehicle(state.nextVehicleId, nextVehicle)))
                  }
                  else {
                    sendToImmovable(id, self, lane.id, envelope(id, lane.id, HandleLastVehicle(null, null)))
                  }
                  sendToImmovable(id, self, lane.id, envelope(id, lane.id, RemovePosition))
                  // persistAsync(PreviousLaneChanged(null)) { evt => }
                  // persist body begin
                  state.previousLaneId = null
                  // persist body end
                  // dal momento che stiamo andando in una zone, azzeriamo il nostro stato
                  // persistAsync(PredecessorGoneSent) { evt => }
                  // persist body begin
                  state.predecessorGoneSent = true
                  state.previousVehicleId = null
                  // persist body end
                  previousVehicle = null
                  if(getMyLength() == car_length) {
                    // persistAsync(CarEvent(NextVehicleGone)) { evt => }
                  }
                  else if (getMyLength() == bus_length) {
                    // persistAsync(BusEvent(NextVehicleGone)) { evt => }
                  }
                  else {
                    // tram
                    // persistAsync(TramEvent(NextVehicleGone)) { evt => }
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
                // persistAsync(IncrementPointIndex) {evt => }
                // persist body begin
                state.currentPointIndex = state.currentPointIndex + 1
                // persist body end
              }
            }
            else {
              // se il flag è uguale a false, significa che fino all'arrivo di una nuova posizione o di un predecessorgone non ha senso mandare posizioni advanced a nessuno
              interestedInVelocityTick = false
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
                    // persistAsync(NextStepEvent) { evt => }
                    // persist body begin
                    state.index = state.index + 1
                    if(state.index >= state.currentRoute.length) {
                      state.handleIndexOverrun
                    }
                    // preoccupati anche del flag di inizio step
                    state.beginOfTheStep = true
                    // persist body end
                    // ora accodati
                    sendToImmovable(id, self, bus_stop.id, envelope(id, bus_stop.id, WaitForPublicTransport(nextStop, state.getSnapshot())))
                    // ammazzati
                    shutdown()
                    /*
                    // prima assicuriamoci di aver salvato tutte le persistAsync
                    defer(DeferObject) { evt =>
                      sendToImmovable(id, self, bus_stop.id, envelope(id, bus_stop.id, WaitForPublicTransport(nextStop, state.getSnapshot())))
                      // ammazzati
                      shutdown()
                    }
                    */
                  }
                }
                else {
                  // siamo un bus che deve far salire e scendere i pedoni, oppure che ha terminato il suo percorso
                  if(pathPhase == 0) {
                    // seleziona chi deve scendere
                    var goingOff = List[(String, MovableStateSnapshot)]()
                    for(entry <- state.travellers) {
                      if(entry._2._1 == bus_stop.id) {
                        val tuple = (entry._1, entry._2._2)
                        goingOff = goingOff :+ tuple
                      }
                    }
                    // rendi persistente la rimozione
                    // persistAsync(BusEvent(TravellersGoneOff(goingOff))) { evt => }
                    // persist body begin
                    for(traveller <- goingOff) {
                      state.travellers = state.travellers - traveller._1
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
                    // persistAsync(NextStepEvent) { evt => }
                    // persist body begin
                    state.index = state.index + 1
                    if(state.index >= state.currentRoute.length) {
                      state.handleIndexOverrun
                    }
                    // preoccupati anche del flag di inizio step
                    state.beginOfTheStep = true
                    // persist body end
                    // ora accodati
                    sendToImmovable(id, self, tram_stop.id, envelope(id, tram_stop.id, WaitForPublicTransport(nextStop, state.getSnapshot())))
                    // ammazzati
                    shutdown()
                    /*
                    // prima assicuriamoci di aver salvato tutte le persistAsync
                    defer(DeferObject) { evt =>
                      sendToImmovable(id, self, tram_stop.id, envelope(id, tram_stop.id, WaitForPublicTransport(nextStop, state.getSnapshot())))
                      // ammazzati
                      shutdown()
                    }
                    */
                  }
                }
                else {
                  // siamo un tram che deve far salire e scendere i pedoni, oppure che ha terminato il suo percorso
                  if(pathPhase == 0) {
                    // seleziona chi deve scendere
                    var goingOff = List[(String, MovableStateSnapshot)]()
                    for(entry <- state.travellers) {
                      if(entry._2._1 == tram_stop.id) {
                        val tuple = (entry._1, entry._2._2)
                        goingOff = goingOff :+ tuple
                      }
                    }
                    // rendi persistente la rimozione
                    // persistAsync(TramEvent(TravellersGoneOff(goingOff))) { evt => }
                    // persist body begin
                    for(traveller <- goingOff) {
                      state.travellers = state.travellers - traveller._1
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
            println("ERRORE: velocity tick dentro un zone step")
            assert(false)
        }
        // controllo sul prossimo messaggio
        if(interestedInVelocityTick) {
          val currentTime = java.lang.System.currentTimeMillis()
          val nextVelocityTick = timestamp + getVelocityTickInterval(id)
          if(nextVelocityTick <= currentTime){
            // sendToMovable(id, self, self, VelocityTick)
            self ! VelocityTick
          }
          else {
            val delay = nextVelocityTick - currentTime
            val cancellable = context.system.scheduler.scheduleOnce(Duration(delay, "millis"), self, VelocityTick)
            //val cancellable = context.system.scheduler.scheduleOnce(Duration(delay, "millis")) {
              //sendToMovable(id, self, self, VelocityTick)
              //self ! VelocityTick
            //}
          }
        }
      }
    
    // PERSISTENCE
    case SaveSnapshot =>
      //state.deliveryState = getDeliverySnapshot
      saveSnapshot(state.getSnapshot())
    case SaveSnapshotSuccess(metadata) =>
      val prevS = previousSequenceNr
      val prevT = previousTimestamp
      previousSequenceNr = metadata.sequenceNr
      previousTimestamp = metadata.timestamp
      if(prevS != -1 && prevT != -1) {
        deleteSnapshot(prevS, prevT)
      }
      // cancella i messaggi fino al sequenceNr alla quale lo snapshot nuovo è stato preso
      deleteMessages(metadata.sequenceNr)
    case SaveSnapshotFailure(metadata, reason) =>
      println("Failed to store snapshot: " + reason)
      
    case PersistenceFailure(payload, sequenceNr, cause) =>
      println("Failed to persist an event: " + cause)
      
    // TIME
    case SubscribeAck =>
      println("Successfully subscribed to time events")
    case UnsubscribeAck =>
      println("Successfully unsubscribed from time events")
    case TimeCommand(timeValue) =>
      // persistAsync(TimeEvent(timeValue)) { evt => }
      // persist body begin
      state.currentTime = timeValue
      // persist body end
      
    // SHUTDOWN
    case Shutdown =>
      context.stop(self)
      
  }
  
  override def receiveRecover: Receive = {
    
    case evt : Event => evt match {
      case NoDuplicate(senderId, deliveryId) =>
        assert(false)
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
        state.previousPointIndex = -1
        state.beginOfTheStep = false
      case IncrementPointIndex =>
        state.currentPointIndex = state.currentPointIndex + 1
      // LANE
      case PreviousVehicleIdArrived(id) =>
        state.previousVehicleId = id
      case PredecessorGoneNotSentYet =>
        state.predecessorGoneSent = false
      case PredecessorGoneSent =>
        state.previousVehicleId = null
        state.predecessorGoneSent = true
        
      // PREVIOUS LANE
      case PreviousLaneChanged(previousLaneId) =>
        state.previousLaneId = previousLaneId
        
      // AT LEAST ONCE
     /* case PersistDeliveryId(deliveryId) =>
        state.deliveryId = deliveryId*/
        
      // DORMI/VEGLIA
      case SleepingStatusChange(alreadyHidden) =>
        state.alreadyHidden = alreadyHidden
        
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
      println("Recovery fallita: " + cause)
    
    case SnapshotOffer(metadata, offeredSnapshot) =>
      offeredSnapshot match {
        case snapshot : MovableState.MovableStateSnapshot =>
          state.setSnapshot(snapshot)
          //setDeliverySnapshot(state.deliveryState)
        case _ =>
          
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
      /*
      command match {
        case ReCreateMe(id) =>
          persist(PersistDeliveryId(deliveryId)) { evt => }
        case PauseExecution(wakeupTime) =>
          persist(PersistDeliveryId(deliveryId)) { evt => }
        case _ =>
          val content = develope(command)
          if(content != null) {
            content match {
              case WaitForPublicTransport(nextStop) =>
                persist(PersistDeliveryId(deliveryId)) { evt => }
              case _ =>
                persistAsync(PersistDeliveryId(deliveryId)) { evt => }
            }
          }
          else {
            persistAsync(PersistDeliveryId(deliveryId)) { evt => }
          }
      }
      // potremmo essere dopo un ripristino
      command match {
        case ReCreateMe(id) =>
          persist(PersistDeliveryId(state.deliveryId + deliveryId)) { evt => }
        case PauseExecution(wakeupTime) =>
          persist(PersistDeliveryId(state.deliveryId + deliveryId)) { evt => }
        case _ =>
          val content = develope(command)
          if(content != null) {
            content match {
              case WaitForPublicTransport(nextStop) =>
                persist(PersistDeliveryId(state.deliveryId + deliveryId)) { evt => }
              case _ =>
                persistAsync(PersistDeliveryId(state.deliveryId + deliveryId)) { evt => }
            }
          }
          else {
            persistAsync(PersistDeliveryId(state.deliveryId + deliveryId)) { evt => }
          }
      }
      val updatedCommand = updateCommand(command,deliveryId)
      */
      ToImmovable(destinationId, ToPersistentMessages.FromMovable(senderId, senderRef, Request(deliveryId, command)))
    })
  }
  
  // UTILITY
  // Funzione che permette l'invio di un messaggio ad un attore mobile, effettuando l'enveloping adeguato
  def sendToMovable(senderId : String, senderRef : ActorRef, destinationRef : ActorRef, command : Command) : Unit = {
    deliver(destinationRef.path, deliveryId => {
      // val updatedCommand = updateCommand(command, deliveryId)
      ToMovable(destinationRef, ToPersistentMessages.FromMovable(senderId, senderRef, Request(deliveryId, command)))
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
    // velocityTimer.cancel()
    // smetti di mandarti messaggi per l'esecuzione di snapshot
    // snapshotTimer.cancel()
    // ammazzati
    self ! Shutdown
  }
  
  // UTILITY
  // Recupera la lunghezza dell'entità rappresentata
  def getMyLength() : Int = {
    getLengthFromId(id)
  }
  
  /*
  // UTILITY
  // aggiorna la deliveryId di uno snapshot
  def updateSnapshot(snapshot : MovableStateSnapshot, deliveryId : Long) : MovableStateSnapshot = {
    return MovableStateSnapshot(
        snapshot.pedestrianRoute,
        snapshot.carRoute,
        snapshot.busRoute,
        snapshot.tramRoute,
        snapshot.currentRoute,
        snapshot.index,
        snapshot.beginOfTheStep,
        snapshot.currentPointsSequence,
        snapshot.currentPointIndex,
        snapshot.currentTime,
        snapshot.nextVehicleId,
        snapshot.previousVehicleId,
        snapshot.predecessorGoneSent,
        snapshot.travellers,
        snapshot.previousLaneId,
        snapshot.lastMessages,
        snapshot.alreadyHidden
    )
  }
  
  // UTILITY
  // modifica un command per includere nello snapshot l'ultima deliveryId, se necessario
  def updateCommand(command : Command, deliveryId : Long) : Command = {
    command match {
      case ReCreateMe(id, snapshot) =>
        return ReCreateMe(id, updateSnapshot(snapshot, deliveryId))
      case PauseExecution(wakeupTime, snapshot) =>
        return PauseExecution(wakeupTime, updateSnapshot(snapshot, deliveryId))
      case _ =>
        val content = develope(command)
        if(content != null) {
          command match {
            case ToBusStop(realCommand) =>
              realCommand match {
                case FromPedestrian(message) =>
                  message match {
                    case WaitForPublicTransport(nextStop, snapshot) =>
                      return ToBusStop(FromPedestrian(WaitForPublicTransport(nextStop, updateSnapshot(snapshot, deliveryId))))
                    case _ =>
                      return command
                  }
                case _ =>
                  return command
              }
            case ToTramStop(realCommand) =>
              realCommand match {
                case FromPedestrian(message) =>
                  message match {
                    case WaitForPublicTransport(nextStop, snapshot) =>
                      return ToTramStop(FromPedestrian(WaitForPublicTransport(nextStop, updateSnapshot(snapshot, deliveryId))))
                    case _ =>
                      return command
                  }
                case _ =>
                  return command
              }
            case _ =>
              return command
          }
        }
        else {
          return command
        }
    }
  }
  */
  
}