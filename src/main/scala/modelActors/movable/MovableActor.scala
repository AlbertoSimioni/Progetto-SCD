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
import akka.persistence.RecoveryCompleted
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
import map.PointsSequence._
import map.Domain._
import map.Routes._
import MovableState._

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
  
}

class MovableActor(id : String) extends PersistentActor with AtLeastOnceDelivery {
  
  import context.dispatcher
  
  import MovableActor._
  
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
  
  // pedestrian crossroad
  // variabile per il pedone, modella dove siamo arrivati nel percorso
  var pedestrianCrossroadPhase = 0
  
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
            persist(NoDuplicate(senderId, deliveryId)) { msg =>
              state.updateFilter(msg.senderId, msg.deliveryId)
            }
            // gestione vera e propria del messaggio
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
                persist(RouteArrived(route)) { evt =>
                  state.handleRoute(route)
                }
              case ResumeExecution =>
                // comincia (o riprendi) l'esecuzione
                sendToImmovable(id, self, state.getCurrentStepId, IpRequest)
              case MovableActorResponse(id, ref) =>
                // per il momento, l'unico caso in cui possiamo ricevere questa risposta è per la richiesta del previousVehicle
                assert(id == state.previousVehicleId)
                previousVehicle = ref
                // fai ripartire l'esecuzione
                sendToMovable(id, self, self, ExecuteCurrentStep)
                
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
            persist(NoDuplicate(senderId, deliveryId)) { msg =>
              state.updateFilter(msg.senderId, msg.deliveryId)
            }
            // handling vero e proprio del messaggio
            command match {
              case ExecuteCurrentStep =>
                val stepSequence = state.getStepSequence()
                state.currentRoute(state.index) match {
                  case road_step(road, direction) =>
                    // solo un pedone può avere road_step
                    if(state.beginOfTheStep) {
                      val currentPointsSequence = getPointsSequence(id, stepSequence)
                      persist(BeginOfTheStep(currentPointsSequence)) { evt =>
                        state.currentPointsSequence = evt.pointsSequence
                        state.currentPointIndex = 0
                        state.beginOfTheStep = false
                      }
                    }
                    // attiva l'interessamento agli eventi di avanzamento
                    interestedInVelocityTick = true
                  case lane_step(lane, direction) =>
                    // sto approciando una lane
                    // potrmmo essere nel caso in cui non siamo riusciti a mandare PredecessorGone prima del crash del nodo in cui siamo
                    // la situazione è segnalata dal flag booleano predecessorGoneSent
                    // in questo caso, bisogna recuperare il riferimento al veicolo precedente prima di procedere
                    if(previousVehicle == null && state.predecessorGoneSent == false) {
                      // l'unico caso in cui possiamo essere è che non siamo riusciti ad inviare PredecessorGone prima di morire
                      // recupera id della lane precedente
                      val laneId = state.getStepIdAt(-2)
                      // manda un messaggio di richiesta del veicolo in questione
                      sendToImmovable(id, self, laneId, MovableActorRequest(state.previousVehicleId))
                    }
                    else {
                      // per prima cosa, qualora vi fosse un veicolo predecessore, informalo che non deve più seguirci
                      if(previousVehicle != null) {
                        sendToMovable(id, self, previousVehicle, envelope(id, state.previousVehicleId, PredecessorGone))
                        persist(PredecessorGoneSent) { evt =>
                          state.previousVehicleId = null
                          state.predecessorGoneSent = true
                        }
                        previousVehicle = null
                      }
                      // se c'è una qualche previousLane, avvisala di modificare i campi lastVehicle (qualora fossimo stati l'unico veicolo)
                      if(previousLaneId != null) {
                        sendToImmovable(id, self, previousLaneId, HandleLastVehicle)
                        previousLaneId = null
                      }
                      // a prescindere da primo approccio allo step o ripristino, l'unico dato di cui dispongo
                      // è l'eventuale id del next vehicle
                      if(state.nextVehicleId == null) {
                        // dobbiamo ancora iniziare
                        // avvisa l'entità precedente (incrocio/ strisce pedonali, etc.) che sei ufficialmente sulla nuova lane
                        // in questo modo, l'entità in questione rende persistente l'occupazione della lane considerata.
                        val previousId = state.getPreviousStepId
                        // recupera anche l'id della lane da cui provenivi
                        val previousLaneId = state.getStepIdAt(-2)
                        sendToImmovable(id, self, previousId, envelope(id, previousId, VehicleBusy(previousLaneId)))
                        // manda un messaggio alla lane per ricevere il ref
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
                    
                  case crossroad_step(crossroad, direction) =>
                    //
                  case pedestrian_crossroad_step(pedestrian_crossroad, direction) =>
                    pedestrianCrossroadPhase = 0
                    // vi sono due possibilità: sono un pedone, o sono un veicolo
                    // se sono un pedone, vi sono due possibilità: o le ignoro (ho un solo percorso), o le affronto (ho 2 percorsi)
                    // se sono un veicolo, ho un solo percorso e le devo affrontare
                    // ricordiamoci che non dobbiamo salvare nulla
                    val currentPointsSequence = getPointsSequence(id, stepSequence)
                    if(currentPointsSequence.length > 1) {
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
                    //
                  case tram_stop_step(tram_stop, direction, ignore) =>
                    //
                  case zone_step(zone, direction) =>
                    // PRECONDIZIONE: solo un pedone o un veicolo possono avere uno zone_step nel loro percorso
                    // siamo dunque autorizzati ad attuare la logica di dormi/veglia
                    //
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
                    if(isLate(state.currentTime, comparedTime)) {
                      // se in ritardo, vai al prossimo step
                      sendToMovable(id, self, self, PersistAndNextStep)
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
                persist(NextStepEvent) { evt =>
                  state.index = state.index + 1
                  if(state.index >= state.currentRoute.length) {
                    state.handleIndexOverrun
                  }
                  // preoccupati anche del flag di inizio step
                  state.beginOfTheStep = true
                }
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
      saveSnapshot(state)
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
      deleteSnapshot(sequenceNr, timestamp)
      
    // TIME
    case SubscribeAck =>
      println("Successfully subscribed to time events")
    case UnsubscribeAck =>
      println("Successfully unsubscribed from time events")
    case TimeCommand(timeValue) =>
      persist(TimeEvent(timeValue)) { evt =>
        state.currentTime = evt.timeValue
      }
      
    // VELOCITY
    case VelocityTick =>
      if(interestedInVelocityTick) {
        state.currentRoute(state.index) match {
          case road_step(road, direction) =>
            // LANCIA EVENTO LEGATO AL PUNTO CORRENTE
            
            // esamina dove siamo nella sequenza di punti
            if(state.currentPointIndex == state.currentPointsSequence(0).length-1) {
              // procedi al prossimo step
              sendToMovable(id, self, self, PersistAndNextStep)
              // fine del componente che stiamo percorrendo, spegni l'interruttore
              interestedInVelocityTick = false
            }
            else {
              persist(IncrementPointIndex) {evt =>
               state.currentPointIndex = state.currentPointIndex + 1 
              }
            }
          case lane_step(lane, direction) =>
            // LANCIA EVENTO LEGATO AL PUNTO CORRENTE
            
            // c'è un veicolo dietro?
            if(previousVehicle != null) {
              // c'è qualcuno, manda update della posizione
              val currentPoint = state.currentPointsSequence(0)(state.currentPointIndex)
              sendToMovable(id, self, previousVehicle, envelope(id, state.previousVehicleId, Advanced(currentPoint)))
            }
            // qualora sia stata raggiunta la vehicle length + 1, manda all'entità precedente un messaggio vehicle free
            // per approccio conservativo, utilizziamo la bus_length
            // non utilizziamo la tram_length perchè si assume che per una tratta del tram giri un solo tram, che quindi non deve competere con nessun altro tram
            if(state.currentPointIndex == bus_length + 1) {
              val previousStepId = state.getPreviousStepId
              // recupera anche l'id della lane da cui provenivamo
              val previousLaneId = state.getStepIdAt(-2)
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
                persist(PredecessorGoneNotSentYet) { evt =>
                  state.predecessorGoneSent = false
                }
                // procedi al prossimo step
                sendToMovable(id, self, self, PersistAndNextStep)
                // la lane corrente diventa la nostra previousLane
                previousLaneId = lane.id
                // fine del componente che stiamo percorrendo, spegni l'interruttore
                interestedInVelocityTick = false
              }
              else {
                persist(IncrementPointIndex) {evt =>
                 state.currentPointIndex = state.currentPointIndex + 1 
                }
              }
            }
          case crossroad_step(crossroad, direction) =>
            //
          case pedestrian_crossroad_step(pedestrian_crossroad, direction) =>
            // possiamo essere un pedone o un veicolo
            // in ogni caso, possiamo procedere senza problemi fino alla fine della (prima) sequenza di punti
            // LANCIA EVENTO LEGATO AL PUNTO CORRENTE
            
            // esamina dove siamo nella sequenza di punti
            if(state.currentPointIndex == state.currentPointsSequence(pedestrianCrossroadPhase).length-1) {
              if(pedestrianCrossroadPhase == 0) {
                // controlliamo il numero di sequenze che abbiamo
                if(state.currentPointsSequence.length == 1) {
                  // veicolo o pedone che ignora le strisce
                  // procedi al prossimo step
                  sendToMovable(id, self, self, PersistAndNextStep)
                  // fine del componente che stiamo percorrendo, spegni l'interruttore
                  interestedInVelocityTick = false
                }
                else {
                  // siamo un pedone che è arrivato a dover attraversare effettiavmente le strisce
                  // avanza di fase
                  pedestrianCrossroadPhase = pedestrianCrossroadPhase + 1
                  // azzera l'indice
                  state.currentPointIndex = 0
                  // disabilita l'interessamento ai velocity tick
                  interestedInVelocityTick = false
                  // manifesta la volontà di attraversare
                  sendToImmovable(id, self, pedestrian_crossroad.id, envelope(id, pedestrian_crossroad.id, Cross_In))
                }
              }
              else if(pedestrianCrossroadPhase == 1) {
                // siamo arrivati alla fine dell'attraversamento vero e proprio
                // mantieni l'interessamento agli eventi di avanzamento
                // avanza di fase
                pedestrianCrossroadPhase = pedestrianCrossroadPhase + 1
                // azzera l'indice
                state.currentPointIndex = 0
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
              state.currentPointIndex = state.currentPointIndex + 1
            }
            
          case bus_stop_step(bus_stop, direction, ignore) =>
            //
          case tram_stop_step(tram_stop, direction, ignore) =>
            //
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
        
      case PedestrianEvent(event) =>
        Pedestrian.eventHandler(event, state)
      case CarEvent(event) =>
        Car.eventHandler(event, state)
      case BusEvent(event) =>
        Bus.eventHandler(event, state)
      case TramEvent(event) =>
        Tram.eventHandler(event, state)
    }
    
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
    deliver(shardRegion.path, deliveryId => ToImmovable(destinationId, ToPersistentMessages.FromMovable(senderId, senderRef, Request(deliveryId, command))))
  }
  
  // UTILITY
  // Funzione che permette l'invio di un messaggio ad un attore mobile, effettuando l'enveloping adeguato
  def sendToMovable(senderId : String, senderRef : ActorRef, destinationRef : ActorRef, command : Command) : Unit = {
    deliver(destinationRef.path, deliveryId => ToMovable(destinationRef, ToPersistentMessages.FromMovable(senderId, senderRef, Request(deliveryId, command))))
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
    // smatti di mandarti messaggi per la regolazione della velocità
    velocityTimer.cancel()
    // smetti di mandarti messaggi per l'esecuzione di snapshot
    snapshotTimer.cancel()
    // ammazzati
    self ! PoisonPill
  }
  
  // UTILITY
  // Recupera la lunghezza dell'entità rappresentata
  def getMyLength() : Int = {
    id.substring(0, 3) match {
      case "PED" =>
        return pedestrian_length
      case "CAR" =>
        return car_length
      case "BUS" =>
        return bus_length
      case "TRA" =>
        return tram_length
    }
  }
  
}