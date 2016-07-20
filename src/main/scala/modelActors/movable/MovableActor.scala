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

import modelActors.immovable.ImmovableActor
import modelActors.CommonMessages._
import modelActors.ToPersistentMessages
import modelActors.ToNonPersistentMessages

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
                  self ! PoisonPill
                }
              case Route(route) =>
                // è arrivato il percorso
                persist(RouteArrived(route)) { evt =>
                  state.handleRoute(route)
                }
              case ResumeExecution =>
                // comincia (o riprendi) l'esecuzione
                sendToImmovable(id, self, state.getCurrentStepId, IpRequest)
              case ToPedestrian(command) =>
                // pedestrian handler
              case ToCar(command) =>
                // car handler
              case ToBus(command) =>
                // bus handler
              case ToTram(command) =>
                // tram handler
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
                // DA DEFINIRE
              case PersistAndNextStep =>
                // memorizza
                persist(NextStepEvent) { evt =>
                  state.index = state.index + 1
                  if(state.index >= state.currentRoute.length) {
                    state.handleIndexOverrun
                  }
                }
                // avanti col prossimo step
                sendToImmovable(id, self, state.getCurrentStepId, IpRequest)
              
              case ToPedestrian(command) =>
                // pedestrian handler
              case ToCar(command) =>
                // car handler
              case ToBus(command) =>
                // bus handler
              case ToTram(command) =>
                // tram handler
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
      case RouteArrived(route) =>
        state.handleRoute(route)
        
      case PedestrianEvent(event) =>
        // pedestrian handler
      case CarEvent(event) =>
        // car handler
      case BusEvent(event) =>
        // bus handler
      case TramEvent(event) =>
        // tram handler
    }
    
    case SnapshotOffer(metadata, offeredSnapshot) =>
      offeredSnapshot match {
        case Some(snapshot : MovableState) =>
          state = snapshot
          //setDeliverySnapshot(state.deliveryState)
          println("State recovered from snapshot successfully")
      }
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
  // Funzione che rende persistente un evento
  def persistEvent(event : Event, handler : Event => Unit) : Unit = {
    persist(event) {
      handler
    } 
  }
  
}