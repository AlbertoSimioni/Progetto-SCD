package modelActors.immovable

import scala.concurrent.duration._

import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.ActorSystem
import akka.contrib.pattern.ShardRegion
import akka.persistence.PersistentActor
import akka.persistence.SaveSnapshotSuccess
import akka.persistence.SaveSnapshotFailure
import akka.persistence.SnapshotOffer
import akka.persistence.RecoveryCompleted
import akka.contrib.pattern.ClusterSharding
import akka.persistence.AtLeastOnceDelivery

import ShardUtilities._
import modelActors.movable.MovableActor
import modelActors.CommonMessages._
import modelActors.ToPersistentMessages
import modelActors.ToNonPersistentMessages
import map.Domain._
import map.JSONReader

/**
 * @author Matteo Pozza
 * Template per fondere assieme:
 * - Sharding
 * Deve essere il tipo di attore utilizzato nello sharding
 * - Persistence
 * Ci serve per la fault tolerance, gli attori devono memorizzare un loro stato
 * - At Least Once
 * Ci serve per proteggere le comunicazioni, quando possibile
 * RICORDA:
 * Non è possibile fare sharding di un supertipo, e poi utilizzare in realtà un sottotipo.
 * Per questo motivo siamo costretti a definire tutto il comportamento in questa classe.
 */

object ImmovableActor {
  
  // SHARDING
  // Nome della classe degli attori, serve al ClusterSharding
  val typeOfEntries : String = "ImmovableActor"
  
  // SHARDING
  // Permette la creazione dell'attore dal ClusterSharding
  def props(): Props = Props(new ImmovableActor)
  
  // SHARDING
  // Permette l'estrazione dell'ID del ImmovableActor da un messaggio a lui diretto
  val idExtractor: ShardRegion.IdExtractor = {
    case envelope : ToImmovable =>
        (envelope.destinationId, envelope)
  }
  
  // SHARDING
  // Permette di capire lo shard di appartenenza del ImmovableActor da un messaggio a lui diretto
  val shardResolver: ShardRegion.ShardResolver = msg => msg match {
    case envelope : ToImmovable =>
      decideShard(envelope.destinationId)
  }
  
  // PERSISTENCE
  // Messaggio per il salvataggio e la cancellazione di uno snapshot
  case object SaveSnapshot
  case class DeleteSnapshot(sequenceNr : Long, timestamp : Long)
  
}

class ImmovableActor extends PersistentActor with AtLeastOnceDelivery {
  
  import context.dispatcher
  
  import ImmovableActor._
  
  // PERSISTENCE
  // Serve per attribuire all'attore un ID univoco che serve nella memorizzazione nel DB
  // Non possiamo usare il nostro ID (dalla mappa JSON) in quanto il ClusterSharding non può utilizzare props() con
  // un parametro variabile.
  // Dal momento che gli esempi forniti da Akka suggeriscono questa strategia per la persistenza, noi la manteniamo.
  override def persistenceId: String = "ImmovableActor-" + self.path.name
  
  // PERSISTENCE
  // Permette di effettuare il salvataggio dello snapshot ogni 10 secondi
  val snapshotTimer = context.system.scheduler.schedule(0 millis, 10000 millis, self, SaveSnapshot)
  
  // SHARDING
  // Permette di comunicare con altri TestActor utilizzando il loro identificativo invece che il loro indirizzo
  val shardRegion = ClusterSharding(context.system).shardRegion(ImmovableActor.typeOfEntries)
  
  // PERSISTENCE
  // Lo stato dell'attore deve essere modellato da un var
  var state = new ImmovableState()
  
  // PERSISTENCE
  // numero e timestamp del precedente snapshot
  var previousSequenceNr = -1l
  var previousTimestamp = -1l
  
  // PERSISTENCE
  // Sostituisce la receive normale
  override def receiveCommand: Receive = {
    
    case ToImmovable(destinationId, toImmovableMessage) => toImmovableMessage match {
      case ToPersistentMessages.FromImmovable(senderId, payload) => payload match {
        case Request(deliveryId, command) =>
          // manda ack al mittente
          shardRegion ! ToImmovable(senderId, ToPersistentMessages.FromImmovable(destinationId, Ack(deliveryId)))
          // controlla che il messaggio non sia duplicato
          if(state.isNewMessage(senderId, deliveryId) == true) {
        	  // messaggio nuovo
        	  persist(NoDuplicate(senderId, deliveryId)) { msg =>
        	    state.updateFilter(msg.senderId, msg.deliveryId)
        	  }
        	  // gestione vera e propria del messaggio
        	  command match {
              case ReCreateMobileEntities =>
                // messaggio mandato da se stessi per ricreare le entità mobili
                for(current_id <- state.handledMobileEntities) {
                  val reCreatedMobileEntity = context.actorOf(MovableActor.props(current_id))
                  // fai partire ciascuna entità
                  sendToMovable(destinationId, reCreatedMobileEntity, ResumeExecution)
                }
              
        	    case ToBusStop(command) =>
        	      BusStop.fromImmovableHandler(this, destinationId, senderId, command)
        	    case ToCrossroad(command) =>
        	      // crossroad handler
        	    case ToLane(command) =>
        	      // lane handler
        	    case ToPedestrianCrossroad(command) =>
        	      // pedestrian crossroad handler
        	    case ToRoad(command) =>
        	      // road handler
        	    case ToTramStop(command) =>
        	      // tram stop handler
        	    case ToZone(command) =>
        	      // zone handler
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
          senderRef ! ToMovable(senderRef, ToPersistentMessages.FromImmovable(destinationId, Ack(deliveryId)))
          // controlla che il messaggio non sia duplicato
          if(state.isNewMessage(senderId, deliveryId) == true) {
        	  // messaggio nuovo
        	  persist(NoDuplicate(senderId, deliveryId)) { msg =>
        	    state.updateFilter(msg.senderId, msg.deliveryId)
        	  }
        	  // handling vero e proprio del messaggio
        	  command match {
              case IpRequest =>
                sendToMovable(destinationId, senderRef, IpResponse(getIp()))
              case ReCreateMe(id) =>
                // ri-creazione di un attore mobile
                val reCreatedMobileEntity = context.actorOf(MovableActor.props(id))
                // fai riprendere la sua esecuzione
                sendToMovable(destinationId, reCreatedMobileEntity, ResumeExecution)
              case MobileEntityAdd(id) =>
                persist(MobileEntityArrived(id)) { evt =>
                  if(state.handledMobileEntities.contains(id) == false) {
                    state.handledMobileEntities = state.handledMobileEntities :+ id
                  }
                }
              case MobileEntityRemove(id) =>
                persist(MobileEntityGone(id)) { evt =>
                  if(state.handledMobileEntities.contains(id) == true) {
                    state.handledMobileEntities = state.handledMobileEntities.filter { current_id => current_id != id }
                  }
                }
                
        	    case ToBusStop(command) =>
        	      BusStop.fromMovableHandler(this, destinationId, senderId, sender, command)
        	    case ToCrossroad(command) =>
        	      // crossroad handler
        	    case ToLane(command) =>
        	      // lane handler
        	    case ToPedestrianCrossroad(command) =>
        	      // pedestrian crossroad handler
        	    case ToRoad(command) =>
        	      // road handler
        	    case ToTramStop(command) =>
        	      // tram stop handler
        	    case ToZone(command) =>
        	      // zone handler
        	    case _ =>
        	      println("We should not be here!")
        	  }
          }
        case Ack(deliveryId) =>
          // ack ricevuto, messaggio è stato consegnato con successo
          val ackAboutNewMessage = confirmDelivery(deliveryId)
      }
      case ToPersistentMessages.FromNonPersistent(senderRef, command) => command match {
        case Identity(id) =>
          // messaggio dell'injector, per definirsi
          val entity = id.charAt(0) match {
            case 'R' =>
              val entry = JSONReader.getRoad(current_map, id)
              if(entry.isDefined) {
    	          persist(IdentityArrived(id)) { msg =>
    	            state.id = msg.id
    	            state.roadData = entry.get
    	          }
              }
              else {
    	          println("Problems with received road identifier")
              }
            case 'L' =>
              val entry = JSONReader.getLane(current_map, id)
              if(entry.isDefined) {
          	    persist(IdentityArrived(id)) { msg =>
          	      state.id = msg.id
          	      state.laneData = entry.get
          	    }
              }
              else {
          	    println("Problems with received lane identifier")
              }
            case 'C' =>
              val entry = JSONReader.getCrossroad(current_map, id)
              if(entry.isDefined) {
          	    persist(IdentityArrived(id)) { msg =>
          	      state.id = msg.id
          	      state.crossroadData = entry.get
          	    }
              }
              else {
          	    println("Problems with received crossroad identifier")
              }
            case 'P' =>
              val entry = JSONReader.getPedestrianCrossroad(current_map, id)
              if(entry.isDefined) {
          	    persist(IdentityArrived(id)) { msg =>
          	      state.id = msg.id
          	      state.pedestrian_crossroadData = entry.get
          	    }
              }
              else {
          	    println("Problems with received pedestrian crossroad identifier")
              }
            case 'B' =>
              val entry = JSONReader.getBusStop(current_map, id)
              if(entry.isDefined) {
          	    persist(IdentityArrived(id)) { msg =>
          	      state.id = msg.id
          	      state.bus_stopData = entry.get
          	    }
              }
              else {
          	    println("Problems with received bus stop identifier")
              }
            case 'T' =>
              val entry = JSONReader.getTramStop(current_map, id)
              if(entry.isDefined) {
          	    persist(IdentityArrived(id)) { msg =>
          	      state.id = msg.id
          	      state.tram_stopData = entry.get
          	    }
              }
              else {
          	    println("Problems with received tram stop identifier")
              }
            case 'Z' =>
              val entry = JSONReader.getZone(current_map, id)
              if(entry.isDefined) {
          	    persist(IdentityArrived(id)) { msg =>
          	      state.id = msg.id
          	      state.zoneData = entry.get
          	    }
              }
              else {
          	    println("Problems with received zone identifier")
              }
          }
          
        case CreateMobileEntity(id, route) =>
          // creazione di una entità mobile
          persist(MobileEntityArrived(id)) {evt =>
            if(state.handledMobileEntities.contains(evt.id) == false) {
              state.handledMobileEntities = state.handledMobileEntities :+ evt.id
            }
          }
          val createdMobileEntity = context.actorOf(MovableActor.props(id))
          // invia percorso
          sendToMovable(destinationId, createdMobileEntity, Route(route))
          // fai partire esecuzione
          sendToMovable(destinationId, createdMobileEntity, ResumeExecution)
 
      }
      
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
  
  // PERSISTENCE
  // Receive utilizzata in caso di ripristino
  override def receiveRecover: Receive = {
    case evt : Event => evt match {
      case NoDuplicate(senderId, deliveryId) =>
        state.updateFilter(senderId, deliveryId)
      case IdentityArrived(id) =>
        state.id = id
        val entity = id.charAt(0) match {
        case 'R' =>
          state.roadData = JSONReader.getRoad(current_map, id).get
        case 'L' =>
          state.laneData = JSONReader.getLane(current_map, id).get
        case 'C' =>
          state.crossroadData = JSONReader.getCrossroad(current_map, id).get
        case 'P' =>
          state.pedestrian_crossroadData = JSONReader.getPedestrianCrossroad(current_map, id).get
        case 'B' =>
          state.bus_stopData = JSONReader.getBusStop(current_map, id).get
        case 'T' =>
          state.tram_stopData = JSONReader.getTramStop(current_map, id).get
        case 'Z' =>
          state.zoneData = JSONReader.getZone(current_map, id).get
        }
      case MobileEntityArrived(id) =>
        if(state.handledMobileEntities.contains(id) == false) {
          state.handledMobileEntities = state.handledMobileEntities :+ id
        }
      case MobileEntityGone(id) =>
        if(state.handledMobileEntities.contains(id) == true) {
          state.handledMobileEntities = state.handledMobileEntities.filter { current_id => current_id != id }
        }
      case BusStopEvent(event) =>
        BusStop.eventHandler(event, state)
      case CrossroadEvent(event) =>
        // crossroad handler
      case LaneEvent(event) =>
        // lane handler
      case PedestrianCrossroadEvent(event) =>
        // pedestrian crossroad handler
      case RoadEvent(event) =>
        // road handler
      case TramStopEvent(event) =>
        // tram stop handler
      case ZoneEvent(event) =>
        // zone handler
    }
    
    case RecoveryCompleted =>
      // dobbiamo far partire tutti gli attori sotto la nostra gestione
      // non possiamo farlo subito però, potrebbero esservi delle rimozioni di id dalla lista di entità gestite in coda dei messaggi
      sendToImmovable(state.id, state.id, ReCreateMobileEntities)
    
    case SnapshotOffer(metadata, offeredSnapshot) =>
      offeredSnapshot match {
        case Some(snapshot : ImmovableState) =>
          state = snapshot
          //setDeliverySnapshot(state.deliveryState)
          println("State recovered from snapshot successfully")
      }
    
  }
  
  // UTILITY
  // Funzione che permette l'invio di un messaggio ad un attore immobile, effettuando l'enveloping adeguato
  def sendToImmovable(senderId : String, destinationId : String, command : Command) : Unit = {
    deliver(shardRegion.path, deliveryId => ToImmovable(destinationId, ToPersistentMessages.FromImmovable(senderId, Request(deliveryId, command))))
  }
  
  // UTILITY
  // Funzione che permette l'invio di un messaggio ad un attore mobile, effettuando l'enveloping adeguato
  def sendToMovable(senderId : String, destinationRef : ActorRef, command : Command) : Unit = {
    deliver(destinationRef.path, deliveryId => ToMovable(destinationRef, ToPersistentMessages.FromImmovable(senderId, Request(deliveryId, command))))
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
  
  // UTILITY
  // Ritrona l'indirizzo IP della macchina su cui l'attore risiede
  def getIp() : String = {
    context.system.settings.config.getString("akka.remote.netty.tcp.hostname")
  }
  
}