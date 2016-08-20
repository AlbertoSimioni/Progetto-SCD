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
import akka.persistence.RecoveryFailure
import akka.persistence.PersistenceFailure
import akka.contrib.pattern.ClusterSharding
import akka.persistence.AtLeastOnceDelivery
import akka.contrib.pattern.DistributedPubSubMediator
import akka.contrib.pattern.DistributedPubSubExtension
import DistributedPubSubMediator.Subscribe
import DistributedPubSubMediator.SubscribeAck
import akka.actor.ActorLogging

import ShardUtilities._
import modelActors.movable.MovableActor
import modelActors.Messages._
import common.CommonMessages._
import common.ToPersistentMessages
import common.ToNonPersistentMessages
import map.Domain._
import map.Domain.category._
import map.Domain.position._
import map.JSONReader
import map.Routes._
import time.TimeMessages._
import pubsub.PublisherInstance
import pubsub.Messages._
import modelActors.movable.MovableState
import modelActors.movable.MovableState._

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
      val shardId = decideShard(envelope.destinationId)
      shardId
  }
  
  // PERSISTENCE
  // Messaggio per il salvataggio e la cancellazione di uno snapshot
  case object SaveSnapshot
  
  // CROSSROAD
  case object SemaphoreSwitch


}

class ImmovableActor extends PersistentActor with AtLeastOnceDelivery with ActorLogging {
  
  import context.dispatcher
  
  import ImmovableActor._
  
  // GUI
  // riferimento per la pubblicazione di eventi grafici
  val publisherGuiHandler = PublisherInstance.getPublisherModelEvents(context.system)
  
  // PERSISTENCE
  // Serve per attribuire all'attore un ID univoco che serve nella memorizzazione nel DB
  // Non possiamo usare il nostro ID (dalla mappa JSON) in quanto il ClusterSharding non può utilizzare props() con
  // un parametro variabile.
  // Dal momento che gli esempi forniti da Akka suggeriscono questa strategia per la persistenza, noi la manteniamo.
  override def persistenceId: String = "ImmovableActor-" + self.path.name
  
  // PERSISTENCE
  // Permette di effettuare il salvataggio dello snapshot ogni 10 secondi
  // val snapshotTimer = context.system.scheduler.schedule(180 seconds, 20 seconds, self, SaveSnapshot)

  // SHARDING
  // Permette di comunicare con altri TestActor utilizzando il loro identificativo invece che il loro indirizzo
  val shardRegion = ClusterSharding(context.system).shardRegion(ImmovableActor.typeOfEntries)
  
  // TIME
  // Recupera l'attore che gli permette di ricevere gli eventi temporali e si sottoscrive immediatamente
  val mediator = DistributedPubSubExtension(context.system).mediator
  mediator ! Subscribe(timeMessage, self)
  
  // DOMINIO
  // Riferimento non persistente all'ultimo veicolo che è entrato in corsia
  var lastVehicleEntered : ActorRef = null
  // Id dell'ultimo veicolo che è entrato in corsia
  var lastVehicleEnteredId : String = null
  
  // FAULT TOLERANCE
  // mappa con id e actorref degli attori mobili gestiti, mantenuta aggiornata, ma non persistente
  // PROBLEMA: un actorref non può essere reso persistente perchè al ripristino non ha alcun significato
  // Quando una lane ricrea dei veicoli, ne salva l'actorref.
  // I veicoli chiederanno alla lane l'actorref sulla base dell'id
  // La lane restituirà l'actorref appena creato.
  var handledMobileEntitiesMap = Map[String, ActorRef]()
  
  // LANE
  // Mappa con veicoli e posizioni
  // Serve per permettere l'inserimento di veicoli da zone, invece che dall'inizio della corsia
  // deve essere mantenuta consistente con la mappa handledMobileEntitiesMap
  // in particolare, un veicolo aggiunto a handledMobileEntitiesMap non è necessariamente aggiunto anche a positionsMap
  // mentre un veicolo rimosso deve essere rimosso anche dalla mappa
  var positionsMap = Map[String, (ActorRef, point)]()
  // Richieste pendenti per chi deve entrare a metà della lane
  var pendingLaneRequests = Map[String, (ActorRef, point, direction)]()
  
  // VARIABILI PER STRISCE PEDONALI
  // vehicle_pass => gestisce se si è in attraversamento pedoni o attraversamento veicoli
  // pedestrianRequest => coda per le richieste dei pedoni
  // vehicleRequests => key = lane, entry = id Veicolo + actorref
  // numpedestriancrossing => ci dice se le strisce sono libere o no
  // vehicleFreeTempMap => obiettivo principale è memorizzare i vehiclefree e i vehiclebusy, ma non appena è
  // stato concesso l'accesso ad un veicolo, la variabile corrispondente diventa false (invece di aspettare vehiclebusy)
  // Questo per fault tolerance purposes
  var pedestrianRequests = Map[String, ActorRef]()
  var vehicleRequests = Map[String, (String, ActorRef)]()
  var numPedestrianCrossing = 0
  var numVehicleCrossing = 0
  var vehicleFreeTempMap = Map[String, Boolean]()
  
  // variabili per gestione incroci classic e roundabout
  var crossroadLock = true
  var vehicleDestinationRequests = Map[String, List[(String, ActorRef)]]()
  
  // BUS / TRAM STOP
  // coda non persistente dei pedoni che sono in attesa alla fermata
  // non persistente perchè in caso di ripristino i pedoni partiranno da capo della bus/tram_stop
  // key: id pedone
  // value: bus stop destinazione + snapshot
  var travellersQueue = List[(String, (String, MovableStateSnapshot))]()
  
  // CROSSROAD
  // variabile per il vehicle_free (non abbiamo bisogno di una mappa, basta singola variabile)
  // anche in questo caso, abbiamo la controparte nello stato persistente
  // questa serve solo per impedire il passaggio appena diopo che è stato concesso il vehicle Out
  var vehicleFreeTemp = true
  // configurazione dell'incrocio (utile solo se l'incrocio è classic, semaphore o roundabout)
  var crossroadConfiguration : Map[String, (Boolean, position, List[String])] = null
  // corsia correntemente col verde
  var greenLane : String = null
  
  // tick per il cambiamento del semaforo ogni 5 secondi
  val semaphoreSwitcher = context.system.scheduler.schedule(0 millis, 5000 millis, self, SemaphoreSwitch)
  
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
        	  // persistAsync(NoDuplicate(senderId, deliveryId)) { msg => }
            // persist body begin
            state.updateFilter(senderId, deliveryId)
            // persist body end
        	  // gestione vera e propria del messaggio
            printMessage(senderId, destinationId, command)
        	  command match {
              case ReCreateMobileEntities =>
                // messaggio mandato da se stessi per ricreare le entità mobili
                for(current_id <- state.handledMobileEntities) {
                  val reCreatedMobileEntity = context.actorOf(MovableActor.props(current_id)/*.withDispatcher("custom-dispatcher")*/)
                  // fai partire ciascuna entità
                  sendToMovable(destinationId, reCreatedMobileEntity, ResumeExecution)
                  // aggiungi una entry alla tabella 
                  handledMobileEntitiesMap = handledMobileEntitiesMap + (current_id -> reCreatedMobileEntity)
                }
              
        	    case ToBusStop(command) =>
        	      BusStop.fromImmovableHandler(this, destinationId, senderId, command)
        	    case ToCrossroad(command) =>
        	      Crossroad.fromImmovableHandler(this, destinationId, senderId, command)
        	    case ToLane(command) =>
        	      Lane.fromImmovableHandler(this, destinationId, senderId, command)
        	    case ToPedestrianCrossroad(command) =>
        	      PedestrianCrossroad.fromImmovableHandler(this, destinationId, senderId, command)
        	    case ToRoad(command) =>
        	      Road.fromImmovableHandler(this, destinationId, senderId, command)
        	    case ToTramStop(command) =>
        	      TramStop.fromImmovableHandler(this, destinationId, senderId, command)
        	    case ToZone(command) =>
        	      Zone.fromImmovableHandler(this, destinationId, senderId, command)
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
          senderRef ! ToMovable(senderRef, ToPersistentMessages.FromImmovable(destinationId, Ack(deliveryId)))
          // controlla che il messaggio non sia duplicato
          val actorPath = senderRef.path.toSerializationFormat
          if(state.isNewMessage(actorPath, deliveryId) == true) {
        	  // messaggio nuovo
        	  // persistAsync(NoDuplicate(actorPath, deliveryId)) { msg => }
            // persist body begin
            state.updateFilter(actorPath, deliveryId)
            // persist body end
        	  // handling vero e proprio del messaggio
            printMessage(senderId, destinationId, command)
        	  command match {
              case IpRequest =>
                sendToMovable(destinationId, senderRef, IpResponse(getIp()))
              case ReCreateMe(id, snapshot) =>
                // ri-creazione di un attore mobile
                val reCreatedMobileEntity = context.actorOf(MovableActor.props(id)/*.withDispatcher("custom-dispatcher")*/)
                // offri lo snapshot
                sendToMovable(destinationId, reCreatedMobileEntity, MovableStateSnapshotOffer(snapshot))
                // fai riprendere la sua esecuzione
                sendToMovable(destinationId, reCreatedMobileEntity, ResumeExecution)
              case MobileEntityAdd(id) =>
                // persistAsync(MobileEntityArrived(id)) { evt => }
                // persist body begin
                if(state.handledMobileEntities.contains(id) == false) {
                  state.handledMobileEntities = state.handledMobileEntities :+ id
                }
                // persist body end
                // aggiungi o aggiorna
                if(handledMobileEntitiesMap.contains(id) == true) {
                  handledMobileEntitiesMap = handledMobileEntitiesMap.updated(id, senderRef)
                }
                else {
                  handledMobileEntitiesMap = handledMobileEntitiesMap + (id -> senderRef)
                }
              case MobileEntityRemove(id) =>
                // persistAsync(MobileEntityGone(id)) { evt => }
                // persist body begin
                if(state.handledMobileEntities.contains(id) == true) {
                  state.handledMobileEntities = state.handledMobileEntities.filter { current_id => current_id != id }
                }
                // persist body end
                // rimuovi if any
                handledMobileEntitiesMap = handledMobileEntitiesMap.filter(pair => pair._1 != id)
              case PauseExecution(wakeupTime, snapshot) =>
                // l'attore mobile chiede di essere messo in stato dormiente
                // persistAsync(MobileEntitySleeping(senderId, wakeupTime, snapshot)) { evt => }
                // persist body begin
                state.addSleepingActor(senderId, wakeupTime, snapshot)
                // persist body end
              case MovableActorRequest(id) =>
                // recupera l'actorref dalla tabella, if any
                val actorRef = handledMobileEntitiesMap.get(id).getOrElse(null)
                // spedisci indietro
                sendToMovable(state.id, senderRef, MovableActorResponse(id, actorRef))
                
        	    case ToBusStop(command) =>
        	      BusStop.fromMovableHandler(this, destinationId, senderId, sender, command)
        	    case ToCrossroad(command) =>
        	      Crossroad.fromMovableHandler(this, destinationId, senderId, sender, command)
        	    case ToLane(command) =>
        	      Lane.fromMovableHandler(this, destinationId, senderId, sender, command)
        	    case ToPedestrianCrossroad(command) =>
        	      PedestrianCrossroad.fromMovableHandler(this, destinationId, senderId, sender, command)
        	    case ToRoad(command) =>
        	      Road.fromMovableHandler(this, destinationId, senderId, sender, command)
        	    case ToTramStop(command) =>
        	      TramStop.fromMovableHandler(this, destinationId, senderId, sender, command)
        	    case ToZone(command) =>
        	      Zone.fromMovableHandler(this, destinationId, senderId, sender, command)
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
      case ToPersistentMessages.FromNonPersistent(senderRef, command) => command match {
        case Identity(id) =>
          println("Identità arrivata: " + id)
          // messaggio dell'injector, per definirsi
          val entity = id.charAt(0) match {
            case 'R' =>
              val entry = JSONReader.getRoad(current_map, id)
              if(entry.isDefined) {
    	          // persistAsync(IdentityArrived(id)) { msg => }
                // persist body begin
                state.id = id
                state.kind = "Road"
                state.roadData = entry.get
                // persist body end
              }
              else {
    	          println("Problems with received road identifier")
              }
            case 'L' =>
              val entry = JSONReader.getLane(current_map, id)
              if(entry.isDefined) {
          	    // persistAsync(IdentityArrived(id)) { msg => }
                // persist body begin
                state.id = id
                state.kind = "Lane"
                state.laneData = entry.get
                // persist body end
              }
              else {
          	    println("Problems with received lane identifier")
              }
            case 'C' =>
              val entry = JSONReader.getCrossroad(current_map, id)
              if(entry.isDefined) {
          	    // persistAsync(IdentityArrived(id)) { msg => }
                // persist body begin
                state.id = id
                state.kind = "Crossroad"
                state.crossroadData = entry.get
                // persist body end
                crossroadConfiguration = getCrossroadConfiguration(state.crossroadData.id)
                greenLane = crossroadConfiguration.keys.head
              }
              else {
          	    println("Problems with received crossroad identifier")
              }
            case 'P' =>
              val entry = JSONReader.getPedestrianCrossroad(current_map, id)
              if(entry.isDefined) {
          	    // persistAsync(IdentityArrived(id)) { msg => }
                // persist body begin
                state.id = id
                state.kind = "Pedestrian_Crossroad"
                state.pedestrian_crossroadData = entry.get
                // persist body end
              }
              else {
          	    println("Problems with received pedestrian crossroad identifier")
              }
            case 'B' =>
              val entry = JSONReader.getBusStop(current_map, id)
              if(entry.isDefined) {
          	    // persistAsync(IdentityArrived(id)) { msg => }
                // persist body begin
        	      state.id = id
                state.kind = "Bus_Stop"
        	      state.bus_stopData = entry.get
                // persist body end
              }
              else {
          	    println("Problems with received bus stop identifier")
              }
            case 'T' =>
              val entry = JSONReader.getTramStop(current_map, id)
              if(entry.isDefined) {
          	    // persistAsync(IdentityArrived(id)) { msg => }
                // persist body begin
                state.id = id
                state.kind = "Tram_Stop"
                state.tram_stopData = entry.get
                // persist body end
              }
              else {
          	    println("Problems with received tram stop identifier")
              }
            case 'Z' =>
              val entry = JSONReader.getZone(current_map, id)
              if(entry.isDefined) {
          	    // persistAsync(IdentityArrived(id)) { msg => }
                // persist body begin
                state.id = id
                state.kind = "Zone"
                state.zoneData = entry.get
                // persist body end
              }
              else {
          	    println("Problems with received zone identifier")
              }
          }
          
        case CreateMobileEntity(id, route) =>
          // creazione di una entità mobile
          // persistAsync(MobileEntityArrived(id)) {evt => }
          // persist body begin
          if(state.handledMobileEntities.contains(id) == false) {
            state.handledMobileEntities = state.handledMobileEntities :+ id
          }
          // persist body end
          val createdMobileEntity = context.actorOf(MovableActor.props(id)/*.withDispatcher("custom-dispatcher")*/)
          // aggiungi o aggiorna
          if(handledMobileEntitiesMap.contains(id) == true) {
            handledMobileEntitiesMap = handledMobileEntitiesMap.updated(id, createdMobileEntity)
          }
          else {
            handledMobileEntitiesMap = handledMobileEntitiesMap + (id -> createdMobileEntity)
          }
          // invia percorso
          sendToMovable(destinationId, createdMobileEntity, Route(route))
          // fai partire esecuzione
          sendToMovable(destinationId, createdMobileEntity, ResumeExecution)
 
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
    case TimeCommand(timeValue) =>
      val toBeWakenUp = state.actorsToBeWakenUp(timeValue)
      if(toBeWakenUp.isEmpty == false) {
        for(id <- toBeWakenUp) {
          println(state.id + ": sto svegliando " + id._1 + " perchè sono le " + timeValue)
          // persistAsync(MobileEntityWakingUp(id)) { evt => }
          // persist body begin
          state.removeSleepingActor(id._1)
          // persist body end
          val wakenUpEntity = context.actorOf(MovableActor.props(id._1)/*.withDispatcher("custom-dispatcher")*/)
          sendToMovable(state.id, wakenUpEntity, MovableStateSnapshotOffer(MovableState.updateSnapshot(id._2, timeValue)))
          sendToMovable(state.id, wakenUpEntity, ResumeExecution)
        }
      }
      
    // CROSSROAD
    case SemaphoreSwitch =>
      if(state.crossroadData != null && state.crossroadData.category == `semaphore`) {
        // cambia verde
        greenLane = crossroadConfiguration.get(greenLane).get._3(0)
        // LANCIA EVENTO LEGATO AL SEMAFORO
        var upGreen = false
        var rightGreen = false
        var downGreen = false
        var leftGreen = false
        var tramGreen = false
        for(entry <- crossroadConfiguration) {
          if(entry._1 == greenLane) {
            entry._2._2 match {
              case `up` =>
                if(entry._2._1) {
                  tramGreen = true
                }
                else {
                  upGreen = true
                }
              case `down` =>
                if(entry._2._1) {
                  tramGreen = true
                }
                else {
                  downGreen = true
                }
              case `right` =>
                if(entry._2._1) {
                  tramGreen = true
                }
                else {
                  rightGreen = true
                }
              case `left` =>
                if(entry._2._1) {
                  tramGreen = true
                }
                else {
                  leftGreen = true
                }
            }
          }
        }
        publisherGuiHandler ! semaphoreState(state.id, upGreen, rightGreen, downGreen, leftGreen, tramGreen)
        
        Crossroad.HandleSemaphoreSwitch(this, state.id)
      }
      
    // GESTIONE SALITA DEI PASSEGGERI DAL MEZZO PUBBLICO
    case ToBusStop(command) =>
      // l'unico che può mandare u messaggio così è se stesso
      BusStop.fromImmovableHandler(this, state.id, state.id, command)
    case ToTramStop(command) =>
      // l'unico che può mandare u messaggio così è se stesso
      TramStop.fromImmovableHandler(this, state.id, state.id, command)
      
  }
  
  // PERSISTENCE
  // Receive utilizzata in caso di ripristino
  override def receiveRecover: Receive = {
    case evt : Event => evt match {
      case NoDuplicate(senderId, deliveryId) =>
        assert(false)
        state.updateFilter(senderId, deliveryId)
      case IdentityArrived(id) =>
        state.id = id
        val entity = id.charAt(0) match {
        case 'R' =>
          state.kind = "Road"
          state.roadData = JSONReader.getRoad(current_map, id).get
        case 'L' =>
          state.kind = "Lane"
          state.laneData = JSONReader.getLane(current_map, id).get
        case 'C' =>
          state.kind = "Crossroad"
          state.crossroadData = JSONReader.getCrossroad(current_map, id).get
        case 'P' =>
          state.kind = "Pedestrian_Crossroad"
          state.pedestrian_crossroadData = JSONReader.getPedestrianCrossroad(current_map, id).get
        case 'B' =>
          state.kind = "Bus_Stop"
          state.bus_stopData = JSONReader.getBusStop(current_map, id).get
        case 'T' =>
          state.kind = "Tram_Stop"
          state.tram_stopData = JSONReader.getTramStop(current_map, id).get
        case 'Z' =>
          state.kind = "Zone"
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
        
      // TIME
      case MobileEntityWakingUp(id) =>
        state.removeSleepingActor(id)
      case MobileEntitySleeping(id, wakeupTime, snapshot) =>
        state.addSleepingActor(id, wakeupTime, snapshot)
        
      // AT LEAST ONCE
      /*case PersistDeliveryId(deliveryId) =>
        state.deliveryId = deliveryId*/
        
      case BusStopEvent(event) =>
        BusStop.eventHandler(event, state)
      case CrossroadEvent(event) =>
        Crossroad.eventHandler(event, state)
      case LaneEvent(event) =>
        Lane.eventHandler(event, state)
      case PedestrianCrossroadEvent(event) =>
        PedestrianCrossroad.eventHandler(event, state)
      case RoadEvent(event) =>
        Road.eventHandler(event, state)
      case TramStopEvent(event) =>
        TramStop.eventHandler(event, state)
      case ZoneEvent(event) =>
        Zone.eventHandler(event, state)
    }
    
    case RecoveryCompleted =>
      // la recoverycompleted è eseguita sia all'avvio dell'attore sia al ripristino
      // dobbiamo distinguere quando siamo ad un avvio da quando siamo ad un ripristino
      // se siamo ad un rispristino, sicuramente state.id != null
      if(state.id != null) {
        // dobbiamo far partire tutti gli attori sotto la nostra gestione
        // non possiamo farlo subito però, potrebbero esservi delle rimozioni di id dalla lista di entità gestite in coda dei messaggi
        sendToImmovable(state.id, state.id, ReCreateMobileEntities)
        // per le entità nodo, ripristina la tabella dei vehicleFree
        for(entry <- state.vehicleFreeMap) {
          vehicleFreeTempMap = vehicleFreeTempMap + (entry._1 -> entry._2)
        }
        // in caso di crossroad, genera la configurazione
        if(state.crossroadData != null) {
          state.crossroadData.category match {
            case `classic` | `semaphore` | `roundabout` =>
              crossroadConfiguration = getCrossroadConfiguration(state.crossroadData.id)
              greenLane = crossroadConfiguration.keys.head
            case _ =>
              
          }
        }
      }
      
    case RecoveryFailure(cause) =>
      println("Recovery fallita: " + cause)
      
    case SnapshotOffer(metadata, offeredSnapshot) =>
      offeredSnapshot match {
        case snapshot : ImmovableState.ImmovableStateSnapshot =>
          state.setSnapshot(snapshot)
          //setDeliverySnapshot(state.deliveryState)
        case _ =>
          
      }
      
  }
  
  // UTILITY
  // Funzione che permette l'invio di un messaggio ad un attore immobile, effettuando l'enveloping adeguato
  def sendToImmovable(senderId : String, destinationId : String, command : Command) : Unit = {
    deliver(shardRegion.path, deliveryId => {
      ToImmovable(destinationId, ToPersistentMessages.FromImmovable(senderId, Request(deliveryId, command)))
    })
  }
  
  // UTILITY
  // Funzione che permette l'invio di un messaggio ad un attore mobile, effettuando l'enveloping adeguato
  def sendToMovable(senderId : String, destinationRef : ActorRef, command : Command) : Unit = {
    deliver(destinationRef.path, deliveryId => {
      ToMovable(destinationRef, ToPersistentMessages.FromImmovable(senderId, Request(deliveryId, command)))
    })
  }
  
  // UTILITY
  // Funzione che permette l'invio di un messaggio ad un attore non persistente, effettuando l'enveloping adeguato
  def sendToNonPersistent(senderId : String, destinationRef : ActorRef, command : Command) : Unit = {
    destinationRef ! ToNonPersistent(destinationRef, ToNonPersistentMessages.FromImmovable(senderId, command))
  }
  
  // UTILITY
  // Ritrona l'indirizzo IP della macchina su cui l'attore risiede
  def getIp() : String = {
    context.system.settings.config.getString("akka.remote.netty.tcp.hostname")
  }
  
}