import akka.persistence.PersistentActor
import akka.persistence.SaveSnapshotSuccess
import akka.persistence.SaveSnapshotFailure
import akka.persistence.SnapshotOffer
import akka.actor.ActorLogging
import akka.contrib.pattern.ShardRegion
import akka.actor.Props
import akka.actor.ActorRef

object UrbanElement {
  
  // modello per la costruzione di UrbanElementActor
  def props(): Props = Props(new UrbanElementActor)
  
  // interfaccia per i comandi
  trait Command {
    def UrbanElementId : String
    def Vehicle : String
  }
  case class VehicleIn(UrbanElementId : String, Vehicle: String) extends Command
  case class VehicleOut(UrbanElementId : String, Vehicle: String) extends Command
  
  // interfaccia per gli eventi
  trait Event
  case object VehicleEntered extends Event
  case object VehicleExited extends Event
  
  // messaggio per il salvataggio di uno snapshot
  case object SnapshotTime
  
  // richiesta per la chiamata distribuita
  // UrbanElementId serve per la shardRegion (deve sapere dove indirizzare il messaggio)
  // l'actorRef serve per spedire la risposta al mittente
  // senderIp serve per fare il test di località
  //
  // da notare che è possibile teoricamente ottenere l'IP dall'actorRef (ref.path.address.host)
  // ma di fatto sembra esserci qualche problema (restituisce sempre None)
  // così l'IP viene calcolato a partire dal file di configurazione
  case class isRemoteQuest(UrbanElementId : String, sender : ActorRef, senderIp : String)
  // risposta per la chiamata distribuita
  case class isRemoteAnswer(flag : Boolean)
  
  // classe che modella lo stato
  case class State(numberOfVehicles : Int) {
    
    def updated(evt: Event): State = evt match {
      case VehicleEntered => copy(numberOfVehicles = numberOfVehicles + 1)
      case VehicleExited => copy(numberOfVehicles = numberOfVehicles -1)
    }
    
  }
  
  // funzione per il recupero dell'id del destinatario
  val idExtractor: ShardRegion.IdExtractor = {
    case cmd: Command => (cmd.UrbanElementId, cmd)
    case original : isRemoteQuest => (original.UrbanElementId, original)
  }

  // funzione per il recupero dell'id dello shard destinatario
  // così definita sono possibili 100 shard
  // l'unica cosa importante della funzione è essere in grado di ripartire equamente gli attori tra gli shard
  // 
  // la distribuzione degli shard è gestita dalle funzioni
  // 1) allocateshard
  // 2) rebalance
  // che sono definite nella shardallocationstrategy, parametro del metodo start del clustersharding
  //
  // si noti che il calcolo dell'id dello shard è eseguito a partire dall'id dell'identità
  //
  // idea: divisione della mappa in quadranti, ciascun quadrante corrisponde ad uno shard
  // l'id è contraddistinto dalla coordinata del vertice più in basso e più a sinistra
  val shardResolver: ShardRegion.ShardResolver = msg => msg match {
    case cmd: Command => (math.abs(cmd.UrbanElementId.hashCode) % 100).toString
    case original : isRemoteQuest => (math.abs(original.UrbanElementId.hashCode) % 100).toString
    /*
     * Aggiornamento della funzione
     * basta dare
     * 
     * decideShard(UrbanElementId)
     * 
     */
  }
  
  val typeOfEntries : String = "UrbanElementActor"
  
}

class UrbanElementActor extends PersistentActor with ActorLogging {
  
  import UrbanElement._
  
  // necessario per il database
  override def persistenceId : String = self.path.parent.name + "-" + self.path.name
  
  // variabile che modella lo stato
  private var state = State(0)
  
  // variabile che modella il numero di azioni effettuate, non è necessario includerla nello stato
  private var numberOfActions = 0
  
  // funzione equivalente alla receive
  def receiveCommand: Receive = {
    case VehicleIn(_, vehicle) =>
      persist(VehicleEntered) { evt =>
        state = state.updated(evt)
        log.info("Vehicle {} entered, current number: {}", vehicle, state.numberOfVehicles)
      }
      numberOfActions += 1
      if(numberOfActions % 15 == 0) {
        self ! SnapshotTime
      }
    case VehicleOut(_, vehicle) =>
      persist(VehicleExited) { evt =>
        state = state.updated(evt)
        log.info("Vehicle {} exited, current number: {}", vehicle, state.numberOfVehicles)
      }
      numberOfActions += 1
      if(numberOfActions % 15 == 0) {
        self ! SnapshotTime
      }
    case SnapshotTime =>
      saveSnapshot(state)
    case SaveSnapshotSuccess(metadata)         =>
      log.info("Snapshot store successfully")
    case SaveSnapshotFailure(metadata, reason) =>
      log.info("Snapshot store failure")
    case UrbanElement.isRemoteQuest(_, sender, senderIp) =>
      val localIp = context.system.settings.config.getString("akka.remote.netty.tcp.hostname")
      log.info("{} VS {}", senderIp, localIp)
      sender ! isRemoteAnswer(senderIp != localIp)
  }
  
  // funzione eseguita al ripristino
  override def receiveRecover: Receive = {
    case evt : Event =>
      evt match {
        case VehicleEntered =>
          state = state.updated(evt)
          log.info("Recovering an entered vehicle, current number: {}", state.numberOfVehicles)
        case VehicleExited =>
          state = state.updated(evt)
          log.info("Recovering an exited vehicle, current number: {}", state.numberOfVehicles)
      }
    
    case SnapshotOffer(metadata, offeredSnapshot : UrbanElement.State) =>
      state = offeredSnapshot
      log.info("Snapshot recovered!")
  }
  
}