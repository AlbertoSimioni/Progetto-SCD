package modelActors

import akka.actor.ActorRef
import akka.actor.ActorPath

import map.Routes._

/**
 * @author Matteo Pozza
 * Definisce tutti i messaggi che possono scambiarsi entità mobili ed immobili tra di loro
 */
object CommonMessages {
  
  import ToPersistentMessages._
  import ToNonPersistentMessages._
  
  // dentro all'appropriato handler, definisce lo specifico comportamento
  trait RealCommand
  case class FromBusStop(message : Any) extends RealCommand
  case class FromCrossroad(message : Any) extends RealCommand
  case class FromLane(message : Any) extends RealCommand
  case class FromPedestrianCrossroad(message : Any) extends RealCommand
  case class FromRoad(message : Any) extends RealCommand
  case class FromTramStop(message : Any) extends RealCommand
  case class FromZone(message : Any) extends RealCommand
  
  case class FromPedestrian(message : Any) extends RealCommand
  case class FromCar(message : Any) extends RealCommand
  case class FromBus(message : Any) extends RealCommand
  case class FromTram(message : Any) extends RealCommand
  
  // permette la scelta dell'handler appropriato
  trait Command
  case class ToBusStop(command : RealCommand) extends Command
  case class ToCrossroad(command : RealCommand) extends Command
  case class ToLane(command : RealCommand) extends Command
  case class ToPedestrianCrossroad(command : RealCommand) extends Command
  case class ToRoad(command : RealCommand) extends Command
  case class ToTramStop(command : RealCommand) extends Command
  case class ToZone(command : RealCommand) extends Command
  
  case class ToPedestrian(command : RealCommand) extends Command
  case class ToCar(command : RealCommand) extends Command
  case class ToBus(command : RealCommand) extends Command
  case class ToTram(command : RealCommand) extends Command
  
  // primo messaggio, lanciato dall'injector a ciascuna entità immobile
  case class Identity(id : String) extends Command
  // richiesta di indirizzo IP
  case object IpRequest extends Command
  // risposta con indirizzo IP
  case class IpResponse(ipAddress : String) extends Command
  // comando di creazione di una entità mobile
  case class CreateMobileEntity(id : String, route : route) extends Command
  // comando di ri-creazione di un attore mobile
  case class ReCreateMe(id : String) extends Command
  // comando per l'iniezione di un percorso
  case class Route(route : route) extends Command
  // comando per la creazione degli attori mobili sotto la propria gestione
  case object ReCreateMobileEntities extends Command
  // comando lanciato da un attore mobile a se stesso al termine di uno step
  case object PersistAndNextStep extends Command
  // comandi per la gestione della lista delle entità mobili gestite
  case class MobileEntityAdd(id : String) extends Command
  case class MobileEntityRemove(id : String) extends Command
  // comando per l'esecuzione dello step corrente
  case object ExecuteCurrentStep extends Command
  // comando inviato dall'entità immobile alle entità mobili create per far riprendere a loro l'esecuzione dei loro step
  case object ResumeExecution extends Command
  
  // eventi per la persistenza
  trait Event
  // evento per il filtro dei duplicati
  case class NoDuplicate(senderId : String, deliveryId : Long) extends Event
  
  // evento per la definizione dell'entità immobile
  case class IdentityArrived(id : String) extends Event
  // attore mobile aggiunto in gestione
  case class MobileEntityArrived(id : String) extends Event
  // attore mobile rimosso dall gestione
  case class MobileEntityGone(id : String) extends Event
  // evento per l'avanzamento dell'indice del percorso
  case object NextStepEvent extends Event
  // evento per l'arrivo del percorso all'entità mobile
  case class RouteArrived(route : route) extends Event
  
  case class BusStopEvent(event : Any) extends Event
  case class CrossroadEvent(event : Any) extends Event
  case class LaneEvent(event : Any) extends Event
  case class PedestrianCrossroadEvent(event : Any) extends Event
  case class RoadEvent(event : Any) extends Event
  case class TramStopEvent(event : Any) extends Event
  case class ZoneEvent(event : Any) extends Event
  
  case class PedestrianEvent(event : Any) extends Event
  case class CarEvent(event : Any) extends Event
  case class BusEvent(event : Any) extends Event
  case class TramEvent(event : Any) extends Event
  
  // gerarchia di messaggi
  
  case class ToImmovable(destinationId : String, toImmovableMessage : ToPersistentMessage)
  case class ToMovable(destinationRef : ActorRef, toMovableMessage : ToPersistentMessage)
  case class ToNonPersistent(destinationRef : ActorRef, toNonPersistentMessage : ToNonPersistentMessage)
  
  trait Payload
  case class Ack(deliveryId : Long) extends Payload
  case class Request(deliveryId : Long, command : Command) extends Payload
  
}

object ToPersistentMessages {
  
  import CommonMessages._
  
  trait ToPersistentMessage
  case class FromImmovable(senderId : String, payload : Payload) extends ToPersistentMessage
  case class FromMovable(senderId : String, senderRef : ActorRef, payload : Payload) extends ToPersistentMessage
  case class FromNonPersistent(senderRef : ActorRef, command : Command) extends ToPersistentMessage
  
}

object ToNonPersistentMessages {
  
  import CommonMessages._
  
  trait ToNonPersistentMessage
  case class FromImmovable(senderId : String, command : Command) extends ToNonPersistentMessage
  case class FromMovable(senderRef : ActorRef, command : Command) extends ToNonPersistentMessage
  case class FromNonPersistent(senderRef : ActorRef, command : Command) extends ToNonPersistentMessage
  
}
