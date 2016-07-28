package common

import akka.actor.ActorRef

/**
 * @author Matteo Pozza
 * Qui sono definiti messaggi e trait usati da controller e model actors
 */
object CommonMessages {
  
  import ToPersistentMessages._
  import ToNonPersistentMessages._
  
  // trait per un comando generale
  trait Command
  
  // trait per un evento generale
  trait Event
  
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