package modelActors.immovable

import akka.actor.ActorRef

import modelActors.CommonMessages.RealCommand

object BusStop {
  
  // real commands
  
  // events
  
  def fromImmovableHandler(myRef : ImmovableActor, myId : String, senderId : String, command : RealCommand) : Unit = {
    //
  }
  
  def fromMovableHandler(myRef : ImmovableActor, myId : String, senderId : String, senderRef : ActorRef, command : RealCommand) : Unit = {
    //
  }
  
  def eventHandler(event : Any, state : ImmovableState) : Unit = {
    //
  }
  
  
}