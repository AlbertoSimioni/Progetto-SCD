package modelActors.movable

import akka.actor.ActorRef

import modelActors.Messages._

/**
 * @author pocia
 */
object Pedestrian {
  
  // commands
  
  // events

  def fromImmovableHandler(myRef : MovableActor, myId : String, senderId : String, command : RealCommand) : Unit = {
    command match {
      case FromBusStop(message) =>
        
      case FromCrossroad(message) =>
        
      case FromLane(message) =>
        
      case FromPedestrianCrossroad(message) =>
        message match {
          case Cross_Out =>
            // possiamo procedere nell'avanzamento
            myRef.interestedInVelocityTick = true
        }
      case FromRoad(message) =>
        
      case FromTramStop(message) =>
        
      case FromZone(message) =>
        
    }
  }
  
  def fromMovableHandler(myRef : MovableActor, myId : String, senderId : String, senderRef : ActorRef, command : RealCommand) : Unit = {
    command match {
      case FromPedestrian(message) =>
        
      case FromCar(message) =>
        
      case FromBus(message) =>
        
      case FromTram(message) =>
        
    }
  }
  
  def eventHandler(event : Any, state : MovableState) : Unit = {
    //
  }
  
}