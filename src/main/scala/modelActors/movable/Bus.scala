package modelActors.movable

import akka.actor.ActorRef

import modelActors.Messages._
import map.PointsSequence._
import MovableState._
import common.CommonMessages._

/**
 * @author pocia
 */
object Bus {
  
  // capacità di trasporto di un bus
  val capacity = 20
  
  // commands
  
  // events

  def fromImmovableHandler(myRef : MovableActor, myId : String, senderId : String, command : RealCommand) : Unit = {
    command match {
      case FromBusStop(message) =>
        message match {
          case Vehicle_Out =>
            // possiamo procedere nell'avanzamento
            myRef.interestedInVelocityTick = true
          case GetIn(goingOn) =>
            // per prima cosa, rendi persistente l'arrivo dei passeggeri
            myRef.persist(BusEvent(TravellersGoneOn(goingOn))) { evt =>
              for(tuple <- goingOn) {
                myRef.state.travellers = myRef.state.travellers + (tuple._1 -> tuple._2)
              }
            }
            // procedi con la seconda parte del percorso
            myRef.pathPhase = myRef.pathPhase + 1
            myRef.currentNonPersistentPointIndex = 0
            myRef.interestedInVelocityTick = true
        }
      case FromCrossroad(message) =>
        message match {
          case Vehicle_Out =>
            // possiamo procedere nell'avanzamento
            myRef.interestedInVelocityTick = true
        }
      case FromLane(message) =>
        message match {
          case NextVehicleResponse(id, ref) =>
            Vehicle.FromLane(myRef, myId, senderId, message)
        }
        
      case FromPedestrianCrossroad(message) =>
        message match {
          case Vehicle_Out =>
            // possiamo procedere nell'avanzamento
            myRef.interestedInVelocityTick = true
        }
      case FromRoad(message) =>
        
      case FromTramStop(message) =>
        message match {
          case Vehicle_Out =>
            // possiamo procedere nell'avanzamento
            myRef.interestedInVelocityTick = true
        }
      case FromZone(message) =>
        
    }
  }
  
  def fromMovableHandler(myRef : MovableActor, myId : String, senderId : String, senderRef : ActorRef, command : RealCommand) : Unit = {
    command match {
      case FromPedestrian(message) =>
        
      case FromCar(message) =>
        message match {
          case SuccessorArrived =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case PredecessorArrived =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case Advanced(lastPosition) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case PredecessorGone =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
        }
      case FromBus(message) =>
        message match {
          case SuccessorArrived =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case PredecessorArrived =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case Advanced(lastPosition) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case PredecessorGone =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
        }
      case FromTram(message) =>
        message match {
          case SuccessorArrived =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case PredecessorArrived =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case Advanced(lastPosition) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case PredecessorGone =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
        }
        
    }
  }
  
  def eventHandler(event : Any, state : MovableState) : Unit = {
    event match {
      case NextVehicleIdArrived(id) =>
        state.nextVehicleId = id
      case NextVehicleGone =>
        state.nextVehicleId = null
      case TravellersGoneOff(goingOff) =>
        for(traveller <- goingOff) {
          state.travellers = state.travellers - traveller
        }
      case TravellersGoneOn(goingOn) =>
        for(tuple <- goingOn) {
          state.travellers = state.travellers + (tuple._1 -> tuple._2)
        }
    }
  }
  
}