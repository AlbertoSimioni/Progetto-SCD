package modelActors.movable

import akka.actor.ActorRef

import modelActors.Messages._
import map.PointsSequence._
import MovableState._
import common.CommonMessages._
import MovableActor.VelocityTick

/**
 * @author pocia
 */
object Tram {
  
  // capacità di trasporto di un tram
  val capacity = 40
  
  // commands
  
  // events

  def fromImmovableHandler(myRef : MovableActor, myId : String, senderId : String, command : RealCommand) : Unit = {
    command match {
      case FromBusStop(message) =>
        message match {
          case Vehicle_Out =>
            // possiamo procedere nell'avanzamento
            myRef.interestedInVelocityTick = true
            // myRef.sendToMovable(myId, myRef.self, myRef.self, VelocityTick)
            myRef.self ! VelocityTick
        }
      case FromCrossroad(message) =>
        message match {
          case Vehicle_Out =>
            // possiamo procedere nell'avanzamento
            myRef.interestedInVelocityTick = true
            // myRef.sendToMovable(myId, myRef.self, myRef.self, VelocityTick)
            myRef.self ! VelocityTick
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
            // myRef.sendToMovable(myId, myRef.self, myRef.self, VelocityTick)
            myRef.self ! VelocityTick
        }
      case FromRoad(message) =>
        
      case FromTramStop(message) =>
        message match {
          case Vehicle_Out =>
            // possiamo procedere nell'avanzamento
            myRef.interestedInVelocityTick = true
            // myRef.sendToMovable(myId, myRef.self, myRef.self, VelocityTick)
            myRef.self ! VelocityTick
          case GetIn(goingOn) =>
            // per prima cosa, rendi persistente l'arrivo dei passeggeri
            myRef.persist(TramEvent(TravellersGoneOn(goingOn))) { evt => }
            // persist body begin
            for(tuple <- goingOn) {
              myRef.state.travellers = myRef.state.travellers + (tuple._1 -> tuple._2)
            }
            // persist body end
            // procedi con la seconda parte del percorso
            myRef.pathPhase = myRef.pathPhase + 1
            myRef.currentNonPersistentPointIndex = 0
            myRef.interestedInVelocityTick = true
            // myRef.sendToMovable(myId, myRef.self, myRef.self, VelocityTick)
            myRef.self ! VelocityTick
        }
      case FromZone(message) =>
        
    }
  }
  
  def fromMovableHandler(myRef : MovableActor, myId : String, senderId : String, senderRef : ActorRef, command : RealCommand) : Unit = {
    command match {
      case FromPedestrian(message) =>
        
      case FromCar(message) =>
        message match {
          case SuccessorArrived(laneId) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case PredecessorArrived(laneId) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case Advanced(laneId, lastPosition) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case PredecessorGone(laneId) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case SuccessorGone(laneId) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case PredecessorChanged(laneId, predecessorId, predecessorRef) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case SuccessorChanged(laneId, successorId, successorRef) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
        }
      case FromBus(message) =>
        message match {
          case SuccessorArrived(laneId) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case PredecessorArrived(laneId) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case Advanced(laneId, lastPosition) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case PredecessorGone(laneId) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case SuccessorGone(laneId) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case PredecessorChanged(laneId, predecessorId, predecessorRef) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case SuccessorChanged(laneId, successorId, successorRef) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
        }
      case FromTram(message) =>
        message match {
          case SuccessorArrived(laneId) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case PredecessorArrived(laneId) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case Advanced(laneId, lastPosition) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case PredecessorGone(laneId) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case SuccessorGone(laneId) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case PredecessorChanged(laneId, predecessorId, predecessorRef) =>
            Vehicle.FromVehicle(myRef, myId, senderId, senderRef, message)
          case SuccessorChanged(laneId, successorId, successorRef) =>
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
      case PreviousVehicleGone =>
        state.previousVehicleId = null
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