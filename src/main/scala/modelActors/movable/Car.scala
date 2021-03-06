package modelActors.movable

import akka.actor.ActorRef

import map.Domain._
import modelActors.Messages._
import map.PointsSequence._
import MovableState._
import common.CommonMessages._
import MovableActor.VelocityTick

/**
 * @author pocia
 */
object Car {
  
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
          case LaneAccessGranted(predecessorId, predecessorRef, successorId, successorRef) =>
            // ATTENZIONE: il metodo non garantisce fault tolerance completa
            // potrebbe avvenire che beginOfTheStep sia reso persistente, ma per qualche motivo uno od entrambi dei messaggi
            // inviati al predecessore e al successore vengano persi prima di un crash
            // in questo caso, potrebbe essere che, al ripristino, il predecessor e successor si comportino ignorando il veicolo in mezzo
            // myRef.persistAsync(CarEvent(NextVehicleIdArrived(successorId))) { evt => }
            // persist body begin
            myRef.state.nextVehicleId = successorId
            // persist body end
            myRef.nextVehicle = successorRef
            if(successorId != null) {
              myRef.nextVehicleLastPosition = point(-1, -1)
            }
            else {
              myRef.nextVehicleLastPosition = null
            }
            // myRef.persistAsync(PreviousVehicleIdArrived(predecessorId)) { evt => }
            // persist body begin
            myRef.state.previousVehicleId = predecessorId
            // persist body end
            myRef.previousVehicle = predecessorRef
            // manda gli aggiornamenti a loro, qualora fossero non nulli
            if(predecessorId != null && predecessorRef != null) {
              myRef.sendToMovable(myId, myRef.self, predecessorRef, envelope(myId, predecessorId, PredecessorArrived(myRef.state.getCurrentStepId)))
            }
            if(successorId != null && successorRef != null) {
              myRef.sendToMovable(myId, myRef.self, successorRef, envelope(myId, successorId, SuccessorArrived(myRef.state.getCurrentStepId)))
            }
            // qualora il predecessor fosse null, significa che siamo gli ultimi della lane
            if(predecessorId == null) {
              myRef.sendToImmovable(myId, myRef.self, senderId, envelope(myId, senderId, LastOfTheLane))
            }
            // recupera il percorso
            val stepSequence = myRef.state.getStepSequence()
            if(myRef.state.beginOfTheStep) {
              // recupera la sequenza di punti da percorrere
              val currentPointsSequence = getPointsSequence(myId, stepSequence)
              // myRef.persistAsync(BeginOfTheStep(currentPointsSequence)) { evt => }
              // persist body begin
              myRef.state.currentPointsSequence = currentPointsSequence
              myRef.state.currentPointIndex = 0
              myRef.state.previousPointIndex = -1
              myRef.state.beginOfTheStep = false
              // persist body end
            }
            // attiva l'interessamento agli eventi di avanzamento
            // lo facciamo a prescindere, perchè vogliamo inviare quantomeno la nostra posizione iniziale alla lane e all'eventuale predecessore
            // dunque un giro di velocity tick ci è necessario
            myRef.interestedInVelocityTick = true
            // myRef.sendToMovable(myId, myRef.self, myRef.self, VelocityTick)
            myRef.self ! VelocityTick
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
    }
  }
  
}