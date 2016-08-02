package modelActors.movable

import akka.actor.ActorRef

import map.Domain._
import modelActors.Messages._
import map.PointsSequence._
import MovableState._
import common.CommonMessages._

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
          case LaneAccessGranted(predecessorId, predecessorRef, successorId, successorRef) =>
            // ATTENZIONE: il metodo non garantisce fault tolerance completa
            // potrebbe avvenire che beginOfTheStep sia reso persistente, ma per qualche motivo uno od entrambi dei messaggi
            // inviati al predecessore e al successore vengano persi prima di un crash
            // in questo caso, potrebbe essere che, al ripristino, il predecessor e successor si comportino ignorando il veicolo in mezzo
            myRef.persist(CarEvent(NextVehicleIdArrived(successorId))) { evt =>
              myRef.state.nextVehicleId = successorId
            }
            myRef.nextVehicle = successorRef
            if(successorId != null) {
              myRef.nextVehicleLastPosition = point(-1, -1)
            }
            else {
              myRef.nextVehicleLastPosition = null
            }
            myRef.persist(PredecessorArrived(predecessorId)) { evt =>
              myRef.state.previousVehicleId = predecessorId
            }
            myRef.previousVehicle = predecessorRef
            // manda gli aggiornamenti a loro, qualora fossero non nulli
            if(predecessorId != null && predecessorRef != null) {
              myRef.sendToMovable(myId, myRef.self, predecessorRef, envelope(myId, predecessorId, PredecessorArrived))
            }
            if(successorId != null && successorRef != null) {
              myRef.sendToMovable(myId, myRef.self, successorRef, envelope(myId, successorId, SuccessorArrived))
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
              myRef.persist(BeginOfTheStep(currentPointsSequence)) { evt =>
                myRef.state.currentPointsSequence = evt.pointsSequence
                myRef.state.currentPointIndex = 0
                myRef.state.beginOfTheStep = false
              }
            }
            // dal momento che hai tutti i riferimenti, attiva l'interessamento agli eventi di avanzamento
            myRef.interestedInVelocityTick = true
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
    }
  }
  
}