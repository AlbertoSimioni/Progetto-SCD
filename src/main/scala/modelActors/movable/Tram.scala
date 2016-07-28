package modelActors.movable

import akka.actor.ActorRef

import modelActors.Messages._
import map.PointsSequence._
import MovableState._
import common.CommonMessages._

/**
 * @author pocia
 */
object Tram {
  
  // commands
  
  // events

  def fromImmovableHandler(myRef : MovableActor, myId : String, senderId : String, command : RealCommand) : Unit = {
    command match {
      case FromBusStop(message) =>
        
      case FromCrossroad(message) =>
        
      case FromLane(message) =>
        message match {
          case NextVehicleResponse(id, ref) =>
            // è arrivato il riferimento al prossimo veicolo
            myRef.nextVehicle = ref
            // per prima cosa, salva l'id del next Vehicle
            myRef.persist(CarEvent(NextVehicleIdArrived(id))) { evt => 
              myRef.state.nextVehicleId = id
            }
            // ora è possibile recuperare il percorso, qualora non fosse già stato fatto
            val stepSequence = myRef.state.getStepSequence()
            if(myRef.state.beginOfTheStep) {
              // recupera la sequenza di punti da percorrere
              val currentPointsSequence = getPointsSequence(id, stepSequence)
              myRef.persist(BeginOfTheStep(currentPointsSequence)) { evt =>
                myRef.state.currentPointsSequence = evt.pointsSequence
                myRef.state.currentPointIndex = 0
                myRef.state.beginOfTheStep = false
              }
            }
            // se il riferimento era nullo, allora possiamo procedere
            // altrimenti aspettiamo il primo Advanced
            if(ref == null) {
              // attiva l'interessamento agli eventi di avanzamento
              myRef.interestedInVelocityTick = true
            }
            else {
              // attiva l'aggiornamento della posizione dal veicolo successivo
              myRef.sendToMovable(myId, myRef.self, ref, envelope(myId, id, SuccessorArrived))
            }
        }
      case FromPedestrianCrossroad(message) =>
        message match {
          case Vehicle_Out =>
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
        message match {
          case SuccessorArrived =>
            myRef.persist(PredecessorArrived(senderId)) { evt =>
              myRef.state.previousVehicleId = senderId
            }
            // ogni volta che effettuo uno spostamento, devo notificarlo al predecessore
            myRef.previousVehicle = senderRef
          case Advanced(lastPosition) =>
            // aggiorna la posizione del veicolo davanti
            myRef.nextVehicleLastPosition = lastPosition
            // aggiorna anche il riferimento all'attore, per sicurezza
            myRef.nextVehicle = senderRef
            // se è la prima volta che ricevi questo messaggio, attiva l'interruttore di interessamento ai velocity tick
            if(myRef.interestedInVelocityTick == false) {
              myRef.interestedInVelocityTick = true
            }
          case PredecessorGone =>
            // smetti di seguire gli update del prossimo veicolo
            myRef.persist(TramEvent(NextVehicleGone)) { evt =>
              myRef.state.nextVehicleId = null
            }
            myRef.nextVehicle = null
            myRef.nextVehicleLastPosition = null
        }
      case FromBus(message) =>
        message match {
          case SuccessorArrived =>
            myRef.persist(PredecessorArrived(senderId)) { evt =>
              myRef.state.previousVehicleId = senderId
            }
            // ogni volta che effettuo uno spostamento, devo notificarlo al predecessore
            myRef.previousVehicle = senderRef
          case Advanced(lastPosition) =>
            // aggiorna la posizione del veicolo davanti
            myRef.nextVehicleLastPosition = lastPosition
            // aggiorna anche il riferimento all'attore, per sicurezza
            myRef.nextVehicle = senderRef
            // se è la prima volta che ricevi questo messaggio, attiva l'interruttore di interessamento ai velocity tick
            if(myRef.interestedInVelocityTick == false) {
              myRef.interestedInVelocityTick = true
            }
          case PredecessorGone =>
            // smetti di seguire gli update del prossimo veicolo
            myRef.persist(TramEvent(NextVehicleGone)) { evt =>
              myRef.state.nextVehicleId = null
            }
            myRef.nextVehicle = null
            myRef.nextVehicleLastPosition = null
        }
      case FromTram(message) =>
        message match {
          case SuccessorArrived =>
            myRef.persist(PredecessorArrived(senderId)) { evt =>
              myRef.state.previousVehicleId = senderId
            }
            // ogni volta che effettuo uno spostamento, devo notificarlo al predecessore
            myRef.previousVehicle = senderRef
          case Advanced(lastPosition) =>
            // aggiorna la posizione del veicolo davanti
            myRef.nextVehicleLastPosition = lastPosition
            // aggiorna anche il riferimento all'attore, per sicurezza
            myRef.nextVehicle = senderRef
            // se è la prima volta che ricevi questo messaggio, attiva l'interruttore di interessamento ai velocity tick
            if(myRef.interestedInVelocityTick == false) {
              myRef.interestedInVelocityTick = true
            }
          case PredecessorGone =>
            // smetti di seguire gli update del prossimo veicolo
            myRef.persist(TramEvent(NextVehicleGone)) { evt =>
              myRef.state.nextVehicleId = null
            }
            myRef.nextVehicle = null
            myRef.nextVehicleLastPosition = null
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