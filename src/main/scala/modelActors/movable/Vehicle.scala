package modelActors.movable

import akka.actor.ActorRef

import modelActors.Messages._
import map.PointsSequence._
import MovableState._
import common.CommonMessages._
import map.Domain._

/**
 * @author Matteo Pozza
 * Racchiude funzioni comuni a tutti i veicoli
 * 
 */
object Vehicle {
  
  // handling di un messaggio per un veicolo da un veicolo
  def FromVehicle(myRef : MovableActor, myId : String, senderId : String, senderRef : ActorRef, message : Any) : Unit = {
    message match {
      case SuccessorArrived =>
        myRef.persist(PredecessorArrived(senderId)) { evt => }
        // persist body begin
        myRef.state.previousVehicleId = senderId
        // persist body end
        // ogni volta che effettuo uno spostamento, devo notificarlo al predecessore
        myRef.previousVehicle = senderRef
      case PredecessorArrived =>
        var event : Event = null
        if(myRef.getMyLength() == car_length) {
          event = CarEvent(NextVehicleIdArrived(senderId))
        }
        else if(myRef.getMyLength() == bus_length) {
          event = BusEvent(NextVehicleIdArrived(senderId))
        }
        else {
          // tram
          event = TramEvent(NextVehicleIdArrived(senderId))
        }
        myRef.persist(event) { evt => }
        // persist body begin
        myRef.state.nextVehicleId = senderId
        // persist body end
        myRef.nextVehicle = senderRef
        myRef.nextVehicleLastPosition = point(-1, -1)
      case Advanced(lastPosition) =>
        if(senderId == myRef.state.nextVehicleId) {
          // aggiorna la posizione del veicolo davanti
          myRef.nextVehicleLastPosition = lastPosition
          // aggiorna anche il riferimento all'attore, per sicurezza
          myRef.nextVehicle = senderRef
          // se è la prima volta che ricevi questo messaggio, attiva l'interruttore di interessamento ai velocity tick
          if(myRef.interestedInVelocityTick == false) {
            myRef.interestedInVelocityTick = true
          }
        }
      case PredecessorGone =>
        if(senderId == myRef.state.nextVehicleId) {
          // smetti di seguire gli update del prossimo veicolo
          var event : Event = null
          if(myRef.getMyLength() == car_length) {
            event = CarEvent(NextVehicleGone)
          }
          else if(myRef.getMyLength() == bus_length) {
            event = BusEvent(NextVehicleGone)
          }
          else {
            // tram
            event = TramEvent(NextVehicleGone)
          }
          myRef.persist(event) { evt => }
          // persist body begin
          myRef.state.nextVehicleId = null
          // persist body end
          myRef.nextVehicle = null
          myRef.nextVehicleLastPosition = null
        }
      case SuccessorGone =>
        if(senderId == myRef.state.previousVehicleId) {
          // smetti di inviare update della posizione
          var event : Event = null
          if(myRef.getMyLength() == car_length) {
            event = CarEvent(PreviousVehicleGone)
          }
          else if(myRef.getMyLength() == bus_length) {
            event = BusEvent(PreviousVehicleGone)
          }
          else {
            // tram
            event = TramEvent(PreviousVehicleGone)
          }
          myRef.persist(event) { evt => }
          // persist body begin
          myRef.state.previousVehicleId = null
          // persist body end
          myRef.previousVehicle = null
        }
      case PredecessorChanged(predecessorId, predecessorRef) =>
        var event : Event = null
        if(myRef.getMyLength() == car_length) {
          event = CarEvent(NextVehicleIdArrived(predecessorId))
        }
        else if(myRef.getMyLength() == bus_length) {
          event = BusEvent(NextVehicleIdArrived(predecessorId))
        }
        else {
          // tram
          event = TramEvent(NextVehicleIdArrived(predecessorId))
        }
        myRef.persist(event) { evt => }
        // persist body begin
        myRef.state.nextVehicleId = predecessorId
        // persist body end
        myRef.nextVehicle = predecessorRef
        myRef.nextVehicleLastPosition = point(-1, -1)
      case SuccessorChanged(successorId, successorRef) =>
        myRef.persist(PredecessorArrived(successorId)) { evt => }
        // persist body begin
        myRef.state.previousVehicleId = successorId
        // persist body end
        myRef.previousVehicle = successorRef
    }
  }
  
  // handling di un messaggio da una lane
  def FromLane(myRef : MovableActor, myId : String, senderId : String, message : Any) : Unit = {
    message match {
      case NextVehicleResponse(id, ref) =>
        // è arrivato il riferimento al prossimo veicolo
        myRef.nextVehicle = ref
        // per prima cosa, salva l'id del next Vehicle
        var event : Event = null
        if(myRef.getMyLength() == car_length) {
          event = CarEvent(NextVehicleIdArrived(id))
        }
        else if(myRef.getMyLength() == bus_length) {
          event = BusEvent(NextVehicleIdArrived(id))
        }
        else {
          // tram
          event = TramEvent(NextVehicleIdArrived(id))
        }
        myRef.persist(event) { evt =>  }
        // persist body begin
        myRef.state.nextVehicleId = id
        // persist body end
        // ora è possibile recuperare il percorso, qualora non fosse già memorizzato
        val stepSequence = myRef.state.getStepSequence()
        if(myRef.state.beginOfTheStep) {
          // recupera la sequenza di punti da percorrere
          val currentPointsSequence = getPointsSequence(myId, stepSequence)
          myRef.persist(BeginOfTheStep(currentPointsSequence)) { evt => }
          // persist body begin
          myRef.state.currentPointsSequence = currentPointsSequence
          myRef.state.currentPointIndex = 0
          myRef.state.beginOfTheStep = false
          // persist body end
        }
        // se il riferimento era nullo, allora possiamo procedere
        // altrimenti aspettiamo il primo Advanced
        if(ref == null) {
          myRef.nextVehicleLastPosition = null
        }
        else {
          // posizione dummy
          myRef.nextVehicleLastPosition = point(-1, -1)
          // attiva l'aggiornamento della posizione dal veicolo successivo
          myRef.sendToMovable(myId, myRef.self, ref, envelope(myId, id, SuccessorArrived))
        }
        // attiva l'interessamento agli eventi di avanzamento
        myRef.interestedInVelocityTick = true
    }
  }
  
}