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
        myRef.persist(PredecessorArrived(senderId)) { evt =>
          myRef.state.previousVehicleId = senderId
        }
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
        myRef.persist(event) { evt => 
          myRef.state.nextVehicleId = senderId
        }
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
          myRef.persist(event) { evt =>
            myRef.state.nextVehicleId = null
          }
          myRef.nextVehicle = null
          myRef.nextVehicleLastPosition = null
        }
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
        myRef.persist(event) { evt => 
          myRef.state.nextVehicleId = id
        }
        // ora è possibile recuperare il percorso, qualora non fosse già memorizzato
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
        // se il riferimento era nullo, allora possiamo procedere
        // altrimenti aspettiamo il primo Advanced
        if(ref == null) {
          myRef.nextVehicleLastPosition = null
          // attiva l'interessamento agli eventi di avanzamento
          myRef.interestedInVelocityTick = true
        }
        else {
          // posizione dummy
          myRef.nextVehicleLastPosition = point(-1, -1)
          // attiva l'aggiornamento della posizione dal veicolo successivo
          myRef.sendToMovable(myId, myRef.self, ref, envelope(myId, id, SuccessorArrived))
        }
    }
  }
  
}