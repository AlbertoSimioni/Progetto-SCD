package modelActors.immovable

import scala.concurrent.duration._

import akka.actor.ActorRef

import modelActors.Messages._
import modelActors.movable.MovableActor
import modelActors.movable.Bus
import pubsub.Messages._
import modelActors.movable.MovableState.MovableStateSnapshot

object BusStop {
  
  // messages
  case class CompleteTravellersHandling(numTravellers : Int, busId : String, busRef : ActorRef)
  
  // events
  
  def fromImmovableHandler(myRef : ImmovableActor, myId : String, senderId : String, command : RealCommand) : Unit = {
    command match {
      case FromBusStop(message) =>
        message match {
          case CompleteTravellersHandling(numTravellers, busId, busRef) =>
            // seleziona i pedoni che devono salire
            val placesAvailable = Bus.capacity - numTravellers
            var goingOn = List[(String, (String, MovableStateSnapshot))]()
            if(myRef.travellersQueue.length <= placesAvailable) {
              // prendili tutti
              goingOn = myRef.travellersQueue
              myRef.travellersQueue = List[(String, (String, MovableStateSnapshot))]()
            }
            else {
              // prendi i primi placesAvailable
              goingOn = myRef.travellersQueue.slice(0,placesAvailable)
              myRef.travellersQueue = myRef.travellersQueue.slice(placesAvailable, myRef.travellersQueue.length)
            }
            // rendi persistente la rimozione
            // myRef.persistAsync(BusStopEvent(TravellersGoneOn(goingOn))) { evt => }
            // persist body begin
            for(traveller <- goingOn) {
              myRef.state.handledMobileEntities = myRef.state.handledMobileEntities.filter { current => current != traveller._1 }
            }
            // persist body end
            for(traveller <- goingOn) {
              myRef.handledMobileEntitiesMap = myRef.handledMobileEntitiesMap - traveller._1
              // genera gli eventi hide
              myRef.publisherGuiHandler ! hidePedestrian(traveller._1, "", true)
            }
            myRef.sendToMovable(myId, busRef, envelope(myId, busId, GetIn(goingOn)))
        }
        
      case FromCrossroad(message) =>
        
      case FromLane(message) =>
        
      case FromPedestrianCrossroad(message) =>
        
      case FromRoad(message) =>
        
      case FromTramStop(message) =>
        
      case FromZone(message) =>
        
    }
  }
  
  def fromMovableHandler(myRef : ImmovableActor, myId : String, senderId : String, senderRef : ActorRef, command : RealCommand) : Unit = {
    command match {
      case FromPedestrian(message) =>
        message match {
          case WaitForPublicTransport(nextStop, snapshot) =>
            // aggiungi il pedone nella coda non persistente
            val tuple = (senderId, (nextStop, snapshot))
            myRef.travellersQueue = myRef.travellersQueue :+ tuple
        }
        
      case FromCar(message) =>
        message match {
          case Vehicle_In(comingFrom, goingTo) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case VehicleBusy(comingFrom, goingTo) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case VehicleFree(comingFrom, goingTo) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
        }
      case FromBus(message) =>
        message match {
          case Vehicle_In(comingFrom, goingTo) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case VehicleBusy(comingFrom, goingTo) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case VehicleFree(comingFrom, goingTo) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case GetOut(travellers, numTravellers) =>
            // rendi subito persistente l'arrivo dei viaggiatori
            // myRef.persistAsync(BusStopEvent(TravellersGoneOff(travellers))) { evt => }
            // persist body begin
            for(traveller <- travellers) {
              myRef.state.handledMobileEntities = myRef.state.handledMobileEntities :+ traveller._1
            }
            // persist body end
            // fai ripartire gli attori
            for(traveller <- travellers) {
              val travellerRef = myRef.context.actorOf(MovableActor.props(traveller._1))
              // offri lo snapshot
              myRef.sendToMovable(myId, travellerRef, MovableStateSnapshotOffer(traveller._2))
              // fai partire ciascuna entità
              myRef.sendToMovable(myId, travellerRef, ResumeExecution)
              // aggiungi una entry alla tabella 
              myRef.handledMobileEntitiesMap = myRef.handledMobileEntitiesMap + (traveller._1 -> travellerRef)
            }
            // prima di far salire i passeggeri, aspettiamo un pò
            import myRef.context._
            val cancellable = myRef.context.system.scheduler.scheduleOnce(Duration(1500, "millis"), myRef.self, envelope(myId, myId, CompleteTravellersHandling(numTravellers, senderId, senderRef)))
        }
      case FromTram(message) =>
        message match {
          case Vehicle_In(comingFrom, goingTo) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case VehicleBusy(comingFrom, goingTo) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case VehicleFree(comingFrom, goingTo) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
        }
    }
  }
  
  def eventHandler(event : Any, state : ImmovableState) : Unit = {
    event match {
      case VehicleBusyArrived(comingFrom) =>
        if(state.vehicleFreeMap.contains(comingFrom)) {
          state.vehicleFreeMap = state.vehicleFreeMap.updated(comingFrom, false)
        }
        else {
          state.vehicleFreeMap = state.vehicleFreeMap + (comingFrom -> false)
        }
      case VehicleFreeArrived(comingFrom) =>
        if(state.vehicleFreeMap.contains(comingFrom)) {
          state.vehicleFreeMap = state.vehicleFreeMap.updated(comingFrom, true)
        }
        else {
          state.vehicleFreeMap = state.vehicleFreeMap + (comingFrom -> true)
        }
      case TravellersGoneOff(travellers) =>
        for(traveller <- travellers) {
          state.handledMobileEntities = state.handledMobileEntities :+ traveller._1
        }
      case TravellersGoneOn(travellers) =>
        for(traveller <- travellers) {
          state.handledMobileEntities = state.handledMobileEntities.filter { current => current != traveller._1 }
        }
    }
  }
  
  // UTILITY
  // modella la risposta ad un generico veicolo
  def FromVehicle(myRef : ImmovableActor, myId : String, senderId : String, senderRef : ActorRef, message : Any) : Unit = {
    message match {
      case Vehicle_In(comingFrom, goingTo) =>
        if(myRef.vehicleFreeTempMap.get(comingFrom).getOrElse(true)) {
          // poni il corrispondente vehicleFree a false
          if(myRef.vehicleFreeTempMap.contains(comingFrom)) {
            myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(comingFrom, false)
          }
          else {
            myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (comingFrom -> false)
          }
          // fai passare
          myRef.sendToMovable(myId, senderRef, envelope(myId, senderId, Vehicle_Out))
        }
        else {
          var text = myId + ": sto facendo aspettare " + senderId + " nonostante abbia appena fatto richiesta a causa di vehiclefree"
          //println(text)
          // accodamento
          // PRECONDIZIONE: non vi possono essere richieste presenti relative alla stessa lane
          assert(myRef.vehicleRequests.contains(comingFrom) == false)
          val tuple = (senderId, senderRef)
          myRef.vehicleRequests = myRef.vehicleRequests + (comingFrom -> tuple)
        }
      case VehicleBusy(comingFrom, goingTo) =>
        // per sicurezza, metti a false anche la entry nella tabella temporanea
        if(myRef.vehicleFreeTempMap.contains(comingFrom)) {
          myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(comingFrom, false)
        }
        else {
          myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (comingFrom -> false)
        }
        // rendi persistente il cambiamento
        // myRef.persistAsync(BusStopEvent(VehicleBusyArrived(comingFrom))) { evt => }
        // persist body begin
        if(myRef.state.vehicleFreeMap.contains(comingFrom)) {
          myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap.updated(comingFrom, false)
        }
        else {
          myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap + (comingFrom -> false)
        }
        // persist body end
      case VehicleFree(comingFrom, goingTo) =>
        // metti a true la entry nella tabella temporanea
        if(myRef.vehicleFreeTempMap.contains(comingFrom)) {
          myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(comingFrom, true)
        }
        else {
          myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (comingFrom -> true)
        }
        // rendi persistente il cambiamento
        // myRef.persistAsync(BusStopEvent(VehicleFreeArrived(comingFrom))) { evt => }
        // persist body begin
        if(myRef.state.vehicleFreeMap.contains(comingFrom)) {
          myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap.updated(comingFrom, true)
        }
        else {
          myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap + (comingFrom -> true)
        }
        // persist body end
        // controlla se ci sono richieste pendenti dalla corsia
        if(myRef.vehicleRequests.contains(comingFrom)) {
          val entry = myRef.vehicleRequests.get(comingFrom).get
          myRef.vehicleRequests = myRef.vehicleRequests - comingFrom
          // poni il corrispondente vehicleFree a false
          if(myRef.vehicleFreeTempMap.contains(comingFrom)) {
            myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(comingFrom, false)
          }
          else {
            myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (comingFrom -> false)
          }
          myRef.sendToMovable(myId, entry._2, envelope(myId, entry._1, Vehicle_Out))
        }
    }
  }
  
}