package modelActors.immovable

import akka.actor.ActorRef

import modelActors.Messages._
import modelActors.movable.MovableActor
import modelActors.movable.Tram
import pubsub.Messages._

object TramStop {
  
  // commands
  
  // events
  
  def fromImmovableHandler(myRef : ImmovableActor, myId : String, senderId : String, command : RealCommand) : Unit = {
    command match {
      case FromBusStop(message) =>
        
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
          case WaitForPublicTransport(nextStop) =>
            // aggiungi il pedone nella coda non persistente
            val tuple = (senderId, nextStop)
            myRef.travellersQueue = myRef.travellersQueue :+ tuple
        }
        
      case FromCar(message) =>
        message match {
          case Vehicle_In(comingFrom) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case VehicleBusy(comingFrom) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case VehicleFree(comingFrom) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
        }
      case FromBus(message) =>
        message match {
          case Vehicle_In(comingFrom) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case VehicleBusy(comingFrom) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case VehicleFree(comingFrom) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case GetOut(travellers, numTravellers) =>
            // rendi subito persistente l'arrivo dei viaggiatori
            myRef.persist(TramStopEvent(TravellersGoneOff(travellers))) { evt => }
            // persist body begin
            for(traveller <- travellers) {
              myRef.state.handledMobileEntities = myRef.state.handledMobileEntities :+ traveller
            }
            // persist body end
            // fai ripartire gli attori
            for(traveller <- travellers) {
              val travellerRef = myRef.context.actorOf(MovableActor.props(traveller))
              // fai partire ciascuna entità
              myRef.sendToMovable(myId, travellerRef, ResumeExecution)
              // aggiungi una entry alla tabella 
              myRef.handledMobileEntitiesMap = myRef.handledMobileEntitiesMap + (traveller -> travellerRef)
            }
            // seleziona i pedoni che devono salire
            val placesAvailable = Tram.capacity - numTravellers
            var goingOn = List[(String, String)]()
            if(myRef.travellersQueue.length <= placesAvailable) {
              // prendili tutti
              goingOn = myRef.travellersQueue
              myRef.travellersQueue = List[(String, String)]()
            }
            else {
              // prendi i primi placesAvailable
              goingOn = myRef.travellersQueue.slice(0,placesAvailable)
              myRef.travellersQueue = myRef.travellersQueue.slice(placesAvailable, myRef.travellersQueue.length)
            }
            // rendi persistente la rimozione
            myRef.persist(TramStopEvent(TravellersGoneOn(goingOn))) { evt => }
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
            myRef.sendToMovable(myId, senderRef, envelope(myId, senderId, GetIn(goingOn)))
        }
      case FromTram(message) =>
        message match {
          case Vehicle_In(comingFrom) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case VehicleBusy(comingFrom) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case VehicleFree(comingFrom) =>
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
          state.handledMobileEntities = state.handledMobileEntities :+ traveller
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
      case Vehicle_In(comingFrom) =>
        if(myRef.vehicle_pass == true && myRef.vehicleFreeTempMap.get(comingFrom).getOrElse(true)) {
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
          // accodamento
          // PRECONDIZIONE: non vi possono essere richieste presenti relative alla stessa lane
          assert(myRef.vehicleRequests.contains(comingFrom) == false)
          val tuple = (senderId, senderRef)
          myRef.vehicleRequests = myRef.vehicleRequests + (comingFrom -> tuple)
        }
      case VehicleBusy(comingFrom) =>
        // per sicurezza, metti a false anche la entry nella tabella temporanea
        if(myRef.vehicleFreeTempMap.contains(comingFrom)) {
          myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(comingFrom, false)
        }
        else {
          myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (comingFrom -> false)
        }
        // rendi persistente il cambiamento
        myRef.persist(TramStopEvent(VehicleBusyArrived(comingFrom))) { evt => }
        // persist body begin
        if(myRef.state.vehicleFreeMap.contains(comingFrom)) {
          myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap.updated(comingFrom, false)
        }
        else {
          myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap + (comingFrom -> false)
        }
        // persist body end
      case VehicleFree(comingFrom) =>
        // metti a true la entry nella tabella temporanea
        if(myRef.vehicleFreeTempMap.contains(comingFrom)) {
          myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(comingFrom, true)
        }
        else {
          myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (comingFrom -> true)
        }
        // rendi persistente il cambiamento
        myRef.persist(TramStopEvent(VehicleFreeArrived(comingFrom))) { evt => }
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
          myRef.sendToMovable(myId, entry._2, envelope(myId, entry._1, Vehicle_Out))
          myRef.vehicleRequests = myRef.vehicleRequests - comingFrom
        }
    }
  }
  
  
}