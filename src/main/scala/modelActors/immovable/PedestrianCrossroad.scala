package modelActors.immovable

import akka.actor.ActorRef

import modelActors.Messages._

object PedestrianCrossroad {
  
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
          case Cross_In =>
            if(myRef.numVehicleCrossing == 0) {
              // siamo in pedestrian_pass, soddisfa immediatamente la richiesta
              myRef.numPedestrianCrossing = myRef.numPedestrianCrossing + 1
              myRef.sendToMovable(myId, senderRef, envelope(myId, senderId, Cross_Out))
            }
            else {
              // siamo in vehicle_pass, accoda
              myRef.pedestrianRequests = myRef.pedestrianRequests + (senderId -> senderRef)
            }
          case CrossFree =>
            myRef.numPedestrianCrossing = myRef.numPedestrianCrossing - 1
            if(myRef.numPedestrianCrossing == 0) {
              if(myRef.vehicleRequests.size > 0) {
                // vi sono richieste di veicoli in sospeso
                for(entry <- myRef.vehicleRequests) {
                  myRef.numVehicleCrossing = myRef.numVehicleCrossing + 1
                  myRef.sendToMovable(myId, entry._2._2, envelope(myId, entry._2._1, Vehicle_Out))
                }
                myRef.vehicleRequests = myRef.vehicleRequests.empty
              }
            }
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
    }
  }
  
  // UTILITY
  // modella la risposta ad un generico veicolo
  def FromVehicle(myRef : ImmovableActor, myId : String, senderId : String, senderRef : ActorRef, message : Any) : Unit = {
    message match {
      case Vehicle_In(comingFrom, goingTo) =>
        if((myRef.pedestrianRequests.size == 0 && myRef.numPedestrianCrossing == 0) && myRef.vehicleFreeTempMap.get(comingFrom).getOrElse(true)) {
          myRef.numVehicleCrossing = myRef.numVehicleCrossing + 1
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
      case VehicleBusy(comingFrom, goingTo) =>
        // per sicurezza, metti a false anche la entry nella tabella temporanea
        if(myRef.vehicleFreeTempMap.contains(comingFrom)) {
          myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(comingFrom, false)
        }
        else {
          myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (comingFrom -> false)
        }
        // rendi persistente il cambiamento
        // myRef.persistAsync(PedestrianCrossroadEvent(VehicleBusyArrived(comingFrom))) { evt => }
        // persist body begin
        if(myRef.state.vehicleFreeMap.contains(comingFrom)) {
          myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap.updated(comingFrom, false)
        }
        else {
          myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap + (comingFrom -> false)
        }
        // persist body end
      case VehicleFree(comingFrom, goingTo) =>
        myRef.numVehicleCrossing = myRef.numVehicleCrossing - 1
        // metti a true la entry nella tabella temporanea
        if(myRef.vehicleFreeTempMap.contains(comingFrom)) {
          myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(comingFrom, true)
        }
        else {
          myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (comingFrom -> true)
        }
        // rendi persistente il cambiamento
        // myRef.persistAsync(PedestrianCrossroadEvent(VehicleFreeArrived(comingFrom))) { evt => }
        // persist body begin
        if(myRef.state.vehicleFreeMap.contains(comingFrom)) {
          myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap.updated(comingFrom, true)
        }
        else {
          myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap + (comingFrom -> true)
        }
        // persist body end
        // controlla se vi sono richieste di pedoni
        if(myRef.pedestrianRequests.size > 0) {
          if(myRef.numVehicleCrossing == 0) {
            for(entry <- myRef.pedestrianRequests) {
              myRef.numPedestrianCrossing = myRef.numPedestrianCrossing + 1
              myRef.sendToMovable(myId, entry._2, envelope(myId, entry._1, Cross_Out))
            }
            myRef.pedestrianRequests = myRef.pedestrianRequests.empty
          }
        }
        else {
          // controlla se c'è una richiesta relativa alla corsia
          if(myRef.vehicleRequests.contains(comingFrom)) {
            val entry = myRef.vehicleRequests.get(comingFrom).get
            myRef.numVehicleCrossing = myRef.numVehicleCrossing + 1
            myRef.sendToMovable(myId, entry._2, envelope(myId, entry._1, Vehicle_Out))
            myRef.vehicleRequests = myRef.vehicleRequests - comingFrom
          }
        }
    }
  }
  
}