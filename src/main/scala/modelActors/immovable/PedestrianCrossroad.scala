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
            if(myRef.vehicle_pass == false) {
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
                myRef.vehicle_pass = true
                for(entry <- myRef.vehicleRequests) {
                  myRef.sendToMovable(myId, entry._2._2, envelope(myId, entry._2._1, Vehicle_Out))
                }
                myRef.vehicleRequests = myRef.vehicleRequests.empty
              }
            }
        }
      case FromCar(message) =>
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
              myRef.vehicleRequests = myRef.vehicleRequests + (comingFrom -> (senderId, senderRef))
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
            myRef.persist(PedestrianCrossroadEvent(VehicleBusyArrived(comingFrom))) { evt =>
              if(myRef.state.vehicleFreeMap.contains(comingFrom)) {
                myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap.updated(comingFrom, false)
              }
              else {
                myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap + (comingFrom -> false)
              }
            }
          case VehicleFree(comingFrom) =>
            // metti a true la entry nella tabella temporanea
            if(myRef.vehicleFreeTempMap.contains(comingFrom)) {
              myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(comingFrom, true)
            }
            else {
              myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (comingFrom -> true)
            }
            // rendi persistente il cambiamento
            myRef.persist(PedestrianCrossroadEvent(VehicleFreeArrived(comingFrom))) { evt =>
              if(myRef.state.vehicleFreeMap.contains(comingFrom)) {
                myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap.updated(comingFrom, true)
              }
              else {
                myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap + (comingFrom -> true)
              }
            }
            // controlla se vi sono richieste di pedoni
            if(myRef.pedestrianRequests.size > 0) {
              myRef.vehicle_pass = false
              for(entry <- myRef.pedestrianRequests) {
                myRef.numPedestrianCrossing = myRef.numPedestrianCrossing + 1
                myRef.sendToMovable(myId, entry._2, envelope(myId, entry._1, Cross_Out))
              }
              myRef.pedestrianRequests = myRef.pedestrianRequests.empty
            }
            else {
              // controlla se c'è una richiesta relativa alla corsia
              if(myRef.vehicleRequests.contains(comingFrom)) {
                myRef.vehicle_pass = true
                val entry = myRef.vehicleRequests.get(comingFrom).get
                myRef.sendToMovable(myId, entry._2, envelope(myId, entry._1, Vehicle_Out))
                myRef.vehicleRequests = myRef.vehicleRequests - comingFrom
              }
            }
        }
      case FromBus(message) =>
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
              myRef.vehicleRequests = myRef.vehicleRequests + (comingFrom -> (senderId, senderRef))
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
            myRef.persist(PedestrianCrossroadEvent(VehicleBusyArrived(comingFrom))) { evt =>
              if(myRef.state.vehicleFreeMap.contains(comingFrom)) {
                myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap.updated(comingFrom, false)
              }
              else {
                myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap + (comingFrom -> false)
              }
            }
          case VehicleFree(comingFrom) =>
            // metti a true la entry nella tabella temporanea
            if(myRef.vehicleFreeTempMap.contains(comingFrom)) {
              myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(comingFrom, true)
            }
            else {
              myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (comingFrom -> true)
            }
            // rendi persistente il cambiamento
            myRef.persist(PedestrianCrossroadEvent(VehicleFreeArrived(comingFrom))) { evt =>
              if(myRef.state.vehicleFreeMap.contains(comingFrom)) {
                myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap.updated(comingFrom, true)
              }
              else {
                myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap + (comingFrom -> true)
              }
            }
            // controlla se vi sono richieste di pedoni
            if(myRef.pedestrianRequests.size > 0) {
              myRef.vehicle_pass = false
              for(entry <- myRef.pedestrianRequests) {
                myRef.numPedestrianCrossing = myRef.numPedestrianCrossing + 1
                myRef.sendToMovable(myId, entry._2, envelope(myId, entry._1, Cross_Out))
              }
              myRef.pedestrianRequests = myRef.pedestrianRequests.empty
            }
            else {
              // controlla se c'è una richiesta relativa alla corsia
              if(myRef.vehicleRequests.contains(comingFrom)) {
                myRef.vehicle_pass = true
                val entry = myRef.vehicleRequests.get(comingFrom).get
                myRef.sendToMovable(myId, entry._2, envelope(myId, entry._1, Vehicle_Out))
                myRef.vehicleRequests = myRef.vehicleRequests - comingFrom
              }
            }
        }
      case FromTram(message) =>
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
              myRef.vehicleRequests = myRef.vehicleRequests + (comingFrom -> (senderId, senderRef))
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
            myRef.persist(PedestrianCrossroadEvent(VehicleBusyArrived(comingFrom))) { evt =>
              if(myRef.state.vehicleFreeMap.contains(comingFrom)) {
                myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap.updated(comingFrom, false)
              }
              else {
                myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap + (comingFrom -> false)
              }
            }
          case VehicleFree(comingFrom) =>
            // metti a true la entry nella tabella temporanea
            if(myRef.vehicleFreeTempMap.contains(comingFrom)) {
              myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap.updated(comingFrom, true)
            }
            else {
              myRef.vehicleFreeTempMap = myRef.vehicleFreeTempMap + (comingFrom -> true)
            }
            // rendi persistente il cambiamento
            myRef.persist(PedestrianCrossroadEvent(VehicleFreeArrived(comingFrom))) { evt =>
              if(myRef.state.vehicleFreeMap.contains(comingFrom)) {
                myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap.updated(comingFrom, true)
              }
              else {
                myRef.state.vehicleFreeMap = myRef.state.vehicleFreeMap + (comingFrom -> true)
              }
            }
            // controlla se vi sono richieste di pedoni
            if(myRef.pedestrianRequests.size > 0) {
              myRef.vehicle_pass = false
              for(entry <- myRef.pedestrianRequests) {
                myRef.numPedestrianCrossing = myRef.numPedestrianCrossing + 1
                myRef.sendToMovable(myId, entry._2, envelope(myId, entry._1, Cross_Out))
              }
              myRef.pedestrianRequests = myRef.pedestrianRequests.empty
            }
            else {
              // controlla se c'è una richiesta relativa alla corsia
              if(myRef.vehicleRequests.contains(comingFrom)) {
                myRef.vehicle_pass = true
                val entry = myRef.vehicleRequests.get(comingFrom).get
                myRef.sendToMovable(myId, entry._2, envelope(myId, entry._1, Vehicle_Out))
                myRef.vehicleRequests = myRef.vehicleRequests - comingFrom
              }
            }
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
  
}