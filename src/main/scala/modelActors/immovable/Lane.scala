package modelActors.immovable

import akka.actor.ActorRef

import modelActors.Messages._

/*
 * 
 */
object Lane {
  
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
        
      case FromCar(message) =>
        message match {
          case NextVehicleFirstRequest =>
            // restituisci il riferimento all'ultimo veicolo entrato
            myRef.sendToMovable(myId, senderRef, envelope(myId, senderId, NextVehicleResponse(myRef.lastVehicleEnteredId, myRef.lastVehicleEntered)))
            // aggiorna i tuoi campi, visto che un nuovo veicolo è entrato
            myRef.lastVehicleEnteredId = senderId
            myRef.lastVehicleEntered = senderRef
          case NextVehicleRequest(id, last) =>
            if(last == true) {
              // è l'ultimo veicolo nella lane
              myRef.lastVehicleEnteredId = senderId
              myRef.lastVehicleEntered = senderRef
            }
            // restituisci il riferimento al veicolo da tabella
            val correspondingRef = myRef.handledMobileEntitiesMap.get(id).getOrElse(null)
            if(correspondingRef == null) {
              myRef.sendToMovable(myId, senderRef, envelope(myId, senderId, NextVehicleResponse(null, correspondingRef)))
            }
            else {
              myRef.sendToMovable(myId, senderRef, envelope(myId, senderId, NextVehicleResponse(id, correspondingRef)))
            }
        }
      case FromBus(message) =>
        message match {
          case NextVehicleFirstRequest =>
            // restituisci il riferimento all'ultimo veicolo entrato
            myRef.sendToMovable(myId, senderRef, envelope(myId, senderId, NextVehicleResponse(myRef.lastVehicleEnteredId, myRef.lastVehicleEntered)))
          case NextVehicleRequest(id, last) =>
            if(last == true) {
              // è l'ultimo veicolo nella lane
              myRef.lastVehicleEnteredId = senderId
              myRef.lastVehicleEntered = senderRef
            }
            // restituisci il riferimento al veicolo da tabella
            val correspondingRef = myRef.handledMobileEntitiesMap.get(id).getOrElse(null)
            if(correspondingRef == null) {
              myRef.sendToMovable(myId, senderRef, envelope(myId, senderId, NextVehicleResponse(null, correspondingRef)))
            }
            else {
              myRef.sendToMovable(myId, senderRef, envelope(myId, senderId, NextVehicleResponse(id, correspondingRef)))
            }
        }
      case FromTram(message) =>
        message match {
          case NextVehicleFirstRequest =>
            // restituisci il riferimento all'ultimo veicolo entrato
            myRef.sendToMovable(myId, senderRef, envelope(myId, senderId, NextVehicleResponse(myRef.lastVehicleEnteredId, myRef.lastVehicleEntered)))
          case NextVehicleRequest(id, last) =>
            if(last == true) {
              // è l'ultimo veicolo nella lane
              myRef.lastVehicleEnteredId = senderId
              myRef.lastVehicleEntered = senderRef
            }
            // restituisci il riferimento al veicolo da tabella
            val correspondingRef = myRef.handledMobileEntitiesMap.get(id).getOrElse(null)
            if(correspondingRef == null) {
              myRef.sendToMovable(myId, senderRef, envelope(myId, senderId, NextVehicleResponse(null, correspondingRef)))
            }
            else {
              myRef.sendToMovable(myId, senderRef, envelope(myId, senderId, NextVehicleResponse(id, correspondingRef)))
            }
        } 
    }
  }
  
  def eventHandler(event : Any, state : ImmovableState) : Unit = {
    //
  }
  
}