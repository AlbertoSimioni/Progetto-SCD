package modelActors.immovable

import scala.util.Random

import akka.actor.ActorRef

import modelActors.Messages._
import map.Domain.category._

object Crossroad {
  
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
        state.vehicleFree = false
      case VehicleFreeArrived(comingFrom) =>
        state.vehicleFree = true
    }
  }
  
  // UTILITY
  // modella la risposta ad un generico veicolo
  def FromVehicle(myRef : ImmovableActor, myId : String, senderId : String, senderRef : ActorRef, message : Any) : Unit = {
    message match {
      case Vehicle_In(comingFrom) =>
        if(myRef.state.vehicleFree == true && myRef.vehicleFreeTemp == true) {
          // poni il corrispondente vehicleFree a false
          myRef.vehicleFreeTemp = false
          // logica dell'incrocio per decidere chi deve passare
          val toBeSatisfied = crossroadLogic(myRef)
          if(toBeSatisfied._1 != null && toBeSatisfied._2 != null) {
            myRef.sendToMovable(myId, toBeSatisfied._2, envelope(myId, toBeSatisfied._1, Vehicle_Out))
          }
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
        myRef.vehicleFreeTemp = false
        // rendi persistente il cambiamento
        myRef.persist(CrossroadEvent(VehicleBusyArrived(comingFrom))) { evt => }
         // persist body begin
        myRef.state.vehicleFree = false
        // persist body end
      case VehicleFree(comingFrom) =>
        // metti a true la entry nella tabella temporanea
        myRef.vehicleFreeTemp = true
        // rendi persistente il cambiamento
        myRef.persist(CrossroadEvent(VehicleFreeArrived(comingFrom))) { evt => }
        // persist body begin
        myRef.state.vehicleFree = true
        // persist body end
        // logica dell'incrocio per decidere chi deve passare
        val toBeSatisfied = crossroadLogic(myRef)
        if(toBeSatisfied._1 != null && toBeSatisfied._2 != null) {
          myRef.sendToMovable(myId, toBeSatisfied._2, envelope(myId, toBeSatisfied._1, Vehicle_Out))
        }
    }
  }
  
  // UTILITY
  // Regola cosa fare al cambio semaforo
  def HandleSemaphoreSwitch(myRef : ImmovableActor, myId : String) : Unit = {
    // logica dell'incrocio per decidere chi deve passare
    val toBeSatisfied = crossroadLogic(myRef)
    if(toBeSatisfied._1 != null && toBeSatisfied._2 != null) {
      myRef.sendToMovable(myId, toBeSatisfied._2, envelope(myId, toBeSatisfied._1, Vehicle_Out))
    }
  }
  
  // UTILITY
  // Logica dell'incrocio per decidere se e a chi mandare Vehicle_Out
  // Torna una coppia nulla se nessuno deve ricevere vehicle_out
  // Causa side-effect: qualora venga restituita una coppia, è stata già rimossa dalle vehicleRequests
  def crossroadLogic(myRef : ImmovableActor) : (String, ActorRef) = {
    // prima cosa: capisco che incrocio sono
    myRef.state.crossroadData.category match {
      case `classic` =>
        // se abbiamo delle richieste pendenti da una delle corsie senza obbligo di precedenza, soddisfane una
        val precedenceKeys = myRef.crossroadConfiguration.filter(tuple => tuple._2._3.length == 0).keys
        var requests = List[String]()
        for(precedenceKey <- precedenceKeys) {
           if(myRef.vehicleRequests.contains(precedenceKey)) {
             requests = requests :+ precedenceKey
           }
        }
        if(requests.length > 0) {
          val toBeSatisfied = Random.shuffle(requests).head
          val tuple = myRef.vehicleRequests.get(toBeSatisfied).get
          myRef.vehicleRequests = myRef.vehicleRequests - toBeSatisfied
          return tuple
        }
        else {
          // altrimenti, controlla se vi sono delle richieste pendenti sulla corsia(e) che deve aspettare le altre due
          val noPriorityKeys = myRef.crossroadConfiguration.filter(tuple => tuple._2._3.length != 0).keys
          var requests = List[String]()
          for(noPriorityKey <- noPriorityKeys) {
             if(myRef.vehicleRequests.contains(noPriorityKey)) {
               requests = requests :+ noPriorityKey
             }
          }
          if(requests.length > 0) {
            val toBeSatisfied = Random.shuffle(requests).head
            val tuple = myRef.vehicleRequests.get(toBeSatisfied).get
            myRef.vehicleRequests = myRef.vehicleRequests - toBeSatisfied
            return tuple
          }
          else {
            // nessuna richiesta da soddisfare
            return (null, null)
          }
        }
        
      case `semaphore` =>
        // la logica è molto semplice
        // se la corsia che ha il verde ha una richiesta, soddisfala
        // altrimenti, non soddisfare nessuna richiesta
        if(myRef.vehicleRequests.contains(myRef.greenLane)) {
          val tuple = myRef.vehicleRequests.get(myRef.greenLane).get
          myRef.vehicleRequests = myRef.vehicleRequests - myRef.greenLane
          return tuple
        }
        else {
          return (null, null)
        }
        
      case `roundabout` =>
        // prendi in esame tutte le corsie con richiesta, e identifica quelle che hanno la corsia sinistra libera
        var eligibleLanes = List[String]()
        for(lane <- myRef.crossroadConfiguration) {
          if(myRef.vehicleRequests.contains(lane._1) && !myRef.vehicleRequests.contains(lane._2._3(0))) {
            eligibleLanes = eligibleLanes :+ lane._1
          }
        }
        // se la lista di richieste è maggiore di 1, prendine una a caso
        // se la lista è lunga 1, soddisfa la richiesta
        if(eligibleLanes.length > 0) {
          val toBeSatisfied = Random.shuffle(eligibleLanes).head
          val tuple = myRef.vehicleRequests.get(toBeSatisfied).get
          myRef.vehicleRequests = myRef.vehicleRequests - toBeSatisfied
          return tuple
        }
        else {
          // altrimenti, ci rimangono due scenari possibili:
          // ciascuna corsia ha una richiesta sulla sinistra
          // allora scegli una a caso e soddisfala
          // oppure non vi sono richieste
          // allora ritorna null
          if(myRef.vehicleRequests.isEmpty) {
            return (null, null)
          }
          else {
            val toBeSatisfied = Random.shuffle(myRef.vehicleRequests.keys).head
            val tuple = myRef.vehicleRequests.get(toBeSatisfied).get
            myRef.vehicleRequests = myRef.vehicleRequests - toBeSatisfied
            return tuple
          }
        }
    }
  }
  
}