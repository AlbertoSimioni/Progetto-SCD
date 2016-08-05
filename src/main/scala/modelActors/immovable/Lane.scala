package modelActors.immovable

import akka.actor.ActorRef

import map.PointsSequence._
import map.Domain.position._
import map.Domain._
import map.Routes._
import modelActors.Messages._

/*
 * 
 */
object Lane {
  
  // cuscinetto di spazio che si vuole tra il predecessor (+ sua lunghezza) e il veicolo da inserire
  // deve essere tale da permettere di evitare il crash tra i veicoli
  val padding = 5
  
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
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case NextVehicleRequest(id, last) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case Advanced(lastPosition) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case HandleLastVehicle =>
            FromVehicle(myRef, myId, senderId, senderRef, message)  
          case LastOfTheLane =>
            FromVehicle(myRef, myId, senderId, senderRef, message)  
          case LaneAccessRequest(startPosition, direction) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
        }
      case FromBus(message) =>
        message match {
          case NextVehicleFirstRequest =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case NextVehicleRequest(id, last) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case Advanced(lastPosition) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case HandleLastVehicle =>
            FromVehicle(myRef, myId, senderId, senderRef, message)  
          case LastOfTheLane =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case LaneAccessRequest(startPosition, direction) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
        }
      case FromTram(message) =>
        message match {
          case NextVehicleFirstRequest =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case NextVehicleRequest(id, last) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case Advanced(lastPosition) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case HandleLastVehicle =>
            FromVehicle(myRef, myId, senderId, senderRef, message)  
          case LastOfTheLane =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
          case LaneAccessRequest(startPosition, direction) =>
            FromVehicle(myRef, myId, senderId, senderRef, message)
        } 
    }
  }
  
  def eventHandler(event : Any, state : ImmovableState) : Unit = {
    //
  }
  
  // UTILITY
  // modella la risposta ad un generico veicolo
  def FromVehicle(myRef : ImmovableActor, myId : String, senderId : String, senderRef : ActorRef, message : Any) : Unit = {
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
      case Advanced(lastPosition) =>
        // dobbiamo aggiungere o aggiornare la posizione nella nostra tabella
        // per prima cosa, l'entità in esame deve essere sotto la nostra gestione
        if(myRef.state.handledMobileEntities.contains(senderId)) {
          if(myRef.positionsMap.contains(senderId)) {
            myRef.positionsMap = myRef.positionsMap.updated(senderId, lastPosition)
          }
          else {
            myRef.positionsMap = myRef.positionsMap + (senderId -> lastPosition)
          }
        }
        // vedi se ci sono delle richieste pendenti da soddisfare
        if(myRef.pendingLaneRequests.size > 0) {
          var satisfied = List[String]()
          for(entry <- myRef.pendingLaneRequests) {
            if(checkEnoughSpace(myRef, entry._2._2, entry._2._3)) {
              // soddisfa immediatamente la richiesta
              val neighbours = getNeighbours(myRef, entry._2._2, entry._2._3)
              val predecessorRef = myRef.handledMobileEntitiesMap.get(neighbours._1).getOrElse(null)
              val successorRef = myRef.handledMobileEntitiesMap.get(neighbours._2).getOrElse(null)
              myRef.sendToMovable(myId, entry._2._1, envelope(myId, entry._1, LaneAccessGranted(neighbours._1, predecessorRef, neighbours._2, successorRef)))
              satisfied = satisfied :+ entry._1
            }
          }
          // rimuovi dalla lista delle richieste pendenti quelle soddisfatte
          for(requestingId <- satisfied) {
            myRef.pendingLaneRequests = myRef.pendingLaneRequests - requestingId
          }
        }
      case HandleLastVehicle =>
        // spedito da un veicolo per avvisarci
        // qualora lui fosse il nostro lastVehicle corrente, bisogna riportarlo a null
        if(myRef.lastVehicleEnteredId == senderId) {
          myRef.lastVehicleEnteredId = null
          myRef.lastVehicleEntered = null
        }
      case LastOfTheLane =>
        // aggiorniamo i nostri campi
        myRef.lastVehicleEnteredId = senderId
        myRef.lastVehicleEntered = senderRef
      case LaneAccessRequest(startPosition, direction) =>
        // ATTENZIONE: il metodo non garantisce fault tolerance completa
        // infatti, la risposta del check si basa sulle informazioni presenti nella tabella positionsMap
        // potrebbe accadere che, al ripristino, le posizioni di tutti i veicoli debbano ancora essere ricevute dalla lane
        // e sulla base di queste parziali informazioni conceda l'accesso ad un veicolo, facendolo collidere con un altro
        if(checkEnoughSpace(myRef, startPosition, direction)) {
          // soddisfa immediatamente la richiesta
          val neighbours = getNeighbours(myRef, startPosition, direction)
          val predecessorRef = myRef.handledMobileEntitiesMap.get(neighbours._1).getOrElse(null)
          val successorRef = myRef.handledMobileEntitiesMap.get(neighbours._2).getOrElse(null)
          myRef.sendToMovable(myId, senderRef, envelope(myId, senderId, LaneAccessGranted(neighbours._1, predecessorRef, neighbours._2, successorRef)))
        }
        else {
          // accoda la richiesta
          val tuple = (senderRef, startPosition, direction)
          myRef.pendingLaneRequests = myRef.pendingLaneRequests + (senderId -> tuple)
        }
    }
  }
  
  // UTILITY
  // controlla se c'è spazio a sufficienza per il nuovo veicolo
  def checkEnoughSpace(myRef : ImmovableActor, startPosition : point, direction : direction) : Boolean = {
    val neighbours = getNeighbours(myRef, startPosition, direction)
    if(neighbours._1 == null && neighbours._2 == null) {
      // nè predecessore nè successore
      return true
    }
    else if(neighbours._1 != null && neighbours._2 == null) {
      // solo predecessore
      val predecessorPoint = myRef.positionsMap.get(neighbours._1).get
      direction.position match {
        case `up` =>
          // coordinata x, decrescente
          if(startPosition.x + getLengthFromId(myRef.state.id) < predecessorPoint.x - padding) {
            return true
          }
          else {
            return false
          }
        case `down` =>
          // coordinata x, crescente
          if(startPosition.x > predecessorPoint.x + getLengthFromId(neighbours._1) + padding) {
            return true
          }
          else {
            return false
          }
        case `left` =>
          // coordinata y, decrescente
          if(startPosition.y + getLengthFromId(myRef.state.id) < predecessorPoint.y - padding) {
            return true
          }
          else {
            return false
          }
        case `right` =>
          // coordinata y, crescente
          if(startPosition.y > predecessorPoint.y + getLengthFromId(neighbours._1) + padding) {
            return true
          }
          else {
            return false
          }
      }
    }
    else if(neighbours._1 == null && neighbours._2 != null) {
      // solo successore
      val successorPoint = myRef.positionsMap.get(neighbours._2).get
      direction.position match {
        case `up` =>
          // coordinata x, decrescente
          if(startPosition.x > successorPoint.x + getLengthFromId(neighbours._2)) {
            return true
          }
          else {
            return false
          }
        case `down` =>
          // coordinata x, crescente
          if(startPosition.x + getLengthFromId(myRef.state.id) < successorPoint.x) {
            return true
          }
          else {
            return false
          }
        case `left` =>
          // coordinata y, decrescente
          if(startPosition.y > successorPoint.y + getLengthFromId(neighbours._2)) {
            return true
          }
          else {
            return false
          }
        case `right` =>
          // coordinata y, crescente
          if(startPosition.y + getLengthFromId(myRef.state.id) < successorPoint.y) {
            return true
          }
          else {
            return false
          }
      }
    }
    else {
      // sia predecessore sia successore
      val predecessorPoint = myRef.positionsMap.get(neighbours._1).get
      val successorPoint = myRef.positionsMap.get(neighbours._2).get
      direction.position match {
        case `up` =>
          // coordinata x, decrescente
          if(startPosition.x + getLengthFromId(myRef.state.id) < predecessorPoint.x - padding && startPosition.x > successorPoint.x + getLengthFromId(neighbours._2)) {
            return true
          }
          else {
            return false
          }
        case `down` =>
          // coordinata x, crescente
          if(startPosition.x > predecessorPoint.x + getLengthFromId(neighbours._1) + padding && startPosition.x + getLengthFromId(myRef.state.id) < successorPoint.x) {
            return true
          }
          else {
            return false
          }
        case `left` =>
          // coordinata y, decrescente
          if(startPosition.y + getLengthFromId(myRef.state.id) < predecessorPoint.y - padding && startPosition.y > successorPoint.y + getLengthFromId(neighbours._2)) {
            return true
          }
          else {
            return false
          }
        case `right` =>
          // coordinata y, crescente
          if(startPosition.y > predecessorPoint.y + getLengthFromId(neighbours._1) + padding && startPosition.y + getLengthFromId(myRef.state.id) < successorPoint.y) {
            return true
          }
          else {
            return false
          }
      }
    }
  }
  
  // UTILITY
  // restituisce gli id del predecessore e del successore, o null se non vi sono
  def getNeighbours(myRef : ImmovableActor, startPosition : point, direction : direction) : (String, String) = {
    // per prima cosa, capisci su quale coordinata dobbiamo concentrare l'attenzione
    direction.position match {
      case `up` =>
        assert(direction.beginToEnd == false)
        // coordinata x, decrescente
        // trova il predecessor
        var predecessor : String = null
        for(entry <- myRef.positionsMap) {
          if(entry._2.x > startPosition.x) {
            // potrebbe essere il predecessore
            if(predecessor != null) {
              if(entry._2.x < myRef.positionsMap.get(predecessor).get.x) {
                // è un veicolo più vicino
                predecessor = entry._1
              }
            }
            else {
              predecessor = entry._1
            }
          }
        }
        // trova il successor
        var successor : String = null
        for(entry <- myRef.positionsMap) {
          if(entry._2.x < startPosition.x) {
            // potrebbe essere il successore
            if(successor != null) {
              if(entry._2.x > myRef.positionsMap.get(successor).get.x) {
                // è un veicolo più vicino
                successor = entry._1
              }
            }
            else {
              successor = entry._1
            }
          }
        }
        return (predecessor, successor)
      case `down` =>
        assert(direction.beginToEnd == true)
        // coordinata x, crescente
        // trova il predecessor
        var predecessor : String = null
        for(entry <- myRef.positionsMap) {
          if(entry._2.x < startPosition.x) {
            // potrebbe essere il predecessore
            if(predecessor != null) {
              if(entry._2.x > myRef.positionsMap.get(predecessor).get.x) {
                // è un veicolo più vicino
                predecessor = entry._1
              }
            }
            else {
              predecessor = entry._1
            }
          }
        }
        // trova il successor
        var successor : String = null
        for(entry <- myRef.positionsMap) {
          if(entry._2.x > startPosition.x) {
            // potrebbe essere il successore
            if(successor != null) {
              if(entry._2.x < myRef.positionsMap.get(successor).get.x) {
                // è un veicolo più vicino
                successor = entry._1
              }
            }
            else {
              successor = entry._1
            }
          }
        }
        return (predecessor, successor)
      case `left` =>
        assert(direction.beginToEnd == false)
        // coordinata y, decrescente
        // trova il predecessor
        var predecessor : String = null
        for(entry <- myRef.positionsMap) {
          if(entry._2.y > startPosition.y) {
            // potrebbe essere il predecessore
            if(predecessor != null) {
              if(entry._2.y < myRef.positionsMap.get(predecessor).get.y) {
                // è un veicolo più vicino
                predecessor = entry._1
              }
            }
            else {
              predecessor = entry._1
            }
          }
        }
        // trova il successor
        var successor : String = null
        for(entry <- myRef.positionsMap) {
          if(entry._2.y < startPosition.y) {
            // potrebbe essere il successore
            if(successor != null) {
              if(entry._2.y > myRef.positionsMap.get(successor).get.y) {
                // è un veicolo più vicino
                successor = entry._1
              }
            }
            else {
              successor = entry._1
            }
          }
        }
        return (predecessor, successor)
      case `right` =>
        assert(direction.beginToEnd == true)
        // coordinata y, crescente
        // trova il predecessor
        var predecessor : String = null
        for(entry <- myRef.positionsMap) {
          if(entry._2.y < startPosition.y) {
            // potrebbe essere il predecessore
            if(predecessor != null) {
              if(entry._2.y > myRef.positionsMap.get(predecessor).get.y) {
                // è un veicolo più vicino
                predecessor = entry._1
              }
            }
            else {
              predecessor = entry._1
            }
          }
        }
        // trova il successor
        var successor : String = null
        for(entry <- myRef.positionsMap) {
          if(entry._2.y > startPosition.y) {
            // potrebbe essere il successore
            if(successor != null) {
              if(entry._2.y < myRef.positionsMap.get(successor).get.y) {
                // è un veicolo più vicino
                successor = entry._1
              }
            }
            else {
              successor = entry._1
            }
          }
        }
        return (predecessor, successor)
    }
  }
  
}