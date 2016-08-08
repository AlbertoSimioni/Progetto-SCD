package modelActors.immovable

import akka.persistence.AtLeastOnceDelivery.AtLeastOnceDeliverySnapshot

import map.Domain._
import time.TimeMessages._

/**
 * @author Matteo Pozza
 * Questa classe modella lo stato di un TestActor
 * Deve essere comprensiva di tutti i dati che potrebbero servire ad un qualunque "sottotipo"
 * Ciascun attore utilizzerà poi solo i dati a lui necessari in base al ruolo
 */

class ImmovableState {
  
  // lista degli identificativi delle entità mobili sotto la propria gestione
  var handledMobileEntities = List[String]()
  
  // TIME
  // tabella dei dormienti
  var sleepingActors = Map[String, TimeValue]()
  
  def addSleepingActor(id : String, wakeupTime : TimeValue) : Unit = {
    // PRECONDIZIONE: l'entità che ci chiede di dormire deve essere sotto la nostra gestione
    assert(handledMobileEntities.contains(id))
    if(sleepingActors.contains(id)) {
      sleepingActors = sleepingActors.updated(id, wakeupTime)
    }
    else {
      sleepingActors = sleepingActors + (id -> wakeupTime)
    }
  }
  
  def removeSleepingActor(id : String) : Unit = {
    if(sleepingActors.contains(id)) {
      sleepingActors = sleepingActors - (id)
    }
  }
  
  // ritorna la lista di attori che devono essere svegliati perchè è l'ora esatta o sono in ritardo
  def actorsToBeWakenUp(currentTime : TimeValue) : List[String] = {
    var toBeWakenUp = List[String]()
    for(pair <- sleepingActors) {
      if(isLate(pair._2, currentTime)) {
        toBeWakenUp = toBeWakenUp :+ pair._1
      }
    }
    return toBeWakenUp
  }
  
  // STRISCE PEDONALI
  var vehicleFreeMap = Map[String, Boolean]()
  
  // CROSSROAD
  var vehicleFree = true
  
  // AT-LEAST-ONCE
  // Stato della at-least-once dell'attore
  //var deliveryState : AtLeastOnceDeliverySnapshot = null
  
  // AT-LEAST-ONCE
  // filtro dei duplicati
  var lastMessages = Map[String, Long]()
  
  // AT-LEAST-ONCE
  // test sul filtro dei duplicati
  def isNewMessage(actorId : String, deliveryId : Long) : Boolean = {
    if(lastMessages.contains(actorId)) {
      if(lastMessages.get(actorId).get >= deliveryId) {
        return false
      }
      else {
        return true
      }
    }
    else {
      return true
    }
  }
  
  // AT-LEAST-ONCE
  // aggiornamento del filtro dei duplicati: se il messaggio è nuovo, aggiorna la mappa
  def updateFilter(actorId : String, deliveryId : Long) : Unit = {
    if(lastMessages.contains(actorId)) {
      lastMessages = lastMessages.updated(actorId, deliveryId)
    }
    else {
      lastMessages = lastMessages + (actorId -> deliveryId)
    }
  }
  
  var id : String = null
  var kind : String = null
  
  var bus_stopData : bus_stop = null
  var crossroadData : crossroad = null
  var laneData : lane = null
  var pedestrian_crossroadData : pedestrian_crossroad = null
  var roadData : road = null
  var tram_stopData : tram_stop = null
  var zoneData : zone = null
  
}