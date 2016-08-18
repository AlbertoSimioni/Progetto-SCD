package modelActors.immovable

import scala.Long.MinValue

import akka.persistence.AtLeastOnceDelivery.AtLeastOnceDeliverySnapshot

import map.Domain._
import time.TimeMessages._
import modelActors.movable.MovableState.MovableStateSnapshot

/**
 * @author Matteo Pozza
 * Questa classe modella lo stato di un TestActor
 * Deve essere comprensiva di tutti i dati che potrebbero servire ad un qualunque "sottotipo"
 * Ciascun attore utilizzerà poi solo i dati a lui necessari in base al ruolo
 */

object ImmovableState {
  
  case class ImmovableStateSnapshot(
      handledMobileEntities : List[String],
      sleepingActors : Map[String, (TimeValue, MovableStateSnapshot)],
      vehicleFreeMap : Map[String, Boolean],
      vehicleFree : Boolean,
      lastMessages : Map[String, Long],
      deliveryId : Long,
      id : String,
      kind : String,
      bus_stopData : bus_stop,
      crossroadData : crossroad,
      laneData : lane,
      pedestrian_crossroadData : pedestrian_crossroad,
      roadData : road,
      tram_stopData : tram_stop,
      zoneData : zone
  )
  
}

class ImmovableState {
  
  import ImmovableState._
  
  // lista degli identificativi delle entità mobili sotto la propria gestione
  var handledMobileEntities = List[String]()
  
  // TIME
  // tabella dei dormienti
  var sleepingActors = Map[String, (TimeValue, MovableStateSnapshot)]()
  
  def addSleepingActor(id : String, wakeupTime : TimeValue, snapshot : MovableStateSnapshot) : Unit = {
    // PRECONDIZIONE: l'entità che ci chiede di dormire deve essere sotto la nostra gestione
    assert(handledMobileEntities.contains(id))
    val tuple = (wakeupTime, snapshot)
    if(sleepingActors.contains(id)) {
      sleepingActors = sleepingActors.updated(id, tuple)
    }
    else {
      sleepingActors = sleepingActors + (id -> tuple)
    }
  }
  
  def removeSleepingActor(id : String) : Unit = {
    if(sleepingActors.contains(id)) {
      sleepingActors = sleepingActors - (id)
    }
  }
  
  // ritorna la lista di attori che devono essere svegliati perchè è l'ora esatta o sono in ritardo
  def actorsToBeWakenUp(currentTime : TimeValue) : List[(String, MovableStateSnapshot)] = {
    var toBeWakenUp = List[(String, MovableStateSnapshot)]()
    for(pair <- sleepingActors) {
      if(isLate(pair._2._1, currentTime)) {
        val tuple = (pair._1, pair._2._2)
        toBeWakenUp = toBeWakenUp :+ tuple
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
  
  // AT-LEAST-ONCE
  // ultima delivery id utilizzata, per ripristino
  var deliveryId = MinValue
  
  var id : String = null
  var kind : String = null
  
  var bus_stopData : bus_stop = null
  var crossroadData : crossroad = null
  var laneData : lane = null
  var pedestrian_crossroadData : pedestrian_crossroad = null
  var roadData : road = null
  var tram_stopData : tram_stop = null
  var zoneData : zone = null
  
  def getSnapshot() : ImmovableStateSnapshot = {
    val snapshot = ImmovableStateSnapshot(
        handledMobileEntities,
        sleepingActors,
        vehicleFreeMap,
        vehicleFree,
        lastMessages,
        deliveryId,
        id,
        kind,
        bus_stopData,
        crossroadData,
        laneData,
        pedestrian_crossroadData,
        roadData,
        tram_stopData,
        zoneData
    )
    return snapshot
  }
  
  def setSnapshot(snapshot : ImmovableStateSnapshot) : Unit = {
    handledMobileEntities = snapshot.handledMobileEntities
    sleepingActors = snapshot.sleepingActors
    vehicleFreeMap = snapshot.vehicleFreeMap
    vehicleFree = snapshot.vehicleFree
    lastMessages = snapshot.lastMessages
    deliveryId = snapshot.deliveryId
    id = snapshot.id
    kind = snapshot.kind
    bus_stopData = snapshot.bus_stopData
    crossroadData = snapshot.crossroadData
    laneData = snapshot.laneData
    pedestrian_crossroadData = snapshot.pedestrian_crossroadData
    roadData = snapshot.roadData
    tram_stopData = snapshot.tram_stopData
    zoneData = snapshot.zoneData
  }
  
}