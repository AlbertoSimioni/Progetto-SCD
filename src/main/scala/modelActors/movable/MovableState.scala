package modelActors.movable

import scala.Long.MinValue

import akka.persistence.AtLeastOnceDelivery.AtLeastOnceDeliverySnapshot

import map.Domain._
import map.Routes
import map.Routes._
import time.TimeMessages._

/**
 * @author pocia
 */
object MovableState {
  
  import common.CommonMessages._
  
  case class BeginOfTheStep(pointsSequence : List[List[point]]) extends Event
  case object IncrementPointIndex extends Event
  
  case class MovableStateSnapshot(
      pedestrianRoute : pedestrian_route,
      carRoute : car_route,
      busRoute : bus_route,
      tramRoute : tram_route,
      currentRoute : List[step],
      index : Int,
      beginOfTheStep : Boolean,
      currentPointsSequence : List[List[point]],
      currentPointIndex : Int,
      previousPointIndex : Int,
      currentTime : TimeValue,
      nextVehicleId : String,
      previousVehicleId : String,
      predecessorGoneSent : Boolean,
      travellers : Map[String, (String, MovableStateSnapshot)],
      previousLaneId : String,
      alreadyHidden : Boolean
  )
  
  // UTILITY
  // aggiorna il timevalue di uno snapshot
  def updateSnapshot(snapshot : MovableStateSnapshot, currentTime : TimeValue) : MovableStateSnapshot = {
    return MovableStateSnapshot(
        snapshot.pedestrianRoute,
        snapshot.carRoute,
        snapshot.busRoute,
        snapshot.tramRoute,
        snapshot.currentRoute,
        snapshot.index,
        snapshot.beginOfTheStep,
        snapshot.currentPointsSequence,
        snapshot.currentPointIndex,
        snapshot.previousPointIndex,
        currentTime,
        snapshot.nextVehicleId,
        snapshot.previousVehicleId,
        snapshot.predecessorGoneSent,
        snapshot.travellers,
        snapshot.previousLaneId,
        snapshot.alreadyHidden
    )
  }
  
}

class MovableState {
  
  import MovableState._
  
  var pedestrianRoute : pedestrian_route = null
  var carRoute : car_route = null
  var busRoute : bus_route = null
  var tramRoute : tram_route = null
  
  var currentRoute : List[step] = null
  var index = 0
  
  def handleIndexOverrun() : Unit = {
    if(pedestrianRoute != null) {
      if(currentRoute == pedestrianRoute.houseToWorkRoute) {
        currentRoute = pedestrianRoute.workToFunRoute
      }
      else if(currentRoute == pedestrianRoute.workToFunRoute) {
        currentRoute = pedestrianRoute.funToHomeRoute
      }
      else {
        currentRoute = pedestrianRoute.houseToWorkRoute
      }
      index = 0
    }
    else if(carRoute != null) {
      if(currentRoute == carRoute.houseToWorkRoute) {
        currentRoute = carRoute.workToFunRoute
      }
      else if(currentRoute == carRoute.workToFunRoute) {
        currentRoute = carRoute.funToHomeRoute
      }
      else {
        currentRoute = carRoute.houseToWorkRoute
      }
      index = 0
    }
    else if(busRoute != null) {
      index = 0
    }
    else {
      index = 0
    }
  }
  
  def getCurrentStepId : String = {
    currentRoute(index) match {
      case road_step(road, _) =>
        return road.id
      case lane_step(lane, _) =>
        return lane.id
      case crossroad_step(crossroad, _) =>
        return crossroad.id
      case pedestrian_crossroad_step(pedestrian_crossroad, _) =>
        return pedestrian_crossroad.id
      case bus_stop_step(bus_stop, _, _) =>
        return bus_stop.id
      case tram_stop_step(tram_stop, _, _) =>
        return tram_stop.id
      case zone_step(zone, _) =>
        return zone.id
    }
  }
  
  def getPreviousStepId : String = {
    return Routes.getStepId(getPreviousStep())
  }
  
  def getNextStepId : String = {
    return Routes.getStepId(getStepAt(1))
  }
  
  def getNextStep() : step = {
    return getStepAt(1)
  }
  
  def getStepIdAt(offset : Int) : String = {
    return Routes.getStepId(getStepAt(offset))
  }
  
  def getPreviousStep() : step = {
    if(pedestrianRoute != null) {
      if(currentRoute == pedestrianRoute.houseToWorkRoute) {
        if(index == 0) {
          return pedestrianRoute.funToHomeRoute(pedestrianRoute.funToHomeRoute.length-1)
        }
        else {
          return currentRoute(index-1)
        }
      }
      else if(currentRoute == pedestrianRoute.workToFunRoute) {
        if(index == 0) {
          return pedestrianRoute.houseToWorkRoute(pedestrianRoute.houseToWorkRoute.length-1)
        }
        else {
          return currentRoute(index-1)
        }
      }
      else {
        if(index == 0) {
          return pedestrianRoute.workToFunRoute(pedestrianRoute.workToFunRoute.length-1)
        }
        else {
          return currentRoute(index-1)
        }
      }
    }
    else if(carRoute != null) {
      if(currentRoute == carRoute.houseToWorkRoute) {
        if(index == 0) {
          return carRoute.funToHomeRoute(carRoute.funToHomeRoute.length-1)
        }
        else {
          return currentRoute(index-1)
        }
      }
      else if(currentRoute == carRoute.workToFunRoute) {
        if(index == 0) {
          return carRoute.houseToWorkRoute(carRoute.houseToWorkRoute.length-1)
        }
        else {
          return currentRoute(index-1)
        }
      }
      else {
        if(index == 0) {
          return carRoute.workToFunRoute(carRoute.workToFunRoute.length-1)
        }
        else {
          return currentRoute(index-1)
        }
      }
    }
    else if(busRoute != null) {
      if(index == 0) {
        return busRoute.route(busRoute.route.length-1)
      }
      else {
        return currentRoute(index-1)
      }
    }
    else {
      if(index == 0) {
        return tramRoute.route(tramRoute.route.length-1)
      }
      else {
        return currentRoute(index-1)
      }
    }
  }
  
  /*
   * Noto l'indice attuale (index), restituisce lo step a (index + offset), con offest possibilmente negativo o nullo
   */
  def getStepAt(offset : Int) : step = {
    if(pedestrianRoute != null) {
      // crea una lista unica
      val commonList = pedestrianRoute.houseToWorkRoute ++ pedestrianRoute.workToFunRoute ++ pedestrianRoute.funToHomeRoute
      // aggiusta l'indice
      var targetIndex = 0
      if(currentRoute == pedestrianRoute.houseToWorkRoute) {
        // l'indice è già giusto, bisogna solo aggiungere l'offset
        targetIndex = index + offset
      }
      else if(currentRoute == pedestrianRoute.workToFunRoute) {
        // l'indice va anche aumentato della lunghezza di houseToWorkRoute
        targetIndex = pedestrianRoute.houseToWorkRoute.length + index + offset
      }
      else {
        // l'indice va aumentato della lunghezza dei due pezzi precedenti
        targetIndex = pedestrianRoute.houseToWorkRoute.length + pedestrianRoute.workToFunRoute.length + index + offset
      }
      // aggiusta l'indice e restituisci lo step corrispondente
      if(targetIndex < 0) {
        targetIndex = commonList.length + targetIndex
      }
      else {
        targetIndex = targetIndex % commonList.length
      }
      return commonList(targetIndex)
    }
    else if(carRoute != null) {
      // crea una lista unica
      val commonList = carRoute.houseToWorkRoute ++ carRoute.workToFunRoute ++ carRoute.funToHomeRoute
      // aggiusta l'indice
      var targetIndex = 0
      if(currentRoute == carRoute.houseToWorkRoute) {
        // l'indice è già giusto, bisogna solo aggiungere l'offset
        targetIndex = index + offset
      }
      else if(currentRoute == carRoute.workToFunRoute) {
        // l'indice va anche aumentato della lunghezza di houseToWorkRoute
        targetIndex = carRoute.houseToWorkRoute.length + index + offset
      }
      else {
        // l'indice va aumentato della lunghezza dei due pezzi precedenti
        targetIndex = carRoute.houseToWorkRoute.length + carRoute.workToFunRoute.length + index + offset
      }
      // aggiusta l'indice e restituisci lo step corrispondente
      if(targetIndex < 0) {
        targetIndex = commonList.length + targetIndex
      }
      else {
        targetIndex = targetIndex % commonList.length
      }
      return commonList(targetIndex)
    }
    else if(busRoute != null) {
      var targetIndex = index + offset
      if(targetIndex < 0) {
        targetIndex = busRoute.route.length + targetIndex
      }
      else {
        targetIndex = targetIndex % busRoute.route.length
      }
      return busRoute.route(targetIndex)
    }
    else {
      var targetIndex = index + offset
      if(targetIndex < 0) {
        targetIndex = tramRoute.route.length + targetIndex
      }
      else {
        targetIndex = targetIndex % tramRoute.route.length
      }
      return tramRoute.route(targetIndex)
    }
  }
  
  def fromZone() : Boolean = {
    getPreviousStep() match {
      case zone_step(_ ,_) =>
        return true
      case _ =>
        return false
    }
  }
  
  def toZone() : Boolean = {
    getNextStep() match {
      case zone_step(_ ,_) =>
        return true
      case _ =>
        return false
    }
  }
  
  def fromBusStop() : Boolean = {
    getPreviousStep() match {
      case bus_stop_step(_ ,_, false) =>
        return true
      case _ =>
        return false
    }
  }
  
  def fromTramStop() : Boolean = {
    getPreviousStep() match {
      case tram_stop_step(_ ,_, false) =>
        return true
      case _ =>
        return false
    }
  }
  
  def fromCrossroad() : Boolean = {
    getPreviousStep() match {
      case crossroad_step(_ ,_) =>
        return true
      case _ =>
        return false
    }
  }
  
  def toCrossroad() : Boolean = {
    getNextStep() match {
      case crossroad_step(_ ,_) =>
        return true
      case _ =>
        return false
    }
  }
  
  // test affidabile per capire se si è da due incroci o meno
  def fromDoubleCrossroad() : Boolean = {
    getStepAt(-1) match {
      case crossroad_step(_, _) =>
        getStepAt(-2) match {
          case crossroad_step(_, _) =>
            return true
          case _ =>
            return false
        }
      case _ =>
        return false
    }
  }
  
  /*
   * Ritorna una lista con:
   * previosPreviousStep
   * previousStep
   * currentStep
   * nextStep
   * nextNextStep
   * nextNextNextStep
   */
  def getStepSequence() : List[step] = {
    var stepSequence = List[step]()
    for(offset <- -2 to 3) {
      stepSequence = stepSequence :+ getStepAt(offset)
    }
    return stepSequence
  }
  
  def handleRoute(route : route) : Unit = {
    route match {
      case original @ pedestrian_route(houseEndTime, houseToWorkRoute, workEndTime, workToFunRoute, funEndTime, funToHomeRoute) =>
        pedestrianRoute = original
        currentRoute = houseToWorkRoute
      case original @ car_route(houseEndTime, houseToWorkRoute, workEndTime, workToFunRoute, funEndTime, funToHomeRoute) =>
        carRoute = original
        currentRoute = houseToWorkRoute
      case original @ bus_route(inner_route) =>
        busRoute = original
        currentRoute = inner_route
      case original @ tram_route(inner_route) =>
        tramRoute = original
        currentRoute = inner_route
    }
  }
  
  // GESTIONE STEP
  var beginOfTheStep = true
  
  var currentPointsSequence = List[List[point]]()
  
  var currentPointIndex = 0
  
  var previousPointIndex = -1
  
  // TIME
  // tempo corrente
  var currentTime = TimeValue(0, 0)
  
  // DOMINIO
  // id del veicolo davanti a noi
  var nextVehicleId : String = null
  // id del veicolo dietro a noi
  var previousVehicleId : String = null
  // flag se abbiamo già mandato il messaggio predecessorGone o meno
  var predecessorGoneSent : Boolean = true
  
  // BUS
  // mappa dei pedoni trasportati, con rispettive destinazioni
  var travellers = Map[String, (String, MovableStateSnapshot)]()
  
  // VEICOLO
  // id della previous lane
  var previousLaneId : String = null
  
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
  

  
  // DORMI/VEGLIA
  // serve per garantire un solo evento di dormita e un solo evento di risveglio verso l'interfaccia grafica
  var alreadyHidden = false
  
  def getSnapshot() : MovableStateSnapshot = {
    val snapshot = MovableStateSnapshot(
        pedestrianRoute,
        carRoute,
        busRoute,
        tramRoute,
        currentRoute,
        index,
        beginOfTheStep,
        currentPointsSequence,
        currentPointIndex,
        previousPointIndex,
        currentTime,
        nextVehicleId,
        previousVehicleId,
        predecessorGoneSent,
        travellers,
        previousLaneId,
        alreadyHidden
      )
    return snapshot
  }
  
  def setSnapshot(snapshot : MovableStateSnapshot) : Unit = {
    pedestrianRoute = snapshot.pedestrianRoute
    carRoute = snapshot.carRoute
    busRoute = snapshot.busRoute
    tramRoute = snapshot.tramRoute
    currentRoute = snapshot.currentRoute
    index = snapshot.index
    beginOfTheStep = snapshot.beginOfTheStep
    currentPointsSequence = snapshot.currentPointsSequence
    currentPointIndex = snapshot.currentPointIndex
    previousPointIndex = snapshot.previousPointIndex
    currentTime = snapshot.currentTime
    nextVehicleId = snapshot.nextVehicleId
    previousVehicleId = snapshot.previousVehicleId
    predecessorGoneSent = snapshot.predecessorGoneSent
    travellers = snapshot.travellers
    previousLaneId = snapshot.previousLaneId
    alreadyHidden = snapshot.alreadyHidden
  }
  
}