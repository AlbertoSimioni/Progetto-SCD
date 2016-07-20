package modelActors.movable

import akka.persistence.AtLeastOnceDelivery.AtLeastOnceDeliverySnapshot

import map.Routes
import map.Routes._

/**
 * @author pocia
 */
class MovableState {
  
  var pedestrianRoute : pedestrian_route = null
  var carRoute : car_route = null
  var busRoute : bus_route = null
  var tramRoute : tram_route = null
  
  var currentRoute : List[step] = null
  var index = 0
  
  def handleIndexOverrun : Unit = {
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
    }
  }
  
  def getPreviousStepId : String = {
    if(pedestrianRoute != null) {
      if(currentRoute == pedestrianRoute.houseToWorkRoute) {
        if(index == 0) {
          return Routes.getStepId(pedestrianRoute.funToHomeRoute(pedestrianRoute.funToHomeRoute.length-1))
        }
        else {
          return Routes.getStepId(currentRoute(index-1))
        }
      }
      else if(currentRoute == pedestrianRoute.workToFunRoute) {
        if(index == 0) {
          return Routes.getStepId(pedestrianRoute.houseToWorkRoute(pedestrianRoute.houseToWorkRoute.length-1))
        }
        else {
          return Routes.getStepId(currentRoute(index-1))
        }
      }
      else {
        if(index == 0) {
          return Routes.getStepId(pedestrianRoute.workToFunRoute(pedestrianRoute.workToFunRoute.length-1))
        }
        else {
          return Routes.getStepId(currentRoute(index-1))
        }
      }
    }
    else if(carRoute != null) {
      if(currentRoute == carRoute.houseToWorkRoute) {
        if(index == 0) {
          return Routes.getStepId(carRoute.funToHomeRoute(carRoute.funToHomeRoute.length-1))
        }
        else {
          return Routes.getStepId(currentRoute(index-1))
        }
      }
      else if(currentRoute == carRoute.workToFunRoute) {
        if(index == 0) {
          return Routes.getStepId(carRoute.houseToWorkRoute(carRoute.houseToWorkRoute.length-1))
        }
        else {
          return Routes.getStepId(currentRoute(index-1))
        }
      }
      else {
        if(index == 0) {
          return Routes.getStepId(carRoute.workToFunRoute(carRoute.workToFunRoute.length-1))
        }
        else {
          return Routes.getStepId(currentRoute(index-1))
        }
      }
    }
    else if(busRoute != null) {
      if(index == 0) {
        return Routes.getStepId(busRoute.route(busRoute.route.length-1))
      }
      else {
        return Routes.getStepId(currentRoute(index-1))
      }
    }
    else {
      if(index == 0) {
        return Routes.getStepId(tramRoute.route(tramRoute.route.length-1))
      }
      else {
        return Routes.getStepId(currentRoute(index-1))
      }
    }
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
  
}