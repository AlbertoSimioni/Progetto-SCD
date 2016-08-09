import time.TimeMessages._
import org.scalatest._
import map.JSONReader
import map.BreadthFirst._
import map.PointsSequence._
import map.Domain.position._
import map.Routes
import map.Routes.direction
import map.Domain._
import map.Routes._

/**
 * @author studio
 */
object RoutePrinter {
  
  def printRoute(route : Any) : Unit = {
    route match {
      case pedestrian_route(houseEnd, houseSteps, workEnd, workSteps, funEnd, funSteps) =>
        println("House end time: " ++ houseEnd.hours.toString() ++ ":" ++ houseEnd.minutes.toString())
        for(step <- houseSteps) {
          println(printStep(step))
        }
        println("Work end time: " ++ workEnd.hours.toString() ++ ":" ++ workEnd.minutes.toString())
        for(step <- workSteps) {
          println(printStep(step))
        }
        println("Fun end time: " ++ funEnd.hours.toString() ++ ":" ++ funEnd.minutes.toString())
        for(step <- funSteps) {
          println(printStep(step))
        }
      case car_route(houseEnd, houseSteps, workEnd, workSteps, funEnd, funSteps) =>
        println("House end time: " ++ houseEnd.hours.toString() ++ ":" ++ houseEnd.minutes.toString())
        for(step <- houseSteps) {
          println(printStep(step))
        }
        println("Work end time: " ++ workEnd.hours.toString() ++ ":" ++ workEnd.minutes.toString())
        for(step <- workSteps) {
          println(printStep(step))
        }
        println("Fun end time: " ++ funEnd.hours.toString() ++ ":" ++ funEnd.minutes.toString())
        for(step <- funSteps) {
          println(printStep(step))
        }
      case bus_route(steps) =>
        println("Bus route:")
        for(step <- steps) {
          println(printStep(step))
        }
      case tram_route(steps) =>
        println("Tram route:")
        for(step <- steps) {
          println(printStep(step))
        }
    }
  }
  
  def printStep(step : step) : String = {
    step match {
      case road_step(road, direction) =>
        return "Exiting from road " ++ road.id ++ " with direction: " ++ printDirection(direction)
      case lane_step(lane, direction) =>
        return "Exiting from lane " ++ lane.id ++ " with direction: " ++ printDirection(direction)
      case crossroad_step(crossroad, direction) =>
        return "Exiting from crossroad " ++ crossroad.id ++ " with direction: " ++ printDirection(direction)
      case pedestrian_crossroad_step(pedestrian_crossroad, direction) =>
        return "Exiting from pedestrian crossroad " ++ pedestrian_crossroad.id ++ " with direction: " ++ printDirection(direction)
      case bus_stop_step(bus_stop, direction, ignore) =>
        if(ignore == true) {
          return "Exiting from bus stop " ++ bus_stop.id ++ " with direction: " ++ printDirection(direction)
        }
        else {
          return "Considering " ++ bus_stop.id ++ " with direction: " ++ printDirection(direction)
        }
      case tram_stop_step(tram_stop, direction, ignore) =>
        if(ignore == true) {
          return "Exiting from tram stop " ++ tram_stop.id ++ " with direction: " ++ printDirection(direction)
        }
        else {
          return "Considering " ++ tram_stop.id ++ " with direction: " ++ printDirection(direction)
        }
      case zone_step(zone, direction) =>
        return "Exiting from zone " ++ zone.id ++ " with direction: " ++ printDirection(direction)
    }
  }
  
  def printDirection(direction : direction) : String = {
    if(direction.beginToEnd == true) {
      // verso destra o verso l'alto
      direction.position match {
        case position.up =>
          return "rightward on the upper side"
        case position.down =>
          return "rightward on the lower side"
        case position.left =>
          return "upward on the left side"
        case position.right =>
          return "upward on the right side"
      }
    }
    else {
      // verso sinistra o verso il basso
      direction.position match {
        case position.up =>
          return "leftward on the upper side"
        case position.down =>
          return "leftward on the lower side"
        case position.left =>
          return "downward on the left side"
        case position.right =>
          return "downward on the right side"
      }
    }
  }
  
}