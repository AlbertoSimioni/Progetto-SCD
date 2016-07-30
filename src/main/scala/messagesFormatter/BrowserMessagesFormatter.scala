package messagesFormatter

import map.Domain._
import map.Routes._
import pubsub.Messages._
import time.TimeMessages.TimeValue

/**
 * Created by Alberto on 29/07/2015.
 */
//Funzioni che formattano in JSON i messaggi da inviare ai browser
object BrowserMessagesFormatter {

  def CarPositionToJson(m : carPosition) : String =  {
    s"""{"type": "CarPosition", "info":{"id":"${m.id}","lat":${m.lat},"long":${m.long},"direction":"${m.direction}"}}"""
  }

  def BusPositionToJson(m : busPosition) : String =  {
    s"""{"type": "BusPosition", "info":{"id":"${m.id}","lat":${m.lat},"long":${m.long},"direction":"${m.direction}"}}"""
  }

  def TramPositionToJson(m : tramPosition) : String =  {
    s"""{"type": "TramPosition", "info":{"id":"${m.id}","lat":${m.lat},"long":${m.long},"direction":"${m.direction}"}}"""
  }

  def PedestrianPositionToJson(m : pedestrianPosition) : String =  {
    s"""{"type": "PedestrianPosition", "info":{"id":"${m.id}","lat":${m.lat},"long":${m.long},"direction":"${m.direction}"}}"""
  }

  def HideCarToJson(m : hideCar) : String =  {
    s"""{"type": "HideCar", "info":{"id":"${m.id}"}}"""
  }

  def HidePedestrianToJson(m : hidePedestrian) : String =  {
    s"""{"type": "HidePedestrian", "info":{"id":"${m.id}"}}"""
  }

  def HideBusToJson(m : hideBus) : String =  {
    s"""{"type": "HideBus", "info":{"id":"${m.id}"}}"""
  }

  def HideTramToJson(m : hideTram) : String =  {
    s"""{"type": "HideTram", "info":{"id":"${m.id}"}}"""
  }

  def SemaphoreStateToJson(m : semaphoreState) : String =  {
    var up = "red";
    var right = "red";
    var down = "red";
    var left = "red";
    if(m.upGreen){up = "green"}
    if(m.downGreen){down = "green"}
    if(m.leftGreen){left = "green"}
    if(m.rightGreen){right = "green"}

    s"""{"type": "SemaphoreState", "info":{"id":"${m.id}","up":${up},"right":${right},"down":${down},"left":${left}}}"""
  }

  def TimeToJson(hours : Int,minutes : Int) : String =  {

    s"""{"type": "time", "info":{"hours":${hours},"minutes":${minutes}}}"""
  }

  def PathToJson(id: String, path: route) : String = {
    path match {
      case pedestrian_route(houseEndTime, houseToWorkRoute, workEndTime, workToFunRoute, funEndTime,funToHomeRoute) =>
        return StepsToJson(houseToWorkRoute ::: workToFunRoute ::: funToHomeRoute,id)
      case car_route(houseEndTime, houseToWorkRoute, workEndTime, workToFunRoute, funEndTime,funToHomeRoute) =>
        return StepsToJson(houseToWorkRoute ::: workToFunRoute ::: funToHomeRoute,id)
      case bus_route(route) =>
        return StepsToJson(route,id)
      case tram_route(route) =>
        return StepsToJson(route,id)
    }
  }

  def StepsToJson(steps : List[step],id: String) : String = {
    var stringSteps : StringBuilder = new StringBuilder
    stringSteps ++=  s"""{"type": "path", "steps":["""
    var first = true
    val entityType = id.substring(0,1);
    for(step <- steps){
      var stepString : String = ""
      if(first) first = false
      else stringSteps ++= ","
      step match {
          //pedone
          case road_step(road, direction) =>
            stepString = s"""{"type" :"road","id": "${road.id}", "beginToEnd":  ${direction.beginToEnd}, "position": "${direction.position}"}"""
          case lane_step(lane,direction) =>
            stepString = s"""{"type" :"lane","id": "${lane.id}", "beginToEnd":  ${direction.beginToEnd}, "position": "${direction.position}"}"""
          case crossroad_step(crossroad,direction) =>
            stepString = s"""{"type" :"crossroad","id": "${crossroad.id}"}"""
          case pedestrian_crossroad_step(pedestrian_crossroad,direction) =>
            stepString = s"""{"entity":"${entityType}","type" :"crosswalk",id": "${pedestrian_crossroad.id}"}"""
          case bus_stop_step(bus_stop,direction,ignore) =>
            stepString = s"""{"entity":"${entityType}","type" :"bus_stop","id": "${bus_stop.id}", "beginToEnd":  ${direction.beginToEnd}, "position": "${direction.position}", "ignore" : ${ignore}}"""
          case tram_stop_step(tram_stop,direction,ignore) =>
            stepString = s"""{"entity":"${entityType}","type" :"tram_stop","id": "${tram_stop.id}", "beginToEnd":  ${direction.beginToEnd}, "position": "${direction.position}", "ignore" : ${ignore}}"""
          case zone_step(zone, direction) =>
            stepString = s"""{"type" :"zone","id": "${zone.id}"}"""
      }
      stringSteps ++= stepString
    }
    stringSteps ++= "]}"
    return stringSteps.toString()
  }

  /*def NewCarMessageFormat(m : NewCar) : String =  {
    s"""{"type": "NewCar", "info":{"id":"${m.id}","position":"${m.position}"}}"""
  }*/
}
