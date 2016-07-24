package messagesFormatter

import pubsub.Messages._

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

  
  /*def NewCarMessageFormat(m : NewCar) : String =  {
    s"""{"type": "NewCar", "info":{"id":"${m.id}","position":"${m.position}"}}"""
  }*/
}
