package time

import akka.actor.{ActorLogging, ActorRef, Actor}
import time.TimeCounter.{ResponseCurrentTime, GetTime, UpdateTime}


import scala.collection.mutable

/**
 * Created by Alberto on 28/07/2015.
 */

object TimeCounter{
  sealed trait TimeMessages
  case class UpdateTime(days : Int, minutes: Int) extends TimeMessages
  case class GetTime() extends TimeMessages
  case class ResponseCurrentTime(daysElapsed : Int, minutesElapsed: Int) extends TimeMessages
}


class TimeCounter extends Actor with ActorLogging {
  var CurrentDay = 0;
  var CurrentMinutes = 0;

  var firstMessageArrived = false
  val waitingVehicles = mutable.ListBuffer[ActorRef]()

  def receive = {
    case UpdateTime(daysElapsed, minutesElapsed) =>
      CurrentDay = daysElapsed
      CurrentMinutes = minutesElapsed
      //log.info(minutesElapsed.toString)
      if(!firstMessageArrived) {
        firstMessageArrived = true
        for(vehicle <- waitingVehicles)
          vehicle ! ResponseCurrentTime(CurrentDay,CurrentMinutes)
        waitingVehicles.clear()
      }
    case GetTime() =>
      if(!firstMessageArrived) {
        waitingVehicles += sender
      }
      else
        sender ! ResponseCurrentTime(CurrentDay,CurrentMinutes)
  }
}
