package main

import akka.actor.{DeadLetter, ActorLogging, Actor}
import common.ToPersistentMessages._
import modelActors.Messages._
import common.CommonMessages.{Ack,Request}


/**
 * Created by Alberto on 8/19/2016.
 */
class DeadLetterListener extends Actor with ActorLogging {

  override def receive = {
    case DeadLetter(msg, from, to) =>
      var log : String = from + " => " + to + ": "
      var flag = true
      if(flag) {
        println(log + msg.toString)
      }

  }
}
