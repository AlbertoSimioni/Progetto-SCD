package controllerActors

import pubsub.PublisherInstance

import scala.concurrent.duration._

import akka.actor.Actor
import akka.contrib.pattern.DistributedPubSubMediator
import akka.contrib.pattern.DistributedPubSubExtension
import DistributedPubSubMediator.Publish

import time.TimeMessages._

/**
 * @author Matteo Pozza
 * La classe modella un attore, costruito nel controller, che si preoccupa di rilasciare eventi temporali.
 * Tutti i subscribed (i.e. le entità immobili) ricevono gli eventi da lui pubblicati.
 */

class TimePublisher extends Actor {
  
  import context.dispatcher
  
  // recupera il mediator
  val mediator = DistributedPubSubExtension(context.system).mediator

  val publisherGuiHanlder = PublisherInstance.getPublisherModelEvents(context.system)

  // ogni secondo rilascia un evento temporale
  val autoTick = context.system.scheduler.schedule(0 millis, 1000 millis, self, Tick)
  
  var minutes = 0
  var hours = 0
  
  override def receive : Receive = {
    case Tick =>
      // un secondo è passato, fai incrementare il contatore e pubblicizza il suo valore
      increaseTimeCounter
      mediator ! Publish(timeMessage, TimeCommand(TimeValue(hours, minutes)))
      publisherGuiHanlder ! TimeCommand(TimeValue(hours,minutes))
  }
  
  def increaseTimeCounter() : Unit = {
    if(minutes + 1 == 60) {
      minutes = 0
      if(hours + 1 == 24) {
        hours = 0
      }
      else {
        hours = hours + 1
      }
    }
    else {
      minutes = minutes + 1
    }
  }
  
}