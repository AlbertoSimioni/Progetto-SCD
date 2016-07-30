package pubsub

import coreActors.ActiveConnections
import messagesFormatter.BrowserMessagesFormatter
import modelActors.Messages.CreateMobileEntity
import time.TimeCounter
import TimeCounter.UpdateTime
import akka.actor.{ActorLogging, Actor}
import akka.contrib.pattern.{DistributedPubSubMediator,DistributedPubSubExtension}
import coreActors.ActiveConnections
import pubsub.Messages._
import time.TimeMessages.{TimeValue, TimeCommand}

/**
 * Created by Alberto on 20/07/2015.
 */

//Subscriber, il tipo dato in input indica quali classi di messaggi deve gestire
class Subscriber(contentType : String) extends Actor with ActorLogging {
  import DistributedPubSubMediator.{ Subscribe, SubscribeAck }
  val mediator = DistributedPubSubExtension(context.system).mediator
  // subscribe to the topic named "content"
  mediator ! Subscribe(contentType, self)

  def receive = {
    case SubscribeAck(Subscribe("modelEvent", None, `self`)) ⇒
      context become readyModel
    case SubscribeAck(Subscribe("timeEvent", None, `self`)) ⇒
      context become readyTime

  }
  //Messaggi inviati dai worker per inviare al guihandler gli aggiornamenti nel model
  def readyModel: Actor.Receive = {
    case m @ carPosition(id,lat,long,dir) =>   context.actorSelection("/user/activeConnections") !
      ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.CarPositionToJson(m))
    case m @ busPosition(id,lat,long,dir) =>   context.actorSelection("/user/activeConnections") !
      ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.BusPositionToJson(m))
    case m @ tramPosition(id,lat,long,dir) =>   context.actorSelection("/user/activeConnections") !
      ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.TramPositionToJson(m))
    case m @ pedestrianPosition(id,lat,long,dir) =>   context.actorSelection("/user/activeConnections") !
      ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.PedestrianPositionToJson(m))
    case m @ hideCar(id) =>   context.actorSelection("/user/activeConnections") !
      ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.HideCarToJson(m))
    case m @ hideBus(id) =>   context.actorSelection("/user/activeConnections") !
      ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.HideBusToJson(m))
    case m @ hidePedestrian(id) =>   context.actorSelection("/user/activeConnections") !
      ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.HidePedestrianToJson(m))
    case m @ hideTram(id) =>   context.actorSelection("/user/activeConnections") !
      ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.HideTramToJson(m))
    case m @ semaphoreState(id,upGreen,rightGreen,downGreen,leftGreen) =>  context.actorSelection("/user/activeConnections") !
      ActiveConnections.updateSemaphoreState(id,BrowserMessagesFormatter.SemaphoreStateToJson(m))
    case TimeCommand(time) =>    context.actorSelection("/user/activeConnections") !
      ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.TimeToJson(time.hours,time.minutes))
    case CreateMobileEntity(id,route) =>  context.actorSelection("/user/activeConnections") !
      ActiveConnections.entityPath(id,BrowserMessagesFormatter.PathToJson(id,route))
  }

  //Messaggi inviati dal controller ai worker per fare avanzare il tempo
  def readyTime: Actor.Receive = {
    case CurrentTime(daysElapsed, minutesElapsed) =>   context.actorSelection("/user/timeCounter") !
      UpdateTime(daysElapsed, minutesElapsed)

  }
}