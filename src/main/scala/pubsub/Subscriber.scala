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
  }
  //Messaggi inviati dai worker per inviare al guihandler gli aggiornamenti nel model
  def readyModel: Actor.Receive = {
    case m @ carPosition(id,lat,long,dir) =>   context.actorSelection("/user/activeConnections") !
      ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.CarPositionToJson(m),true)
      //println("carPosition")
    case m @ busPosition(id,lat,long,dir) =>   context.actorSelection("/user/activeConnections") !
      ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.BusPositionToJson(m),true)
      //println("busPosition")
    case m @ tramPosition(id,lat,long,dir) =>   context.actorSelection("/user/activeConnections") !
      ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.TramPositionToJson(m),true)
      //println("tramPosition")
    case m @ pedestrianPosition(id,lat,long,dir) =>   context.actorSelection("/user/activeConnections") !
      ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.PedestrianPositionToJson(m),true)
      //println("PedestrianPosition")
    case m @ hideCar(id,zoneID) =>   context.actorSelection("/user/activeConnections") !
      hideCar(id,zoneID)
      //println("hideCar")
    case m @ hideBus(id) =>   context.actorSelection("/user/activeConnections") !
      ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.HideBusToJson(m),false)
      //println("hideBus")
    case m @ hidePedestrian(id,zoneID,inVehicle) =>   context.actorSelection("/user/activeConnections") !
      hidePedestrian(id,zoneID,inVehicle)
      //println("hidePedestrian")
    case m @ hideTram(id) =>   context.actorSelection("/user/activeConnections") !
      ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.HideTramToJson(m),false)
      //println("hideTram");
    case m @ semaphoreState(id,upGreen,rightGreen,downGreen,leftGreen,tramGreen) =>  context.actorSelection("/user/activeConnections") !
      ActiveConnections.updateSemaphoreState(id,BrowserMessagesFormatter.SemaphoreStateToJson(m))
    case TimeCommand(time) =>    context.actorSelection("/user/activeConnections") !
      ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.TimeToJson(time.hours,time.minutes),false)
    case CreateMobileEntity(id,route) =>  context.actorSelection("/user/activeConnections") !
      ActiveConnections.entityPath(id,BrowserMessagesFormatter.PathToJson(id,route))
    case entityAwaked(entityID,zoneID) => context.actorSelection("/user/activeConnections") !
      entityAwaked(entityID,zoneID)

  }
}