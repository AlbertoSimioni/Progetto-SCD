package pubsub

import coreActors.ActiveConnections
import messagesFormatter.BrowserMessagesFormatter
import time.TimeCounter
import TimeCounter.UpdateTime
import akka.actor.{ActorLogging, Actor}
import akka.contrib.pattern.{DistributedPubSubMediator,DistributedPubSubExtension}
import coreActors.ActiveConnections
import pubsub.Messages._
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
    case m @ Moved(id,p) =>   context.actorSelection("/user/activeConnections") !
      ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.CarMovedMessageFormat(m))
    /*case m @ NewCar(id,p) =>   context.actorSelection("/user/activeConnections") !
      ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.NewCarMessageFormat(m))*/

  }

  //Messaggi inviati dal controller ai worker per fare avanzare il tempo
  def readyTime: Actor.Receive = {
    case CurrentTime(daysElapsed, minutesElapsed) =>   context.actorSelection("/user/timeCounter") !
      UpdateTime(daysElapsed, minutesElapsed)

  }
}