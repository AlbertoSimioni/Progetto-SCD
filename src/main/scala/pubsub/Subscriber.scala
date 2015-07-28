package pubsub

import akka.actor.{ActorLogging, Actor}
import akka.contrib.pattern.{DistributedPubSubMediator,DistributedPubSubExtension}
import coreActors.ActiveConnections
import pubsub.Messages.Moved
/**
 * Created by Alberto on 20/07/2015.
 */
class Subscriber extends Actor with ActorLogging {
  import DistributedPubSubMediator.{ Subscribe, SubscribeAck }
  val mediator = DistributedPubSubExtension(context.system).mediator
  // subscribe to the topic named "content"
  mediator ! Subscribe("content", self)

  def receive = {
    case SubscribeAck(Subscribe("content", None, `self`)) ⇒
      context become ready
  }

  def ready: Actor.Receive = {
    case Moved(p) =>   context.actorSelection("/user/activeConnections") ! ActiveConnections.SendMessageToClients(p.toString)

  }
}