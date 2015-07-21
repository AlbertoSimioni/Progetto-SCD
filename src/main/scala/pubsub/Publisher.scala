package pubsub

import akka.actor.{ActorRef, ActorSystem, Props, Actor}
import akka.contrib.pattern.{DistributedPubSubMediator,DistributedPubSubExtension}

/**
 * Created by Alberto on 20/07/2015.
 */
class Publisher extends Actor {

  import DistributedPubSubMediator.Publish

  // activate the extension
  val mediator = DistributedPubSubExtension(context.system).mediator

  def receive = {
    case in: Any ⇒
     // val out = in.toUpperCase
      mediator ! Publish("content", in)
  }
}
object PublisherInstance {

  var publisher : ActorRef = null;

  def getPublisher(system: ActorSystem) : ActorRef = {
    if (publisher == null){
      publisher =  system.actorOf(Props[Publisher], "publisher");
    }
    return publisher;
  }
}
