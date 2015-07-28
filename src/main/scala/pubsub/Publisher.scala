package pubsub

import akka.actor.{ActorRef, ActorSystem, Props, Actor}
import akka.contrib.pattern.{DistributedPubSubMediator,DistributedPubSubExtension}

/**
 * Created by Alberto on 20/07/2015.
 */

class Publisher(messageType: String) extends Actor {

  import DistributedPubSubMediator.Publish
  // activate the extension
  val mediator = DistributedPubSubExtension(context.system).mediator
  def receive = {
    case in: Any ⇒
      mediator ! Publish(messageType, in)
  }
}


object PublisherInstance {

  var publisherModel : ActorRef = null;
  var publisherTime : ActorRef = null;

  def getPublisherModelEvents(system: ActorSystem) : ActorRef = {
    if (publisherModel == null){
      publisherModel =  system.actorOf(Props(classOf[Publisher], "modelEvent"), "publisherModel")
      publisherModel
    }
    return publisherModel;
  }
  def getPublisherTimeEvents(system: ActorSystem) : ActorRef = {
    if (publisherTime == null){
      publisherTime =  system.actorOf(Props(classOf[Publisher], "timeEvent"), "publisherTime")
        publisherTime
    }
    return publisherTime;
  }
}
