package pubsub

import akka.actor.{ActorRef, ActorSystem, Props, Actor}
import akka.contrib.pattern.{DistributedPubSubMediator,DistributedPubSubExtension}

/**
 * Created by Alberto on 20/07/2015.
 */

//Publisher: fa il forwarding ai subscriber di tutti i messaggi dati in input. Come tipo del messaggio
//utilizza quello dato in input
class Publisher(messageType: String) extends Actor {

  import DistributedPubSubMediator.Publish
  // activate the extension
  val mediator = DistributedPubSubExtension(context.system).mediator
  def receive = {
    case in: Any ⇒
      mediator ! Publish(messageType, in)
  }
}

//Oggetto per richiedere le uniche istanze dei publisher
object PublisherInstance {

  var publisherModel : ActorRef = null;
  var publisherTime : ActorRef = null;

  //istanza creata nei worker (una per ogni nodo)
  def getPublisherModelEvents(system: ActorSystem) : ActorRef = {
    if(publisherModel == null){
      //publisherModel = system.actorOf(Props(classOf[Publisher], "modelEvent"), "publisherModel")
      publisherModel = system.actorOf(Props(classOf[Publisher], "modelEvent"))
    }
    return publisherModel
  }


}
