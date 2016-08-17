package controllerActors

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props

import common.CommonMessages._
import common.ToPersistentMessages
import common.ToNonPersistentMessages
import Messages._

import scala.concurrent.duration.Duration

/**
 * @author Matteo Pozza
 * Corrisponde al coordinatore vero e prorpio del sistema
 */
class Controller extends Actor {
  import context.dispatcher
  // fai partire il sistema
  //sendToNonPersistent(self, self, StartSystem)

  context.system.scheduler.scheduleOnce(Duration(5000, "millis"), self, ToNonPersistent(self,ToNonPersistentMessages.FromNonPersistent(self, StartSystem)))


  override def receive : Receive = {
    case ToNonPersistent(destinationRef, toNonPersistentMessage) =>
      toNonPersistentMessage match {
        case ToNonPersistentMessages.FromImmovable(senderId, command) =>
          //
        case ToNonPersistentMessages.FromMovable(senderRef, command) =>
          //
        case ToNonPersistentMessages.FromNonPersistent(senderRef, command) =>
          command match {
            case StartSystem =>
              // per prima cosa, azzera il db corrente
              val dbEraser = context.actorOf(Props(new DBEraser))
              sendToNonPersistent(self, dbEraser, EraseDB)
            case EraseDBAck =>
              // una volta che il db è stato azzerato, fai partire l'attore del tempo
              val timeCounter = context.actorOf(Props(new TimePublisher))
              // poi fai partire l'iniettore di entità
              val injector = context.actorOf(Props(new Injector))
          }
      }
  }
  
  def sendToNonPersistent(senderRef : ActorRef, destinationRef : ActorRef, command : Command) : Unit = {
    destinationRef ! ToNonPersistent(destinationRef, ToNonPersistentMessages.FromNonPersistent(senderRef, command))
  }
  
}