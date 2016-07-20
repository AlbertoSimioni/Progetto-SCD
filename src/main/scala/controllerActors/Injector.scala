package controllerActors

import akka.actor.Actor
import akka.actor.ActorRef
import akka.contrib.pattern.ClusterSharding

import modelActors.immovable.ImmovableActor
import modelActors.movable.MovableActor
import modelActors.CommonMessages._
import modelActors.ToNonPersistentMessages
import modelActors.ToPersistentMessages

import map.Routes
import map.Routes._

/**
 * @author Matteo Pozza
 * La classe modella l'attore che crea entità mobili, generando un percorso appropriato e iniettandolo dentro all'entità.
 */
object Injector {
  
  case class CreatePedestrian(id : String) extends Command
  case class CreateCar(id : String) extends Command
  case class CreateBus(id : String, route : Int) extends Command
  case class CreateTram(id : String, route : Int) extends Command
  
}

class Injector extends Actor {
  
  import Injector._
  
  // SHARDING
  // Permette di comunicare con altri ImmovableActor utilizzando il loro identificativo invece che il loro indirizzo
  val shardRegion = ClusterSharding(context.system).shardRegion(ImmovableActor.typeOfEntries)
  
  override def receive : Receive = {
    case ToNonPersistent(destinationRef, toNonPersistentMessage) =>
      toNonPersistentMessage match {
        case ToNonPersistentMessages.FromImmovable(senderId, command) =>
          //
        case ToNonPersistentMessages.FromMovable(senderRef, command) =>
          //
        case ToNonPersistentMessages.FromNonPersistent(senderRef, command) =>
          command match {
            case CreatePedestrian(id) =>
              val pedestrianRoute = Routes.createPedestrianRoute()._1
              val firstId = Routes.getStepId(pedestrianRoute.houseToWorkRoute(0))
              sendToImmovable(self, firstId, CreateMobileEntity(id, pedestrianRoute))
            case CreateCar(id) =>
              val carRoute = Routes.createCarRoute()
              val firstId = Routes.getStepId(carRoute.houseToWorkRoute(0))
              sendToImmovable(self, firstId, CreateMobileEntity(id, carRoute))
            case CreateBus(id, route) =>
              val busRoute = Routes.createBusRoute(route)
              val firstId = Routes.getStepId(busRoute.route(0))
              sendToImmovable(self, firstId, CreateMobileEntity(id, busRoute))
            case CreateTram(id, route) =>
              val tramRoute = Routes.createBusRoute(route)
              val firstId = Routes.getStepId(tramRoute.route(0))
              sendToImmovable(self, firstId, CreateMobileEntity(id, tramRoute))
          }
      }
  }
  
  def sendToImmovable(senderRef : ActorRef, destinationId : String, command : Command) : Unit = {
    shardRegion ! ToImmovable(destinationId, ToPersistentMessages.FromNonPersistent(senderRef, command))
  }
  
  def sendToMovable(senderRef : ActorRef, destinationRef : ActorRef, command : Command) : Unit = {
    destinationRef ! ToMovable(destinationRef, ToPersistentMessages.FromNonPersistent(senderRef, command))
  }
  
  def sendToNonPersistent(senderRef : ActorRef, destinationRef : ActorRef, command : Command) : Unit = {
    destinationRef ! ToNonPersistent(destinationRef, ToNonPersistentMessages.FromNonPersistent(senderRef, command))
  }
  
}