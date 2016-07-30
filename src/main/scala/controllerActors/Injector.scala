package controllerActors

import pubsub.PublisherInstance

import scala.collection.immutable.StringOps

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.PoisonPill
import akka.contrib.pattern.ClusterSharding
import com.typesafe.config.ConfigFactory

import modelActors.Messages._
import modelActors.immovable.ImmovableActor
import modelActors.movable.MovableActor
import common.CommonMessages._
import common.ToNonPersistentMessages
import common.ToPersistentMessages
import Messages._
import map.Routes
import map.Routes._

/**
 * @author Matteo Pozza
 * La classe modella l'attore che crea entità mobili, generando un percorso appropriato e iniettandolo dentro all'entità.
 */
class Injector extends Actor {
  
  // SHARDING
  // Permette di comunicare con altri ImmovableActor utilizzando il loro identificativo invece che il loro indirizzo
  val shardRegion = ClusterSharding(context.system).shardRegion(ImmovableActor.typeOfEntries)

  val publisherGuiHanlder = PublisherInstance.getPublisherModelEvents(context.system)
  // determina l'inizio dell'injection
  sendToNonPersistent(self, self, StartInjection)
  
  override def receive : Receive = {
    case ToNonPersistent(destinationRef, toNonPersistentMessage) =>
      toNonPersistentMessage match {
        case ToNonPersistentMessages.FromImmovable(senderId, command) =>
          //
        case ToNonPersistentMessages.FromMovable(senderRef, command) =>
          //
        case ToNonPersistentMessages.FromNonPersistent(senderRef, command) =>
          command match {
            case StartInjection =>
              var id_counter = 1
              // preferibile (ma non necessario) avere bus e tram già in circolazione prima di iniettare pedoni e macchine
              val num_bus_routes = getAllBusRoutes().length
              for(route <- 1 to num_bus_routes) {
                val id = "BUS" + "%07d".format(id_counter)
                sendToNonPersistent(self, self, CreateBus(id, route))
                id_counter = id_counter + 1
              }
              val num_tram_routes = getAllTramRoutes().length
              for(route <- 1 to num_tram_routes) {
                val id = "TRA" + "%07d".format(id_counter)
                sendToNonPersistent(self, self, CreateTram(id, route))
                id_counter = id_counter + 1
              }
              // recupera il numero di pedoni e macchine
              val configuration = ConfigFactory.load
              val num_pedestrians = configuration.getInt("domain.num_pedestrians")
              for(i <- 1 to num_pedestrians) {
                val id = "PED" + "%07d".format(id_counter)
                sendToNonPersistent(self, self, CreatePedestrian(id))
                id_counter = id_counter + 1
              }
              val num_car = configuration.getInt("domain.num_cars")
              for(i <- 1 to num_car) {
                val id = "CAR" + "%07d".format(id_counter)
                sendToNonPersistent(self, self, CreateCar(id))
                id_counter = id_counter + 1
              }
              // lavoro finito
              self ! PoisonPill
            
            case CreatePedestrian(id) =>
              val pedestrianRoute = Routes.createPedestrianRoute()._1
              val firstId = Routes.getStepId(pedestrianRoute.houseToWorkRoute(0))
              sendToImmovable(self, firstId, CreateMobileEntity(id, pedestrianRoute))
              publisherGuiHanlder ! CreateMobileEntity(id, pedestrianRoute)
            case CreateCar(id) =>
              val carRoute = Routes.createCarRoute()
              val firstId = Routes.getStepId(carRoute.houseToWorkRoute(0))
              sendToImmovable(self, firstId, CreateMobileEntity(id, carRoute))
              publisherGuiHanlder ! CreateMobileEntity(id, carRoute)
            case CreateBus(id, route) =>
              val busRoute = Routes.createBusRoute(route)
              val firstId = Routes.getStepId(busRoute.route(0))
              sendToImmovable(self, firstId, CreateMobileEntity(id, busRoute))
              publisherGuiHanlder ! CreateMobileEntity(id, busRoute)
            case CreateTram(id, route) =>
              val tramRoute = Routes.createBusRoute(route)
              val firstId = Routes.getStepId(tramRoute.route(0))
              sendToImmovable(self, firstId, CreateMobileEntity(id, tramRoute))
              publisherGuiHanlder ! CreateMobileEntity(id, tramRoute)
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