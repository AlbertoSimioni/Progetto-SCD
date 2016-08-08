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
import map.Domain._
import map.JSONReader
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
              // dal momento che per gli attori del cluster sharding non è possibile utilizzare un parametro nel props
              // siamo costretti ad inviargli un messaggio di identità iniziale
              for(bus_stop <- JSONReader.getAllBusStops(current_map)) {
                sendToImmovable(self, bus_stop.id, Identity(bus_stop.id))
              }
              for(crossroad <- JSONReader.getAllCrossroads(current_map)) {
                sendToImmovable(self, crossroad.id, Identity(crossroad.id))
              }
              for(lane <- JSONReader.getAllLanes(current_map)) {
                sendToImmovable(self, lane.id, Identity(lane.id))
              }
              for(pedestrian_crossroad <- JSONReader.getAllPedestrianCrossroads(current_map)) {
                sendToImmovable(self, pedestrian_crossroad.id, Identity(pedestrian_crossroad.id))
              }
              for(road <- JSONReader.getAllRoads(current_map)) {
                sendToImmovable(self, road.id, Identity(road.id))
              }
              for(tram_stop <- JSONReader.getAllTramStops(current_map)) {
                sendToImmovable(self, tram_stop.id, Identity(tram_stop.id))
              }
              for(zone <- JSONReader.getAllZones(current_map)) {
                sendToImmovable(self, zone.id, Identity(zone.id))
              }
              // injection vera e propria
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
              println("ATTENZIONE! PERCORSO PEDONE NON RANDOM")
              val pedestrianRoute = Routes.createPedestrianRoute()._1
              val firstId = Routes.getStepId(pedestrianRoute.houseToWorkRoute(0))
              sendToImmovable(self, firstId, CreateMobileEntity(id, pedestrianRoute))
              publisherGuiHanlder ! CreateMobileEntity(id, pedestrianRoute)
              println("Orario uscita di casa pedone: " + pedestrianRoute.houseEndTime)
            case CreateCar(id) =>
              println("ATTENZIONE! PERCORSO AUTOMOBILE NON RANDOM")
              val carRoute = Routes.createCarRoute()
              val firstId = Routes.getStepId(carRoute.houseToWorkRoute(0))
              sendToImmovable(self, firstId, CreateMobileEntity(id, carRoute))
              publisherGuiHanlder ! CreateMobileEntity(id, carRoute)
              println("Orario uscita di casa macchina: " + carRoute.houseEndTime)
            case CreateBus(id, route) =>
              val busRoute = Routes.createBusRoute(route)
              val firstId = Routes.getStepId(busRoute.route(0))
              sendToImmovable(self, firstId, CreateMobileEntity(id, busRoute))
              publisherGuiHanlder ! CreateMobileEntity(id, busRoute)
            case CreateTram(id, route) =>
              val tramRoute = Routes.createTramRoute(route)
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