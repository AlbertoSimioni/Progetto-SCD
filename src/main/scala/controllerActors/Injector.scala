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
import time.TimeMessages._

import scala.concurrent.duration.Duration

/**
 * @author Matteo Pozza
 * La classe modella l'attore che crea entità mobili, generando un percorso appropriato e iniettandolo dentro all'entità.
 */
class Injector extends Actor {
  import context.dispatcher
  val immediateTime = (TimeValue(16, 30), TimeValue(0, 30), TimeValue(8, 30))
  val deferredTime = (TimeValue(2, 30), TimeValue(10, 30), TimeValue(18, 30))
  val pedestrianBusPlaces = ("Z000008400004680", "Z000048000000480", "Z000038400004320")
  val carPlaces = ("Z000040800001680", "Z000014400004800", "Z000012000001440")
  val carPlaces2 = ("Z000032400001440", "Z000014400004800", "Z000012000001440")
  val carPlaces3 = ("Z000031200002040", "Z000014400004800", "Z000012000001440")
  var i = 0
  var j = 0
  
  // SHARDING
  // Permette di comunicare con altri ImmovableActor utilizzando il loro identificativo invece che il loro indirizzo
  val shardRegion = ClusterSharding(context.system).shardRegion(ImmovableActor.typeOfEntries)

  val publisherGuiHanlder = PublisherInstance.getPublisherModelEvents(context.system)
  // determina l'inizio dell'injection
  //sendToNonPersistent(self, self, StartInjection)
  context.system.scheduler.scheduleOnce(Duration(5000, "millis"), self, ToNonPersistent(self,ToNonPersistentMessages.FromNonPersistent(self, StartInjection)))

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
              /*println("ATTENZIONE! PERCORSO PEDONE NON RANDOM")
              var pedestrianRoute : pedestrian_route = null
              if(i == 0) {
                pedestrianRoute = Routes.createPedestrianRoute(pedestrianBusPlaces, immediateTime)._1
                i = i + 1
              }
              else if(i == 1){
                pedestrianRoute = Routes.createPedestrianRoute(carPlaces, deferredTime)._1
                i = i + 1
              }
              else {
                pedestrianRoute = Routes.createPedestrianRouteWithTram(pedestrianBusPlaces, immediateTime, 0)._1
              }*/

              // con probabilità 0.5 genera un percorso che include l'utilzzo del tram
              var pedestrianRoute : pedestrian_route = null
              if(scala.util.Random.nextInt() % 2 == 0) {
                // normale
                pedestrianRoute = Routes.createPedestrianRoute()._1
              }
              else {
                // con inserimento tram forzoso
                pedestrianRoute = Routes.createPedestrianRouteWithTram()._1
              }

              val firstId = Routes.getStepId(pedestrianRoute.houseToWorkRoute(0))
              sendToImmovable(self, firstId, CreateMobileEntity(id, pedestrianRoute))
              publisherGuiHanlder ! CreateMobileEntity(id, pedestrianRoute)
            case CreateCar(id) =>
             /*println("ATTENZIONE! PERCORSO AUTOMOBILE NON RANDOM")
              var carRoute : car_route = null
              if(j < 2) {
                carRoute = Routes.createCarRoute(carPlaces, immediateTime)
                j = j + 1
              }
              else if(j < 4) {
                carRoute = Routes.createCarRoute(carPlaces2, immediateTime)
                j = j + 1
              }
              else {
                carRoute = Routes.createCarRoute(carPlaces3, immediateTime)
              }*/
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