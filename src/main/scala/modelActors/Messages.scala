package modelActors

import akka.actor.ActorRef
import akka.actor.ActorPath

import map.Domain._
import common.CommonMessages._
import map.Routes._
import time.TimeMessages._

/**
 * @author Matteo Pozza
 * Definisce tutti i messaggi che possono scambiarsi entità mobili ed immobili tra di loro
 */
object Messages {
  
  // dentro all'appropriato handler, definisce lo specifico comportamento
  trait RealCommand
  case class FromBusStop(message : Any) extends RealCommand
  case class FromCrossroad(message : Any) extends RealCommand
  case class FromLane(message : Any) extends RealCommand
  case class FromPedestrianCrossroad(message : Any) extends RealCommand
  case class FromRoad(message : Any) extends RealCommand
  case class FromTramStop(message : Any) extends RealCommand
  case class FromZone(message : Any) extends RealCommand
  
  case class FromPedestrian(message : Any) extends RealCommand
  case class FromCar(message : Any) extends RealCommand
  case class FromBus(message : Any) extends RealCommand
  case class FromTram(message : Any) extends RealCommand
  
  // permette la scelta dell'handler appropriato
  case class ToBusStop(command : RealCommand) extends Command
  case class ToCrossroad(command : RealCommand) extends Command
  case class ToLane(command : RealCommand) extends Command
  case class ToPedestrianCrossroad(command : RealCommand) extends Command
  case class ToRoad(command : RealCommand) extends Command
  case class ToTramStop(command : RealCommand) extends Command
  case class ToZone(command : RealCommand) extends Command
  
  case class ToPedestrian(command : RealCommand) extends Command
  case class ToCar(command : RealCommand) extends Command
  case class ToBus(command : RealCommand) extends Command
  case class ToTram(command : RealCommand) extends Command
  
  // primo messaggio, lanciato dall'injector a ciascuna entità immobile
  case class Identity(id : String) extends Command
  // richiesta di indirizzo IP
  case object IpRequest extends Command
  // risposta con indirizzo IP
  case class IpResponse(ipAddress : String) extends Command
  // comando di creazione di una entità mobile
  case class CreateMobileEntity(id : String, route : route) extends Command
  // comando di ri-creazione di un attore mobile
  case class ReCreateMe(id : String) extends Command
  // comando per l'iniezione di un percorso
  case class Route(route : route) extends Command
  // comando per la creazione degli attori mobili sotto la propria gestione
  case object ReCreateMobileEntities extends Command
  // comando lanciato da un attore mobile a se stesso al termine di uno step
  case object PersistAndNextStep extends Command
  // comandi per la gestione della lista delle entità mobili gestite
  case class MobileEntityAdd(id : String) extends Command
  case class MobileEntityRemove(id : String) extends Command
  // comando per l'esecuzione dello step corrente
  case object ExecuteCurrentStep extends Command
  // comando inviato dall'entità mobile a quella immobile per farla entrare in stato dormiente
  case class PauseExecution(wakeupTime : TimeValue) extends Command
  // comando inviato dall'entità immobile alle entità mobili create per far riprendere a loro l'esecuzione dei loro step
  case object ResumeExecution extends Command
  // messaggio per gestire il campo lastVehicle della lane
  case object HandleLastVehicle extends Command
  // messaggio per richiedere l'actorref dato un id
  case class MovableActorRequest(id : String) extends Command
  // messaggio di risposta associato
  case class MovableActorResponse(id : String, ref : ActorRef) extends Command
  
  // inviato da un veicolo alla lane per recuperare l'actorref di chi gli sta davanti (prima volta)
  case object NextVehicleFirstRequest
  // inviato da un veicolo alla lane per recuperare l'actorref di chi gli sta davanti
  // vi è un flag booleano per capire se il veicolo richiedente è l'ultimo della lane o no
  case class NextVehicleRequest(id : String, last : Boolean)
  // inviato dalla lane ad un veicolo per fornire l'actorref di chi sta davanti
  case class NextVehicleResponse(id : String, ref : ActorRef)
  
  // inviato da un veicolo ad un altro veicolo per chiedere la ricezione della posizione
  case object SuccessorArrived
  // inviato da un veicolo ad un altro veicolo per fornire l'ultima posizione in push
  case class Advanced(lastPosition : point)
  // inviato da un veicolo ad un altro per segnalare di non dover più porre attenzione alla posizione (corsia libera)
  case object PredecessorGone
  
  // inviato da una entità mobile all'entità precedente della lane per avvisare che si è arrivati nella nuova lane
  case class VehicleBusy(comingFrom : String)
  // inviato da una entità mobile all'entità precedente della lane per avvisare che la soglia è stata superata
  case class VehicleFree(comingFrom : String)
  // inviato da un pedone alle strisce per notificare l'avvenuto passaggio
  case object CrossFree
  
  
  // inviato da un veicolo per richiedere di passare
  case class Vehicle_In(comingFrom : String)
  // inviato da un pedone per richiedere di passare
  case object Cross_In
  // inviato dalle strisce per garantire il passaggio ad un veicolo
  case object Vehicle_Out
  // inviato dalle strisce per garantire il passaggio ad un pedone
  case object Cross_Out
  
  
  
  // evento per il filtro dei duplicati
  case class NoDuplicate(senderId : String, deliveryId : Long) extends Event
  
  // evento per la definizione dell'entità immobile
  case class IdentityArrived(id : String) extends Event
  // attore mobile aggiunto in gestione
  case class MobileEntityArrived(id : String) extends Event
  // attore mobile rimosso dall gestione
  case class MobileEntityGone(id : String) extends Event
  // attore mobile aggiunto alla lista dei dormienti
  case class MobileEntitySleeping(id : String, wakeupTime : TimeValue) extends Event
  // attore mobile rimosso dalla lista dei dormienti
  case class MobileEntityWakingUp(id : String) extends Event
  // evento per l'avanzamento dell'indice del percorso
  case object NextStepEvent extends Event
  // evento per l'arrivo del percorso all'entità mobile
  case class RouteArrived(route : route) extends Event
  
  case class BusStopEvent(event : Any) extends Event
  case class CrossroadEvent(event : Any) extends Event
  case class LaneEvent(event : Any) extends Event
  case class PedestrianCrossroadEvent(event : Any) extends Event
  case class RoadEvent(event : Any) extends Event
  case class TramStopEvent(event : Any) extends Event
  case class ZoneEvent(event : Any) extends Event
  
  case class PedestrianEvent(event : Any) extends Event
  case class CarEvent(event : Any) extends Event
  case class BusEvent(event : Any) extends Event
  case class TramEvent(event : Any) extends Event
  
  case class NextVehicleIdArrived(id : String)
  case object NextVehicleGone
  
  case class PredecessorArrived(id : String) extends Event
  case object PredecessorGoneNotSentYet extends Event
  case object PredecessorGoneSent extends Event
  
  case class VehicleFreeArrived(id : String)
  case class VehicleBusyArrived(id : String)
  
  // effettua l'imbustamento giusto rispetto alle entità coinvolte
  def envelope(senderId : String, destinationId : String, message : Any) : Command = {
    destinationId.substring(0, 3) match {
      case "PED" =>
        senderId.substring(0, 3) match {
          case "PED" =>
            return ToPedestrian(FromPedestrian(message))
          case "CAR" =>
            return ToPedestrian(FromCar(message))
          case "BUS" =>
            return ToPedestrian(FromBus(message))
          case "TRA" =>
            return ToPedestrian(FromTram(message))
          case _ =>
            senderId.charAt(0) match {
              case 'R' =>
                return ToPedestrian(FromRoad(message))
              case 'L' =>
                return ToPedestrian(FromLane(message))
              case 'C' =>
                return ToPedestrian(FromCrossroad(message))
              case 'P' =>
                return ToPedestrian(FromPedestrianCrossroad(message))
              case 'B' =>
                return ToPedestrian(FromBus(message))
              case 'T' =>
                return ToPedestrian(FromTram(message))
              case 'Z' =>
                return ToPedestrian(FromZone(message))
          }  
        }
      case "CAR" =>
        senderId.substring(0, 3) match {
          case "PED" =>
            return ToCar(FromPedestrian(message))
          case "CAR" =>
            return ToCar(FromCar(message))
          case "BUS" =>
            return ToCar(FromBus(message))
          case "TRA" =>
            return ToCar(FromTram(message))
          case _ =>
            senderId.charAt(0) match {
              case 'R' =>
                return ToCar(FromRoad(message))
              case 'L' =>
                return ToCar(FromLane(message))
              case 'C' =>
                return ToCar(FromCrossroad(message))
              case 'P' =>
                return ToCar(FromPedestrianCrossroad(message))
              case 'B' =>
                return ToCar(FromBus(message))
              case 'T' =>
                return ToCar(FromTram(message))
              case 'Z' =>
                return ToCar(FromZone(message))
          }  
        }
      case "BUS" =>
        senderId.substring(0, 3) match {
          case "PED" =>
            return ToBus(FromPedestrian(message))
          case "CAR" =>
            return ToBus(FromCar(message))
          case "BUS" =>
            return ToBus(FromBus(message))
          case "TRA" =>
            return ToBus(FromTram(message))
          case _ =>
            senderId.charAt(0) match {
              case 'R' =>
                return ToBus(FromRoad(message))
              case 'L' =>
                return ToBus(FromLane(message))
              case 'C' =>
                return ToBus(FromCrossroad(message))
              case 'P' =>
                return ToBus(FromPedestrianCrossroad(message))
              case 'B' =>
                return ToBus(FromBus(message))
              case 'T' =>
                return ToBus(FromTram(message))
              case 'Z' =>
                return ToBus(FromZone(message))
          }  
        }
      case "TRA" =>
        senderId.substring(0, 3) match {
          case "PED" =>
            return ToTram(FromPedestrian(message))
          case "CAR" =>
            return ToTram(FromCar(message))
          case "BUS" =>
            return ToTram(FromBus(message))
          case "TRA" =>
            return ToTram(FromTram(message))
          case _ =>
            senderId.charAt(0) match {
              case 'R' =>
                return ToTram(FromRoad(message))
              case 'L' =>
                return ToTram(FromLane(message))
              case 'C' =>
                return ToTram(FromCrossroad(message))
              case 'P' =>
                return ToTram(FromPedestrianCrossroad(message))
              case 'B' =>
                return ToTram(FromBus(message))
              case 'T' =>
                return ToTram(FromTram(message))
              case 'Z' =>
                return ToTram(FromZone(message))
          }  
        }
      case _ =>
        destinationId.charAt(0) match {
          case 'R' =>
            senderId.substring(0, 3) match {
              case "PED" =>
                return ToRoad(FromPedestrian(message))
              case "CAR" =>
                return ToRoad(FromCar(message))
              case "BUS" =>
                return ToRoad(FromBus(message))
              case "TRA" =>
                return ToRoad(FromTram(message))
              case _ =>
                senderId.charAt(0) match {
                  case 'R' =>
                    return ToRoad(FromRoad(message))
                  case 'L' =>
                    return ToRoad(FromLane(message))
                  case 'C' =>
                    return ToRoad(FromCrossroad(message))
                  case 'P' =>
                    return ToRoad(FromPedestrianCrossroad(message))
                  case 'B' =>
                    return ToRoad(FromBus(message))
                  case 'T' =>
                    return ToRoad(FromTram(message))
                  case 'Z' =>
                    return ToRoad(FromZone(message))
              }  
            }
          case 'L' =>
            senderId.substring(0, 3) match {
              case "PED" =>
                return ToLane(FromPedestrian(message))
              case "CAR" =>
                return ToLane(FromCar(message))
              case "BUS" =>
                return ToLane(FromBus(message))
              case "TRA" =>
                return ToLane(FromTram(message))
              case _ =>
                senderId.charAt(0) match {
                  case 'R' =>
                    return ToLane(FromRoad(message))
                  case 'L' =>
                    return ToLane(FromLane(message))
                  case 'C' =>
                    return ToLane(FromCrossroad(message))
                  case 'P' =>
                    return ToLane(FromPedestrianCrossroad(message))
                  case 'B' =>
                    return ToLane(FromBus(message))
                  case 'T' =>
                    return ToLane(FromTram(message))
                  case 'Z' =>
                    return ToLane(FromZone(message))
              }  
            }
          case 'C' =>
            senderId.substring(0, 3) match {
              case "PED" =>
                return ToCrossroad(FromPedestrian(message))
              case "CAR" =>
                return ToCrossroad(FromCar(message))
              case "BUS" =>
                return ToCrossroad(FromBus(message))
              case "TRA" =>
                return ToCrossroad(FromTram(message))
              case _ =>
                senderId.charAt(0) match {
                  case 'R' =>
                    return ToCrossroad(FromRoad(message))
                  case 'L' =>
                    return ToCrossroad(FromLane(message))
                  case 'C' =>
                    return ToCrossroad(FromCrossroad(message))
                  case 'P' =>
                    return ToCrossroad(FromPedestrianCrossroad(message))
                  case 'B' =>
                    return ToCrossroad(FromBus(message))
                  case 'T' =>
                    return ToCrossroad(FromTram(message))
                  case 'Z' =>
                    return ToCrossroad(FromZone(message))
              }  
            }
          case 'P' =>
            senderId.substring(0, 3) match {
              case "PED" =>
                return ToPedestrianCrossroad(FromPedestrian(message))
              case "CAR" =>
                return ToPedestrianCrossroad(FromCar(message))
              case "BUS" =>
                return ToPedestrianCrossroad(FromBus(message))
              case "TRA" =>
                return ToPedestrianCrossroad(FromTram(message))
              case _ =>
                senderId.charAt(0) match {
                  case 'R' =>
                    return ToPedestrianCrossroad(FromRoad(message))
                  case 'L' =>
                    return ToPedestrianCrossroad(FromLane(message))
                  case 'C' =>
                    return ToPedestrianCrossroad(FromCrossroad(message))
                  case 'P' =>
                    return ToPedestrianCrossroad(FromPedestrianCrossroad(message))
                  case 'B' =>
                    return ToPedestrianCrossroad(FromBus(message))
                  case 'T' =>
                    return ToPedestrianCrossroad(FromTram(message))
                  case 'Z' =>
                    return ToPedestrianCrossroad(FromZone(message))
              }  
            }
          case 'B' =>
            senderId.substring(0, 3) match {
              case "PED" =>
                return ToBusStop(FromPedestrian(message))
              case "CAR" =>
                return ToBusStop(FromCar(message))
              case "BUS" =>
                return ToBusStop(FromBus(message))
              case "TRA" =>
                return ToBusStop(FromTram(message))
              case _ =>
                senderId.charAt(0) match {
                  case 'R' =>
                    return ToBusStop(FromRoad(message))
                  case 'L' =>
                    return ToBusStop(FromLane(message))
                  case 'C' =>
                    return ToBusStop(FromCrossroad(message))
                  case 'P' =>
                    return ToBusStop(FromPedestrianCrossroad(message))
                  case 'B' =>
                    return ToBusStop(FromBus(message))
                  case 'T' =>
                    return ToBusStop(FromTram(message))
                  case 'Z' =>
                    return ToBusStop(FromZone(message))
              }  
            }
          case 'T' =>
            senderId.substring(0, 3) match {
              case "PED" =>
                return ToTramStop(FromPedestrian(message))
              case "CAR" =>
                return ToTramStop(FromCar(message))
              case "BUS" =>
                return ToTramStop(FromBus(message))
              case "TRA" =>
                return ToTramStop(FromTram(message))
              case _ =>
                senderId.charAt(0) match {
                  case 'R' =>
                    return ToTramStop(FromRoad(message))
                  case 'L' =>
                    return ToTramStop(FromLane(message))
                  case 'C' =>
                    return ToTramStop(FromCrossroad(message))
                  case 'P' =>
                    return ToTramStop(FromPedestrianCrossroad(message))
                  case 'B' =>
                    return ToTramStop(FromBus(message))
                  case 'T' =>
                    return ToTramStop(FromTram(message))
                  case 'Z' =>
                    return ToTramStop(FromZone(message))
              }  
            }
          case 'Z' =>
            senderId.substring(0, 3) match {
              case "PED" =>
                return ToZone(FromPedestrian(message))
              case "CAR" =>
                return ToZone(FromCar(message))
              case "BUS" =>
                return ToZone(FromBus(message))
              case "TRA" =>
                return ToZone(FromTram(message))
              case _ =>
                senderId.charAt(0) match {
                  case 'R' =>
                    return ToZone(FromRoad(message))
                  case 'L' =>
                    return ToZone(FromLane(message))
                  case 'C' =>
                    return ToZone(FromCrossroad(message))
                  case 'P' =>
                    return ToZone(FromPedestrianCrossroad(message))
                  case 'B' =>
                    return ToZone(FromBus(message))
                  case 'T' =>
                    return ToZone(FromTram(message))
                  case 'Z' =>
                    return ToZone(FromZone(message))
              }  
            }
        }
    }
  }
}
