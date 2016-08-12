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
  // messaggio per gestire il campo lastVehicle della lane
  case object HandleLastVehicle
  // messaggio per dichiararsi come ultimo della lane
  case object LastOfTheLane
  // inviato da un veicolo per chiedere di entrare nella lane a partire da una zona
  case class LaneAccessRequest(startPosition : point, direction : direction)
  // inviato dalla lane per concedere l'accesso ad un veicolo che voleva entrare da una zona
  case class LaneAccessGranted(predecessorId : String, predecessorRef : ActorRef, successorId : String, successorRef : ActorRef)
  
  // inviato da un veicolo ad un altro veicolo per chiedere la ricezione della posizione
  case class SuccessorArrived(laneId : String)
  // inviato da un veicolo per notificare ad un altro che da ora in poi dovrà regolarsi sull'avanzamento
  case object PredecessorArrived
  // inviato da un veicolo ad un altro veicolo per fornire l'ultima posizione in push
  // viene inviato anche alla lane per gestire le posizioni
  case class Advanced(lastPosition : point)
  // inviato da un veicolo ad un altro per segnalare di non dover più porre attenzione alla posizione (corsia libera)
  case object PredecessorGone
  // inviato da un veicolo al successivo per segnalare di non dover più mandare update della posizione
  case object SuccessorGone
  // inviato per notificare un cambio di successore
  case class SuccessorChanged(successorId : String, successorRef : ActorRef)
  // inviato per notificare un cambio di predecessore
  case class PredecessorChanged(predecessorId : String, predecessorRef : ActorRef)
  
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
  
  // messaggio inviato da un pedone per attendere alla fermata del bus
  case class WaitForPublicTransport(destination : String)
  // messaggio inviato da un bus ad una bus stop per passargli chi sta scendendo e quanti passeggeri sono ancora a bordo
  case class GetOut(travellers : List[String], numTravellers : Int)
  // messaggio inviato dalla bus stop al bus per passargli chi sta salendo
  case class GetIn(travellers : List[(String, String)])
  
  
  
  // evento per il filtro dei duplicati
  case class NoDuplicate(senderId : String, deliveryId : Long) extends Event
  
  // evento per il salvataggio del deliveryId corrente
  case class PersistDeliveryId(deliveryId : Long) extends Event
  
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
  case object PreviousVehicleGone
  case object PredecessorGoneNotSentYet extends Event
  case object PredecessorGoneSent extends Event
  
  case class VehicleFreeArrived(id : String)
  case class VehicleBusyArrived(id : String)
  
  case class TravellersGoneOff(travellers : List[String])
  case class TravellersGoneOn(travellers : List[(String, String)])
  
  case class PreviousLaneChanged(laneId : String) extends Event
  
  // stampa una stringa di logging del messaggio
  def printMessage(senderId : String, destinationId : String, message : Command) : Unit = {
    var log : String = senderId + " => " + destinationId + ": "
    var flag = false
    message match {
      case IpRequest =>
        log = log + "IpRequest"
      case IpResponse(_) =>
        log = log + "IpResponse"
      case ReCreateMe(id) =>
          log = log + "ReCreateMe"
      case Route(_) =>
        log = log + "Route"
      case ReCreateMobileEntities =>
        log = log + "ReCreateMobileEntities"
      case PersistAndNextStep =>
        log = log + "PersistAndNextStep"
      case MobileEntityAdd(_) =>
        log = log + "MobileEntityAdd"
      case MobileEntityRemove(_) =>
        log = log + "MobileEntityRemove"
      case ExecuteCurrentStep =>
        log = log + "ExecuteCurrentStep"
      case PauseExecution(_) =>
        log = log + "PauseExecution"
      case ResumeExecution =>
        log = log + "ResumeExecution"
      case MovableActorRequest(_) =>
        log = log + "MovableActorRequest"
      case MovableActorResponse(_, _) =>
        log = log + "MovableActorResponse"
      case _ =>
        val content = develope(message)
        content match {
          case NextVehicleFirstRequest =>
            log = log + "NextVehicleFirstRequest"
          case NextVehicleRequest(id, last) =>
            log = log + "NextVehicleRequest"
          case NextVehicleResponse(id, ref) =>
            log = log + "NextVehicleResponse"
          case HandleLastVehicle =>
            log = log + "HandleLastVehicle"
          case LastOfTheLane =>
            log = log + "LastOfTheLane"
          case LaneAccessRequest(startPosition, direction) =>
            log = log + "LaneAccessRequest"
          case LaneAccessGranted(predecessorId, predecessorRef, successorId, successorRef) =>
            log = log + "LaneAccessGranted"
          case SuccessorArrived(laneId) =>
            log = log + "SuccessorArrived(" + laneId + ")"
          case PredecessorArrived =>
            log = log + "PredecessorArrived"
          case Advanced(lastPosition) =>
            log = log + "Advanced(" + lastPosition + ")"
            if(destinationId.startsWith("L")) {
              flag = true
            }
          case PredecessorGone =>
            log = log + "PredecessorGone"
          case SuccessorGone =>
            log = log + "SuccessorGone"
          case PredecessorChanged =>
            log = log + "PredecessorChanged"
          case SuccessorChanged =>
            log = log + "SuccessorChanged"
          case VehicleBusy(comingFrom) =>
            log = log + "VehicleBusy(" + comingFrom + ")"
          case VehicleFree(comingFrom) =>
            log = log + "VehicleFree(" + comingFrom + ")"
          case CrossFree =>
            log = log + "CrossFree"
          case Vehicle_In(comingFrom) =>
            log = log + "VehicleIn(" + comingFrom + ")"
          case Cross_In =>
            log = log + "CrossIn"
          case Vehicle_Out =>
            log = log + "VehicleOut"
          case Cross_Out =>
            log = log + "CrossOut"
          case WaitForPublicTransport(destination) =>
            log = log + "WaitForPublicTransport"
          case GetOut(travellers, numTravellers) =>
            log = log + "GetOut"
          case GetIn(travellers) =>
            log = log + "GetIn"
        }
    }
    if(flag == false) {
      println(log)
    }
  }
  
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
                return ToPedestrian(FromBusStop(message))
              case 'T' =>
                return ToPedestrian(FromTramStop(message))
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
                return ToCar(FromBusStop(message))
              case 'T' =>
                return ToCar(FromTramStop(message))
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
                return ToBus(FromBusStop(message))
              case 'T' =>
                return ToBus(FromTramStop(message))
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
                return ToTram(FromBusStop(message))
              case 'T' =>
                return ToTram(FromTramStop(message))
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
                    return ToRoad(FromBusStop(message))
                  case 'T' =>
                    return ToRoad(FromTramStop(message))
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
                    return ToLane(FromBusStop(message))
                  case 'T' =>
                    return ToLane(FromTramStop(message))
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
                    return ToCrossroad(FromBusStop(message))
                  case 'T' =>
                    return ToCrossroad(FromTramStop(message))
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
                    return ToPedestrianCrossroad(FromBusStop(message))
                  case 'T' =>
                    return ToPedestrianCrossroad(FromTramStop(message))
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
                    return ToBusStop(FromBusStop(message))
                  case 'T' =>
                    return ToBusStop(FromTramStop(message))
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
                    return ToTramStop(FromBusStop(message))
                  case 'T' =>
                    return ToTramStop(FromTramStop(message))
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
                    return ToZone(FromBusStop(message))
                  case 'T' =>
                    return ToZone(FromTramStop(message))
                  case 'Z' =>
                    return ToZone(FromZone(message))
              }  
            }
        }
    }
  }
  
  // UTILITY
  // effettua il deimbustamento di un comando per ottenere il messaggio
  def develope(command : Command) : Any = {
	  command match {
  	  case ToPedestrian(FromPedestrian(message)) =>
        return message
  	  case ToPedestrian(FromCar(message)) =>
        return message
  	  case ToPedestrian(FromBus(message)) =>
        return message
  	  case ToPedestrian(FromTram(message)) =>
        return message
  	  case ToPedestrian(FromRoad(message)) =>
        return message
  	  case ToPedestrian(FromLane(message)) =>
        return message
  	  case ToPedestrian(FromCrossroad(message)) =>
        return message
  	  case ToPedestrian(FromPedestrianCrossroad(message)) =>
        return message
  	  case ToPedestrian(FromBusStop(message)) =>
        return message
  	  case ToPedestrian(FromTramStop(message)) =>
        return message
  	  case ToPedestrian(FromZone(message)) =>
        return message
  	  case ToCar(FromPedestrian(message)) =>
        return message
  	  case ToCar(FromCar(message)) =>
        return message
  	  case ToCar(FromBus(message)) =>
        return message
  	  case ToCar(FromTram(message)) =>
        return message
  	  case ToCar(FromRoad(message)) =>
        return message
  	  case ToCar(FromLane(message)) =>
        return message
  	  case ToCar(FromCrossroad(message)) =>
        return message
  	  case ToCar(FromPedestrianCrossroad(message)) =>
        return message
  	  case ToCar(FromBusStop(message)) =>
        return message
  	  case ToCar(FromTramStop(message)) =>
        return message
  	  case ToCar(FromZone(message)) =>
        return message
  	  case ToBus(FromPedestrian(message)) =>
        return message
  	  case ToBus(FromCar(message)) =>
        return message
  	  case ToBus(FromBus(message)) =>
        return message
  	  case ToBus(FromTram(message)) =>
        return message
  	  case ToBus(FromRoad(message)) =>
        return message
  	  case ToBus(FromLane(message)) =>
        return message
  	  case ToBus(FromCrossroad(message)) =>
        return message
  	  case ToBus(FromPedestrianCrossroad(message)) =>
        return message
  	  case ToBus(FromBusStop(message)) =>
        return message
  	  case ToBus(FromTramStop(message)) =>
        return message
  	  case ToBus(FromZone(message)) =>
        return message
  	  case ToTram(FromPedestrian(message)) =>
        return message
  	  case ToTram(FromCar(message)) =>
        return message
  	  case ToTram(FromBus(message)) =>
        return message
  	  case ToTram(FromTram(message)) =>
        return message
  	  case ToTram(FromRoad(message)) =>
        return message
  	  case ToTram(FromLane(message)) =>
        return message
  	  case ToTram(FromCrossroad(message)) =>
        return message
  	  case ToTram(FromPedestrianCrossroad(message)) =>
        return message
  	  case ToTram(FromBusStop(message)) =>
        return message
  	  case ToTram(FromTramStop(message)) =>
        return message
  	  case ToTram(FromZone(message)) =>
        return message
  	  case ToRoad(FromPedestrian(message)) =>
        return message
  	  case ToRoad(FromCar(message)) =>
        return message
  	  case ToRoad(FromBus(message)) =>
        return message
  	  case ToRoad(FromTram(message)) =>
        return message
  	  case ToRoad(FromRoad(message)) =>
        return message
  	  case ToRoad(FromLane(message)) =>
        return message
  	  case ToRoad(FromCrossroad(message)) =>
        return message
  	  case ToRoad(FromPedestrianCrossroad(message)) =>
        return message
  	  case ToRoad(FromBusStop(message)) =>
        return message
  	  case ToRoad(FromTramStop(message)) =>
        return message
  	  case ToRoad(FromZone(message)) =>
        return message
  	  case ToLane(FromPedestrian(message)) =>
        return message
  	  case ToLane(FromCar(message)) =>
        return message
  	  case ToLane(FromBus(message)) =>
        return message
  	  case ToLane(FromTram(message)) =>
        return message
  	  case ToLane(FromRoad(message)) =>
        return message
  	  case ToLane(FromLane(message)) =>
        return message
  	  case ToLane(FromCrossroad(message)) =>
        return message
  	  case ToLane(FromPedestrianCrossroad(message)) =>
        return message
  	  case ToLane(FromBusStop(message)) =>
        return message
  	  case ToLane(FromTramStop(message)) =>
        return message
  	  case ToLane(FromZone(message)) =>
        return message
  	  case ToCrossroad(FromPedestrian(message)) =>
        return message
  	  case ToCrossroad(FromCar(message)) =>
        return message
  	  case ToCrossroad(FromBus(message)) =>
        return message
  	  case ToCrossroad(FromTram(message)) =>
        return message
  	  case ToCrossroad(FromRoad(message)) =>
        return message
  	  case ToCrossroad(FromLane(message)) =>
        return message
  	  case ToCrossroad(FromCrossroad(message)) =>
        return message
  	  case ToCrossroad(FromPedestrianCrossroad(message)) =>
        return message
  	  case ToCrossroad(FromBusStop(message)) =>
        return message
  	  case ToCrossroad(FromTramStop(message)) =>
        return message
  	  case ToCrossroad(FromZone(message)) =>
        return message
  	  case ToPedestrianCrossroad(FromPedestrian(message)) =>
        return message
  	  case ToPedestrianCrossroad(FromCar(message)) =>
        return message
  	  case ToPedestrianCrossroad(FromBus(message)) =>
        return message
  	  case ToPedestrianCrossroad(FromTram(message)) =>
        return message
  	  case ToPedestrianCrossroad(FromRoad(message)) =>
        return message
  	  case ToPedestrianCrossroad(FromLane(message)) =>
        return message
  	  case ToPedestrianCrossroad(FromCrossroad(message)) =>
        return message
  	  case ToPedestrianCrossroad(FromPedestrianCrossroad(message)) =>
        return message
  	  case ToPedestrianCrossroad(FromBusStop(message)) =>
        return message
  	  case ToPedestrianCrossroad(FromTramStop(message)) =>
        return message
  	  case ToPedestrianCrossroad(FromZone(message)) =>
        return message
  	  case ToBusStop(FromPedestrian(message)) =>
        return message
  	  case ToBusStop(FromCar(message)) =>
        return message
  	  case ToBusStop(FromBus(message)) =>
        return message
  	  case ToBusStop(FromTram(message)) =>
        return message
  	  case ToBusStop(FromRoad(message)) =>
        return message
  	  case ToBusStop(FromLane(message)) =>
        return message
  	  case ToBusStop(FromCrossroad(message)) =>
        return message
  	  case ToBusStop(FromPedestrianCrossroad(message)) =>
        return message
  	  case ToBusStop(FromBusStop(message)) =>
        return message
  	  case ToBusStop(FromTramStop(message)) =>
        return message
  	  case ToBusStop(FromZone(message)) =>
        return message
  	  case ToTramStop(FromPedestrian(message)) =>
        return message
  	  case ToTramStop(FromCar(message)) =>
        return message
  	  case ToTramStop(FromBus(message)) =>
        return message
  	  case ToTramStop(FromTram(message)) =>
        return message
  	  case ToTramStop(FromRoad(message)) =>
        return message
  	  case ToTramStop(FromLane(message)) =>
        return message
  	  case ToTramStop(FromCrossroad(message)) =>
        return message
  	  case ToTramStop(FromPedestrianCrossroad(message)) =>
        return message
  	  case ToTramStop(FromBusStop(message)) =>
        return message
  	  case ToTramStop(FromTramStop(message)) =>
        return message
  	  case ToTramStop(FromZone(message)) =>
        return message
  	  case ToZone(FromPedestrian(message)) =>
        return message
  	  case ToZone(FromCar(message)) =>
        return message
  	  case ToZone(FromBus(message)) =>
        return message
  	  case ToZone(FromTram(message)) =>
        return message
  	  case ToZone(FromRoad(message)) =>
        return message
  	  case ToZone(FromLane(message)) =>
        return message
  	  case ToZone(FromCrossroad(message)) =>
        return message
  	  case ToZone(FromPedestrianCrossroad(message)) =>
        return message
  	  case ToZone(FromBusStop(message)) =>
        return message
  	  case ToZone(FromTramStop(message)) =>
        return message
  	  case ToZone(FromZone(message)) =>
        return message
	  }
  }
}
