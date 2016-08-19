package main

import akka.actor.{DeadLetter, ActorLogging, Actor}
import common.ToPersistentMessages._
import modelActors.Messages._
import common.CommonMessages.{Ack,Request}


/**
 * Created by Alberto on 8/19/2016.
 */
class DeadLetterListener extends Actor with ActorLogging {

  override def receive = {
    case DeadLetter(msg, from, to) =>
      var log : String = from + " => " + to + ": "
      var flag = true
      /*msg match {
        case common.CommonMessages.ToMovable(destId,msg2) =>
          msg2 match {
            case FromImmovable(senderId, payload) => log = log + senderId + "\n"
              payload match{
                case  Ack(deliveryId)=>
                case  Request(deliveryId, command) =>
                  command match {
                    case IpRequest =>
                      log = log + "IpRequest"
                    case IpResponse(_) =>
                      log = log + "IpResponse"
                    case ReCreateMe(id, _) =>
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
                    case PauseExecution(_, _) =>
                      log = log + "PauseExecution"
                    case ResumeExecution =>
                      log = log + "ResumeExecution"
                    case MovableActorRequest(_) =>
                      log = log + "MovableActorRequest"
                    case MovableActorResponse(_, _) =>
                      log = log + "MovableActorResponse"
                    case MovableStateSnapshotOffer(snapshot) =>
                      log = log + "MovableStateSnapshotOffer"
                    case _ =>
                      val content = develope(command)
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
                        case PredecessorArrived(laneId) =>
                          log = log + "PredecessorArrived(" + laneId + ")"
                        case Advanced(laneId, lastPosition) =>
                          log = log + "Advanced(" + lastPosition + ")"
                        case PredecessorGone(laneId) =>
                          log = log + "PredecessorGone(" + laneId + ")"
                        case SuccessorGone(laneId) =>
                          log = log + "SuccessorGone(" + laneId + ")"
                        case PredecessorChanged(laneId, predecessorId, predecessorRef) =>
                          log = log + "PredecessorChanged(" + laneId + ")"
                        case SuccessorChanged(laneId, successorId, successorRef) =>
                          log = log + "SuccessorChanged(" + laneId + ")"
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
                        case WaitForPublicTransport(destination, _) =>
                          log = log + "WaitForPublicTransport"
                        case GetOut(travellers, numTravellers) =>
                          log = log + "GetOut"
                        case GetIn(travellers) =>
                          log = log + "GetIn"
                        case RemovePosition =>
                          log = log + "RemovePosition"
                        case _ =>
                          println("PORCO DIO CANE")
                      }
                  }
              }
            case FromMovable(senderId, senderRef, payload) => println("FromMovable")
            case FromNonPersistent(senderRef, command) => println("FromNonPersistent")
          }
        case _ => println(msg.toString + " cosa sea sta roba")
      }*/
      if(flag) {
        println(log + msg.toString)
      }

  }
}
