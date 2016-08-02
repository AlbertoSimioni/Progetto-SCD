package coreActors

import akka.actor.{Actor, ActorLogging}
import map.Domain._
import messagesFormatter.BrowserMessagesFormatter
import pubsub.Messages.{hidePedestrian, hideCar}
import time.TimeCounter
import websocket.WebSocket
import org.json4s._
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._
import java.io._
import scala.collection.mutable
/**
 * Created by Alberto on 28/07/2015.
 */

object ActiveConnections {

  sealed trait ConnectionMessage

  case class Unregister(ws: WebSocket) extends ConnectionMessage

  case class SendMessageToClients(message: String) extends ConnectionMessage

  case class updateSemaphoreState(id: String, state: String) extends ConnectionMessage

  case class entityPath(id: String, path: String) extends ConnectionMessage
}

/*classe che gestisce l'elenco dei browser connessi tramite WebSocket
Per Inviare un messaggio a tutti i browser basta utilizzare il metodo
SendMessageToClients con la stringa contenente il messaggio.
La stringa deve essere già stata formattata in JSON
*/
class ActiveConnections extends Actor with ActorLogging {
  val clients = mutable.ListBuffer[WebSocket]()
  val semaphoreStates = mutable.HashMap[String,String]()
  val entityPaths = mutable.HashMap[String,String]()
  val zoneStates = mutable.HashMap[String,String]()

  override def receive = {


    case m @ hideCar(id,zoneID) =>
      var zoneState = zoneStates.get(zoneID)
      var cars = 0;
      var pedestrians = 0;
      if(!zoneState.isEmpty) {
        var state = zoneState.get
        val carsString = state.split("-")(1)
        val pedestriansString = state.split("-")(0)
        cars = carsString.toInt
        pedestrians = pedestriansString.toInt
      }
      cars += 1
      zoneStates.put(zoneID,pedestrians.toString() + "-" + cars.toString());
      self ! ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.HideCarToJson(m))

    case m @ hidePedestrian(id,zoneID,inVehicle) =>
      if(!inVehicle) {
        var zoneState = zoneStates.get(zoneID)
        var cars = 0;
        var pedestrians = 0;
        if (!zoneState.isEmpty) {
          var state = zoneState.get
          val carsString = state.split("-")(1)
          val pedestriansString = state.split("-")(0)
          cars = carsString.toInt
          pedestrians = pedestriansString.toInt
        }
        pedestrians += 1
        zoneStates.put(zoneID, pedestrians.toString() + "-" + cars.toString());
      }
      self ! ActiveConnections.SendMessageToClients(BrowserMessagesFormatter.HidePedestrianToJson(m))

    case pubsub.Messages.entityAwaked(entityID,zoneID) =>
      var zoneState = zoneStates.get(zoneID)
      var cars = 0;
      var pedestrians = 0;
      if(!zoneState.isEmpty) {
        var state = zoneState.get
        val carsString = state.split("-")(1)
        val pedestriansString = state.split("-")(0)
        cars = carsString.toInt
        pedestrians = pedestriansString.toInt
      }
      if(entityID.substring(0,1) == "C"){
        cars -=1;
      }
      else pedestrians -=1
      zoneStates.put(zoneID,pedestrians.toString() + "-" + cars.toString());

    case WebSocket.Open(ws) =>
      if (null != ws) {
        clients += ws
        log.debug("registered monitor for url {}", ws.path)
        //qua dovrei inserire il codice per inviare al browser tutte le informazioni iniziali
      }

      //Sia in caso di chiusura normale che di errore di connessione effettuo le stesse operazioni
      //quindi chiama su se stesso lo stesso messaggio
    case WebSocket.Close(ws, code, reason) =>
      self ! ActiveConnections.Unregister(ws)
    case WebSocket.Error(ws, ex) =>
      self ! ActiveConnections.Unregister(ws)

    //messaggio da parte del Client (come in AJAX) Per ora i browser non inviano messaggi
    case WebSocket.Message(ws, msg) =>
      if (null != ws) {
        log.debug("url {} received msg '{}'", ws.path, msg)
        var msgSplit = msg.split('-')
        msgSplit(0);
        if (msgSplit(0) == "MAP"){
          val path = getClass.getResource("/map.json").getPath
          val source = scala.io.Source.fromFile(new File(path))
          val environmentString = try source.getLines mkString finally source.close()
          ws.send(environmentString)
          semaphoreStates.foreach {case(key, value) => ws.send(value)}
        }
        if(msgSplit(0) == "PATH"){
          val entityId = msgSplit(1)
          val path = entityPaths.get(entityId)
          ws.send(path.get)
        }
        if(msgSplit(0) == "ZONESTATE"){
          val zoneID = msgSplit(1)
          var zoneState = zoneStates.get(zoneID)
          var cars = 0;
          var pedestrians = 0;
          if(!zoneState.isEmpty) {
            var state = zoneState.get
            val carsString = state.split("-")(1)
            val pedestriansString = state.split("-")(0)
            cars = carsString.toInt
            pedestrians = pedestriansString.toInt
          }
          ws.send( s"""{"type": "zoneState", "info":{"id":"${zoneID}","pedestrians" :"${pedestrians}","cars":"${cars}"}}""")
        }

      }

      //elimina il websocket
    case ActiveConnections.Unregister(ws) =>
      if (null != ws) {
        clients -= ws
        log.debug("unregister monitor")
      }


    case ActiveConnections.updateSemaphoreState(id, state) =>
      val semaphore = semaphoreStates.get(id)
      semaphoreStates.put(id,state)
      self ! ActiveConnections.SendMessageToClients(state)

    case ActiveConnections.entityPath(id,path) =>
      println("path received " + id)
      entityPaths.put(id,path)

    case ActiveConnections.SendMessageToClients(message) =>
      for(client <- clients) client.send(message)
  }

}
