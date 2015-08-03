package coreActors

import akka.actor.{Actor, ActorLogging}
import websocket.WebSocket

import scala.collection.mutable

/**
 * Created by Alberto on 28/07/2015.
 */

object ActiveConnections {

  sealed trait ConnectionMessage

  case class Unregister(ws: WebSocket) extends ConnectionMessage

  case class SendMessageToClients(message: String) extends ConnectionMessage

}

/*classe che gestisce l'elenco dei browser connessi tramite WebSocket
Per Inviare un messaggio a tutti i browser basta utilizzare il metodo
SendMessageToClients con la stringa contenente il messaggio.
La stringa deve essere già stata formattata in JSON
*/
class ActiveConnections extends Actor with ActorLogging {

  val clients = mutable.ListBuffer[WebSocket]()

  override def receive = {
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
      if (null != ws)
        log.debug("url {} received msg '{}'", ws.path, msg)

      //elimina il websocket
    case ActiveConnections.Unregister(ws) =>
      if (null != ws) {
        clients -= ws
        log.debug("unregister monitor")
      }

    case ActiveConnections.SendMessageToClients(message) =>
      for(client <- clients) client.send(message)
  }

}
