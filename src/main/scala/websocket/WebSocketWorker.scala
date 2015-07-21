package websocket

import akka.actor.{ActorRef, Cancellable}
import api.RouteActor
import spray.can.Http
import spray.can.websocket.frame.{CloseFrame, PingFrame, PongFrame, StatusCode, TextFrame}
import spray.can.websocket.{UpgradedToWebSocket, WebSocketServerWorker}
import spray.http.HttpRequest
import spray.routing.{Rejected, RequestContext, Route}

import scala.concurrent.duration.DurationInt

//serverConnection è il sender passatogli dal rootservice,
// ogni istanza di questa classe gestisce una sola connessione
  class WebSocketWorker(val serverConnection: ActorRef, val route: Route) extends RouteActor with WebSocketServerWorker with WebSocket {
  import context.dispatcher

  override lazy val connection = serverConnection

  //route è la parte logica delle risposte del websocket
  override def receive = matchRoute(route) orElse handshaking orElse closeLogic

  private def matchRoute(route : Route) : Receive = {

    //Primo messaggio ricevuto che fa partire l'handshaking del WebSocket
    case request : HttpRequest =>
      val ctx = RequestContext(request, self, request.uri.path)
      log.debug("HTTP request for uri {}", request.uri.path)
      route(ctx.withResponder(self))
      handshaking(request)

      //Register è inviato dal service che risponde all'indirizzo della richiesta
    case WebSocket.Register(request, actor, ping) =>
      //se il websocket prevede ping pong (lo si capisce dal parametro in input), allora viene inizializzato e ogni tot si fa questo protocollo
      if (ping) pinger = Some(context.system.scheduler.scheduleOnce(110.seconds, self, WebSocket.Ping))
      handler = actor
      uripath = request.uri.path.toString
      handler ! WebSocket.Open(this)
    case Rejected(rejections) => //inviato dal toolkit uccide l'attore
      log.info("Rejecting with {}", rejections)
      context stop self
  }



  // this is the actor's behavior after the WebSocket handshaking resulted in an upgraded request
  //Sono i messaggi che arrivano in input al WebSocket dal browser oppure
  override def businessLogic = {
    //Arrivo informazioni dal browser
    case TextFrame(message) =>
      ping
      handler ! WebSocket.Message(this, message.utf8String)
    //boh
    case UpgradedToWebSocket =>
      // nothing to do
    //chiamato da un timer per mandare un PingFrame al browser
    case WebSocket.Ping =>
      send(PingFrame())
    case PongFrame(payload) =>
      ping
    case Http.Aborted =>  // questi messaggi qua sono inviati in automatico dal modulo
      handler ! WebSocket.Error(this, "aborted")
    case Http.ErrorClosed(cause) =>
      handler ! WebSocket.Error(this, cause)
    //5 divers casi in cui il websocket viene considerato chiuso
    case CloseFrame(status, reason) =>
      handler ! WebSocket.Close(this, status.code, reason)
    case Http.Closed =>
      handler ! WebSocket.Close(this, StatusCode.NormalClose.code, "")
    case Http.ConfirmedClosed =>
      handler ! WebSocket.Close(this, StatusCode.GoingAway.code, "")
    case Http.PeerClosed =>
      handler ! WebSocket.Close(this, StatusCode.GoingAway.code, "")
    case WebSocket.Release =>
      handler ! WebSocket.Close(this, StatusCode.NormalClose.code, "")
    case whatever =>
      log.debug("WebSocket received '{}'", whatever)
  }
  //funzione ausiliaria per scrivere la send più brevemente. usata solo nei test
  def send(message : String) = send(TextFrame(message))
  //funzione ausiliaria per chiudere la connessione più velocemente. usata solo nei test
  def close() = send(CloseFrame(StatusCode.NormalClose))
  def path() = uripath

 //aiuta ad implementare il ping pong tra server e browser
  private def ping() : Unit = pinger match {
    case None => // nothing to do
    case Some(timer) =>
      if (!timer.isCancelled) timer.cancel
      pinger = Some(context.system.scheduler.scheduleOnce(110.seconds, self, WebSocket.Ping))
  }
  private var uripath = "/" //uripath della richiesta, inizializzato con default, poi all'avvio del
                            // del websocket viene aggiornato
  private var pinger : Option[Cancellable] = None
  private var handler = self //destinatario dei messaggi inviati dall'attore WebSocketServer
}
