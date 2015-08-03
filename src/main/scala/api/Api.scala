package api

/**
 * Created by Alberto on 09/06/2015.
 */

import _root_.akka.actor.ActorSystem
import _root_.akka.actor.Props
import _root_.akka.event.Logging._
import _root_.spray.http.HttpRequest
import _root_.spray.http.StatusCodes
import _root_.spray.routing.Directives
import _root_.spray.routing.RouteConcatenation
import _root_.spray.routing.directives.LogEntry

import akka.actor.{ ActorSystem, Props }
import akka.event.Logging.InfoLevel
import services.ConnectionService
import websocket.WebSocketWorker
import scala.reflect.ClassTag
import spray.http.{ HttpRequest, StatusCodes }
import spray.routing.{ Directives, RouteConcatenation }
import spray.routing.directives.LogEntry

trait AbstractSystem {
  implicit def system : ActorSystem
}

//In questa classe mettiamo i service con le rispettive routes
trait ReactiveApi extends RouteConcatenation with StaticRoute with AbstractSystem {
  this : MainActors =>
  private def showReq(req : HttpRequest) = LogEntry(req.uri, InfoLevel)

  //BasicRouteActor è il parametro di tipo, mentre le routes sono il parametro del costruttore
  //che è la Route in input al RootService. In questo caso dentro a RootService viene creato
  //un WebSocketServer
  val rootService = system.actorOf(Props(new RootService[BasicRouteActor](routes)), "routes")
  lazy val routes = logRequest(showReq _) {
    //new FindService(find).route ~  //find è l'unico FindActor del sistema
     // new HideService(hide).route ~  //hide è l'unico HideActor del sistema
      staticRoute
  }
  //WebSocketServer è il parametro di tipo, mentre le wsroutes sono il parametro del costruttore
  //che è la Route in input al RootService. In questo caso dentro a RootService viene creato
  //un WebSocketServer
  val wsService = system.actorOf(Props(new RootService[WebSocketWorker](wsroutes)), "wss")
  lazy val wsroutes = logRequest(showReq _) {
    new ConnectionService(connectionsActor).wsroute ~  //sono diverse dal rootservice di prima
     // new HideService(hide).wsroute ~
      complete(StatusCodes.NotFound)
  }

}



  trait StaticRoute extends Directives {
    this : AbstractSystem =>

    lazy val staticRoute =
      path("favicon.ico") {
        getFromResource("favicon.ico")
      } ~
        pathPrefix("images") {
          getFromResourceDirectory("images/")
        } ~
        pathPrefix("js") {
          getFromResourceDirectory("js/")
        } ~
        pathPrefix("css") {
          getFromResourceDirectory("css/")
        } ~
        pathPrefix("fonts") {
          getFromResourceDirectory("fonts/")
        } ~
        pathEndOrSingleSlash {
          getFromResource("index.html")
        } ~ complete(StatusCodes.NotFound)
  }


