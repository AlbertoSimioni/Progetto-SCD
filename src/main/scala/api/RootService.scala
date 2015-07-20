package api

/**
 * Created by Alberto on 09/06/2015.
 */

import _root_.akka.actor.ActorLogging
import _root_.akka.actor.ActorRef
import _root_.akka.actor.Props
import _root_.spray.can.Http
import _root_.spray.routing.HttpServiceActor
import _root_.spray.routing._
import akka.actor.{ Actor, ActorLogging, ActorRef, Props }
import scala.reflect.ClassTag
import spray.can.Http
import spray.routing.{ HttpServiceActor, Route }

//tag è un valore che contine il tipo del parametro di tipo RA. cioè o BasicRouteActor o WebSocketServer
class RootService[RA <: RouteActor](val route : Route)(implicit tag : ClassTag[RA]) extends HttpServiceActor with ActorLogging {
  override def receive = {
    case connected : Http.Connected =>
      // implement the "per-request actor" pattern
      //questa classe risponde solo al primo messaggio di tipo http connected poi crea un attore apposito
      // per gestire tutte le successive chiamate
      sender ! Http.Register(context.actorOf(Props(tag.runtimeClass, sender, route)))
    case whatever => log.debug("RootService got some {}", whatever)
  }
}

//usato sopra per specificare le esigenze di parametro di tipo di RootService
trait RouteActor extends HttpServiceActor {
  def connection : ActorRef
  def route : Route
}

//estende routeActor
private[api] class BasicRouteActor(val connection : ActorRef, val route : Route) extends RouteActor {
  override def receive = runRoute(route)
}
