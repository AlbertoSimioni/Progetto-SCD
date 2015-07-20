package services

/**
 * Created by Alberto on 10/06/2015.
 */

import websocket.WebSocket
import akka.actor.{ ActorRef, ActorSystem }
import spray.http.StatusCodes
import spray.routing.Directives


class CarService(carActor : ActorRef)(implicit system : ActorSystem) extends Directives {
  //invocata solo alla connessione di un nuovo broswer WebSocket
  lazy val wsroute =
      path("ws") {
        implicit ctx =>
          ctx.responder ! WebSocket.Register(ctx.request, carActor, true)
    }
}
