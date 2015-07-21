package coreActors

import akka.actor.{ActorLogging, Actor}
import websocket.WebSocket
import scala.concurrent.duration._
/**
 * Created by Alberto on 10/06/2015.
 */
object CarActor{
  case class startAnimation(ws: WebSocket)
}

class CarActor extends Actor with ActorLogging {
  import context.dispatcher
  override def receive = {
    case WebSocket.Open(ws) =>
      context.system.scheduler.schedule(5 seconds,5 seconds, self, CarActor.startAnimation(ws))

    case CarActor.startAnimation(ws) =>
      ws.send("lol");
  }

}
