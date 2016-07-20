package api

import akka.actor.Props
import coreActors.{ActiveConnections}

trait MainActors {
  this : AbstractSystem =>

  //lazy val carActor = system.actorOf(Props[CarActor], "find")
  lazy val connectionsActor = system.actorOf(Props[ActiveConnections], "activeConnections")
  //lazy val hide = system.actorOf(Props[HideActor], "hide")
}
