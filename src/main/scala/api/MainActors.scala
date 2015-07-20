package api

import akka.actor.Props
import coreActors.CarActor

trait MainActors {
  this : AbstractSystem =>

  lazy val carActor = system.actorOf(Props[CarActor], "find")
  //lazy val hide = system.actorOf(Props[HideActor], "hide")
}
