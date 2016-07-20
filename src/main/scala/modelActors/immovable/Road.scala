package modelActors.immovable

import akka.actor.ActorRef

import modelActors.CommonMessages._

/*
 * La strada viene utilizzata solamente dai pedoni in quanto le macchine, gli autobus e i tram
 * si interfacciano direttamente con le corsie.
 * L'unico dato da salvare è il numero di pedoni: totale ed attuale.
 * Quello totale è salvato con persistenza, quello attuale no.
 */
object Road {
  
  // commands
  
  // messaggio dal pedone alla strada per l'ingresso
  case class pedestrian_in(actorId : String, deliveryId : Long, ipAddress : String, UrbanElementId : String) extends Command
  // messaggio dal pedone alla strada per l'uscita
  case class pedestrian_out(actorId : String, deliveryId : Long, ipAddress : String, UrbanElementId : String) extends Command
  
  // events
  
  // evento di ingresso di un pedone
  case class pedestrian_entered(actorRef : ActorRef, actorId : String, deliveryId : Long, ipAddress : String) extends Event
  // evento di uscita di un pedone
  case class pedestrian_exited(actorRef : ActorRef, actorId : String, deliveryId : Long, ipAddress : String) extends Event
  
}