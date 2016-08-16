package controllerActors

import akka.actor.Actor
import akka.actor.Identify
import akka.actor.ActorIdentity
import akka.persistence.journal.leveldb.SharedLeveldbJournal
import akka.persistence.journal.leveldb.SharedLeveldbStore
import akka.actor.PoisonPill

/**
 * @author Matteo Pozza
 * Attore semplicissimo che recupera l'id del database locale
 */
class LocalDBActor extends Actor {
  
  context.actorSelection("akka.tcp://UrbanSimulator@127.0.0.1:2551/user/store") ! Identify(999)
  
  override def receive : Receive = {
    case ActorIdentity(999, Some(store)) =>
        SharedLeveldbJournal.setStore(store, context.system)
        self ! PoisonPill
  }
  
}