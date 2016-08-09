package controllerActors

import com.typesafe.config.ConfigFactory

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.PoisonPill

import com.mongodb.casbah.Imports._

import Messages._
import common.CommonMessages._
import common.ToNonPersistentMessages
import common.ToPersistentMessages

/**
 * @author Matteo Pozza
 * Si preoccupa di cancellare il DB prima dell'esecuzione vera e propria dell'applicazione.
 */
class DBEraser extends Actor {
  
  override def receive : Receive = {
    case ToNonPersistent(destinationRef, toNonPersistentMessage) =>
      toNonPersistentMessage match {
        case ToNonPersistentMessages.FromImmovable(senderId, command) =>
          //
        case ToNonPersistentMessages.FromMovable(senderRef, command) =>
          //
        case ToNonPersistentMessages.FromNonPersistent(senderRef, command) =>
          command match {
            case EraseDB =>
              val server = new ServerAddress("ds037611.mlab.com", 37611)
              // val server = new ServerAddress("ds029565.mlab.com", 29565)
              val credentials = MongoCredential.createScramSha1Credential("mpozza", "scd", Array('0','1','1','0','9','2'))
              val mongoClient = MongoClient(server, List(credentials))
              val db = mongoClient.getDB("scd")
              // carica i nomi delle collection
              val configuration = ConfigFactory.load
              val journal_collection = configuration.getString("akka.contrib.persistence.mongodb.mongo.journal-collection")
              val snapshot_collection = configuration.getString("akka.contrib.persistence.mongodb.mongo.snaps-collection")
              for(name <- db.collectionNames()) {
                if(name == journal_collection || name == snapshot_collection) {
                  val collection = db(name)
                  collection.drop
                }
              }
              // al termine del lavoro, manda l'ack, poi non servi più
              sendToNonPersistent(self, senderRef, EraseDBAck)
              self ! PoisonPill
          }
      }
  }
  
  def sendToNonPersistent(senderRef : ActorRef, destinationRef : ActorRef, command : Command) : Unit = {
    destinationRef ! ToNonPersistent(destinationRef, ToNonPersistentMessages.FromNonPersistent(senderRef, command))
  }
  
}