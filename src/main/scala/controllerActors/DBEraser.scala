package controllerActors

import java.util.concurrent.TimeUnit
import java.io.File

import com.typesafe.config.ConfigFactory

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.PoisonPill
import akka.persistence.journal.leveldb.SharedLeveldbJournal
import akka.persistence.journal.leveldb.SharedLeveldbStore
import akka.util.Timeout
import akka.actor.Props
import akka.actor.Identify
import akka.actor.ActorIdentity

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
              val configuration = ConfigFactory.load
              val database = configuration.getString("domain.database")
              if(database == "mongo") {
                val server = new ServerAddress("ds037611.mlab.com", 37611)
                // val server = new ServerAddress("ds029565.mlab.com", 29565)
                // val server = new ServerAddress("54.164.61.188", 27017)
                val credentials = MongoCredential.createScramSha1Credential("mpozza", "scd", Array('0','1','1','0','9','2'))
                // val credentials = MongoCredential.createCredential("myUserAdmin", "scd", Array('a','b','c','1','2','3'))
                val mongoClient = MongoClient(server, List(credentials))
                val db = mongoClient.getDB("scd")
                // carica i nomi delle collection
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
              else if(database == "local") {
                // elimina tutto ciò che c'era prima
                // possiamo fare affidamento sul path dell'application.conf
                val applicationFile = new File(getClass.getResource("/application.conf").getPath)
                val journalDirectoryPath = applicationFile.getParentFile.getParentFile.getParentFile.getPath + "/shared-journal"
                val snapshotDirectoryPath = applicationFile.getParentFile.getParentFile.getParentFile.getPath + "/snapshots"
                val journalDirectory = new File(journalDirectoryPath)
                if(journalDirectory.exists()) {
                  deleteFile(journalDirectory)
                }
                val snapshotDirectory = new File(snapshotDirectoryPath)
                if(snapshotDirectory.exists()) {
                  deleteFile(snapshotDirectory)
                }
                // crea tutto il necessario per il nuovo database
                val databaseActor = context.system.actorOf(Props[SharedLeveldbStore], "store")
                SharedLeveldbJournal.setStore(databaseActor, context.system)
                sendToNonPersistent(self, senderRef, EraseDBAck)
                // non puoi ammazzarti perchè tuo figlio è l'attore del DB
              }
          }
      }
      
  }
  
  def deleteFile(file : File) : Unit = {
    if(file.isDirectory()) {
      for(innerFile <- file.listFiles()) {
        deleteFile(innerFile)
      }
    }
    val flag = file.delete()
    assert(flag == true)
  }
  
  def sendToNonPersistent(senderRef : ActorRef, destinationRef : ActorRef, command : Command) : Unit = {
    destinationRef ! ToNonPersistent(destinationRef, ToNonPersistentMessages.FromNonPersistent(senderRef, command))
  }
  
}