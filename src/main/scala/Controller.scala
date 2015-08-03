/*
 * Incarna il generatore di entità
 * Le entità stradali si creano automaticamente quando contattate dalle entità mobili
 * Dunque Controller crea le entità mobili
 */

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.Props
import pubsub.Messages.CurrentTime
import pubsub.PublisherInstance
import scala.util.Random.shuffle
import scala.concurrent.duration._

case object StartInjection
case class CreateVehicle(ID : String)

class Controller extends Actor with ActorLogging {
  import context.dispatcher
  val vehicles = List("Alfa Romeo", "Mercedes", "Fiat", "Peugeot", "Opel", "Ford", "Subaru", "Nissan", "Kia", "Dacia")

  //Campi per la gestione del tempo. Invio periodico (ogni secondo) di un messaggio a se stesso
  val tick = context.system.scheduler.schedule(0 millis, 1000 millis, self, "tick")
  var dayElapsed = 0
  var minutesElapsed = 0
  val maxMinutes = 1440

  override def postStop() = tick.cancel()

  def receive : Receive = {
    //messaggio periodico che produce l'invio del tempo corrente a tutti i nodi worker tramite pub sub
    case "tick" =>
      val publisher = PublisherInstance.getPublisherTimeEvents(context.system)
      publisher ! CurrentTime(dayElapsed,minutesElapsed)
      if(minutesElapsed == maxMinutes){
        dayElapsed += 1
        minutesElapsed = 0
      }
      else minutesElapsed += 1
      //log.info(minutesElapsed.toString)

    case StartInjection =>
      vehicles foreach { vehicle =>
        self ! CreateVehicle(vehicle)
      }
    case CreateVehicle(id) =>
      // modo per creare attori con Props passando argomenti
      val vehicle = context.system.actorOf(Props(classOf[Vehicle], id, generateIter()))
      // istruisci ad andare verso la prima tappa
      vehicle ! Test
    case _ =>
  }
  
  def generateIter() : List[String] = {
    // aggiungi primo elemento
    var iter = List[String](shuffle(JSONReaderOld.getAllEdges()).head)
    var i = 1
    for(i <- 1 to 9) {
      // bisogna alternare edges e nodes
      if(i%2 == 1) {
        // siamo su un arco, bisogna raggiungere il nodo
        iter = iter :+ JSONReaderOld.getEdgeEndNode(iter.last)
      }
      else {
        // siamo su un nodo, bisogna scegliere un arco
        iter = iter :+ shuffle(JSONReaderOld.getNodeOutcomingEdges(iter.last)).head
      }
    }
    return iter
  }
  
}