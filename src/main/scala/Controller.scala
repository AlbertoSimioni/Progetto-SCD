/*
 * Incarna il generatore di entità
 * Le entità stradali si creano automaticamente quando contattate dalle entità mobili
 * Dunque Controller crea le entità mobili
 */

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.Props
import scala.util.Random.shuffle

case object StartInjection
case class CreateVehicle(ID : String)

class Controller extends Actor with ActorLogging {
  
  val vehicles = List("Alfa Romeo", "Mercedes", "Fiat", "Peugeot", "Opel", "Ford", "Subaru", "Nissan", "Kia", "Dacia")
  
  def receive : Receive = {
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
    var iter = List[String](shuffle(JSONReader.getAllEdges()).head)
    var i = 1
    for(i <- 1 to 9) {
      // bisogna alternare edges e nodes
      if(i%2 == 1) {
        // siamo su un arco, bisogna raggiungere il nodo
        iter = iter :+ JSONReader.getEdgeEndNode(iter.last)
      }
      else {
        // siamo su un nodo, bisogna scegliere un arco
        iter = iter :+ shuffle(JSONReader.getNodeOutcomingEdges(iter.last)).head
      }
    }
    return iter
  }
  
}