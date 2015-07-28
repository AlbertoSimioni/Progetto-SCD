import akka.actor.Actor
import akka.actor.ActorLogging
import akka.contrib.pattern.ClusterSharding
import scala.util.Random.shuffle
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global
import pubsub.{Publisher,PublisherInstance}
import pubsub.Messages.Moved
// Test per testare se il destinatario è remoto o meno
case object Test
case object TravelIn
case object TravelOut

class Vehicle(id : String, iter : List[String]) extends Actor with ActorLogging {
  
  // recupera l'ActorRef della ShardRegion di appartenenza
  val region = ClusterSharding(context.system).shardRegion(UrbanElement.typeOfEntries)
  var index = 0
  var currentPlace : String = ""
  
  def receive : Receive = {
    case Test =>
      currentPlace = iter(index)
      index = (index + 1)%10
      val publisher = PublisherInstance.getPublisherModelEvents(context.system)
      publisher ! Moved(index)
      val senderIp = context.system.settings.config.getString("akka.remote.netty.tcp.hostname")
      region ! UrbanElement.isRemoteQuest(currentPlace, self, senderIp)
    case UrbanElement.isRemoteAnswer(flag) =>
      if(flag == true)
        log.info("Remote destination")
      else
        log.info("Local destination")
      self ! TravelIn
    case TravelIn =>
      region ! UrbanElement.VehicleIn(currentPlace, id)
      context.system.scheduler.scheduleOnce(FiniteDuration(3000, "milliseconds"), self, TravelOut)
      ()
    case TravelOut =>
      region ! UrbanElement.VehicleOut(currentPlace, id)
      context.system.scheduler.scheduleOnce(FiniteDuration(3000, "milliseconds"), self, Test)
      ()
    case _ =>
  }
  
}