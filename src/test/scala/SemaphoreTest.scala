import org.scalatest._
import map.Domain._
import map.Domain.position._

/**
 * @author studio
 */
class SemaphoreTest extends UnitSpec {
  
  val semaphoreId = "C000010800003360"
  
  val upLane = "L000010800003601"
  val rightLane = "L000012000003482"
  val downLane = "L000010800003243"
  val leftLane = "L000007200003482"
  val leftTramLane = "L000007200003481"
  
  semaphoreId should "have a proper configuration" in {
    val crossroadConfiguration = getCrossroadConfiguration(semaphoreId)
    for(entry <- crossroadConfiguration) {
      println(entry._1 + ":")
      println("Tram: " + entry._2._1)
      println("Position: " + printPosition(entry._2._2))
      for(precedence <- entry._2._3) {
        println("Precedence: " + precedence)
      }
    }
    crossroadConfiguration.size should be (5)
  } 
  
}