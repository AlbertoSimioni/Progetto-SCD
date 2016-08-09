import time.TimeMessages._
import org.scalatest._
import map.JSONReader
import map.JSONReader._
import map.Domain._
import map.Routes._

/**
 * @author studio
 */
class RouteTest2 extends UnitSpec {
  
  "A path with bus usage" should "print out" in {
    val pedestrianBusPlaces = ("Z000008400004680", "Z000048000000480", "Z000038400004320")
    val immediateTime = (TimeValue(16, 30), TimeValue(0, 30), TimeValue(8, 30))
    val pedestrianRoute = createPedestrianRoute(pedestrianBusPlaces, immediateTime)
    RoutePrinter.printRoute(pedestrianRoute._1)
  }
  
}