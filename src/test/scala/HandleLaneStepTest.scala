import time.TimeMessages._
import org.scalatest._
import map.JSONReader
import map.BreadthFirst._
import map.PointsSequence._
import map.Domain.position._
import map.Routes
import map.Routes.direction
import map.Domain._
import map.Routes._

/**
 * @author studio
 */
class HandleLaneStepTest extends UnitSpec {
  
  "The lane step handler" should "handle properly all the directions" in {
    val immediateTime = (TimeValue(16, 30), TimeValue(0, 30), TimeValue(8, 30))
    val carPlaces = ("Z000040800001680", "Z000014400004800", "Z000012000001440")
    val carRoute = Routes.createCarRoute(carPlaces, immediateTime)
    
    val lane = JSONReader.getLane(current_map, "L000032400000721").get
    val laneDirection = direction(`left`, false)
    val laneStep = lane_step(lane, laneDirection)
    
    val crossroad = JSONReader.getCrossroad(current_map, "C000032400000960").get
    val crossroadDirection = direction(`left`, false)
    val crossroadArrivingDirection = direction(`up`, false)
    val crossroadStep = crossroad_step(crossroad, crossroadDirection)
    
    val nextCrossroad = JSONReader.getCrossroad(current_map, "C000032400000480").get
    val nextCrossroadDirection = direction(`up`, false)
    val nextCrossroadStep = crossroad_step(nextCrossroad, nextCrossroadDirection)
    
    // trova lo stesso step nel percorso generato
    //val index = carRoute.workToFunRoute.indexOf(laneStep)
    /*
    for(step <- carRoute.workToFunRoute) {
      println(printStep(step))
    }
    */
    
    val directions = getCrossroadAvailableDirections(crossroad, crossroadArrivingDirection)
    for(direction <- directions) {
      println(RoutePrinter.printDirection(direction))
    }
    
    
    /*
    carRoute.workToFunRoute(index) match {
      case targetStep @ lane_step(_, _) =>
        val result = handleLaneStep(targetStep, car_length, carRoute.workToFunRoute(index-1), carRoute.workToFunRoute(index+1))
        println("ECCOCI QUA:")
        println(result.size)
        println(result(0).size)
    }
    */
  }
  
}