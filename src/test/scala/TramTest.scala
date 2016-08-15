import time.TimeMessages._
import org.scalatest._
import map.JSONReader
import map.JSONReader._
import map.Domain._
import map.Routes._

/**
 * @author studio
 */
class TramTest extends UnitSpec {
  
  /*"This generated path" should "include tram transportation" in {
    val places = ("Z000012000000840", "Z000002400001440", "Z000004800001080")
    val times = createTimes()
    val pedestrianRoute = createPedestrianRoute(places, times)
    val path = pedestrianRoute._1
    if(pedestrianRoute._2) {
      var tramUsed = false
      for(i <- 0 until path.houseToWorkRoute.length-1) {
        path.houseToWorkRoute(i) match {
          case tram_stop_step(_, _, false) =>
            path.houseToWorkRoute(i+1) match {
              case tram_stop_step(_, _, false) =>
                tramUsed = true
              case _ =>
                
            }
          case _ =>
            
        }
      }
      if(tramUsed) {
        println("Tram usato nella tratta da casa a lavoro")
      }
      else {
        for(i <- 0 until path.workToFunRoute.length-1) {
          path.workToFunRoute(i) match {
            case tram_stop_step(_, _, false) =>
              path.workToFunRoute(i+1) match {
                case tram_stop_step(_, _, false) =>
                  tramUsed = true
                case _ =>
                  
              }
            case _ =>
              
          }
        }
        if(tramUsed) {
          println("Tram usato nella tratta da lavoro a svago")
        }
        else {
          for(i <- 0 until path.funToHomeRoute.length-1) {
            path.funToHomeRoute(i) match {
              case tram_stop_step(_, _, false) =>
                path.funToHomeRoute(i+1) match {
                  case tram_stop_step(_, _, false) =>
                    tramUsed = true
                  case _ =>
                    
                }
              case _ =>
                
            }
          }
          if(tramUsed) {
            println("Tram usato nella tratta da svago a casa")
          }
          else {
            println("TramTest: usage of bus in place of tram")
          }
        }
      }
    }
    else {
      println("TramTest: no usage of public transportation")
    }
  }*/
  
  "The tram path generator" should "work properly" in {
    println("TRAM PATH GENERATOR:")
    val pedestrianRoute = createPedestrianRouteWithTram()
    RoutePrinter.printRoute(pedestrianRoute._1)
    pedestrianRoute._2 should be (true)
  }
  
  "The tram path generator" should "produce valid paths" in {
    var noTramUsage = false
    for(i <- 1 to 500) {
      val pedestrianRoute = createPedestrianRouteWithTram()
      val path = pedestrianRoute._1
      var tramUsed = false
      for(i <- 0 until path.houseToWorkRoute.length-1) {
        path.houseToWorkRoute(i) match {
          case tram_stop_step(_, _, false) =>
            path.houseToWorkRoute(i+1) match {
              case tram_stop_step(_, _, false) =>
                tramUsed = true
              case _ =>
                
            }
          case _ =>
            
        }
      }
      if(tramUsed) {
        println("Tram usato nella tratta da casa a lavoro")
      }
      else {
        for(i <- 0 until path.workToFunRoute.length-1) {
          path.workToFunRoute(i) match {
            case tram_stop_step(_, _, false) =>
              path.workToFunRoute(i+1) match {
                case tram_stop_step(_, _, false) =>
                  tramUsed = true
                case _ =>
                  
              }
            case _ =>
              
          }
        }
        if(tramUsed) {
          println("Tram usato nella tratta da lavoro a svago")
        }
        else {
          for(i <- 0 until path.funToHomeRoute.length-1) {
            path.funToHomeRoute(i) match {
              case tram_stop_step(_, _, false) =>
                path.funToHomeRoute(i+1) match {
                  case tram_stop_step(_, _, false) =>
                    tramUsed = true
                  case _ =>
                    
                }
              case _ =>
                
            }
          }
          if(tramUsed) {
            println("Tram usato nella tratta da svago a casa")
          }
          else {
            noTramUsage = true
          }
        }
      }
    }
    noTramUsage should be (false)
  }
  
}