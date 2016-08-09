import org.scalatest._
import map.JSONReader
import map.JSONReader._
import map.Domain._
import map.Routes._

/**
 * @author studio
 */
class RouteTest extends UnitSpec {
  
  "A pedestrian path" should "include bus transport when possible" in {
    var busUsed = false
    var index = 0
    var flag = false
    var path : pedestrian_route = null
    while(busUsed == false) {
      flag = false
      path = null
      while(flag == false) {
        val tuple = createPedestrianRoute()
        flag = tuple._2
        path = tuple._1
      }
      for(i <- 0 until path.houseToWorkRoute.length-1) {
        path.houseToWorkRoute(i) match {
          case bus_stop_step(_, _, false) =>
            path.houseToWorkRoute(i+1) match {
              case bus_stop_step(_, _, false) =>
                busUsed = true
                index = i
              case _ =>
                
            }
          case _ =>
            
        }
      }
      if(busUsed) {
        println("Bus usato nella tratta da casa a lavoro: step " + index + " e " + (index+1))
      }
      else {
        for(i <- 0 until path.workToFunRoute.length-1) {
          path.workToFunRoute(i) match {
            case bus_stop_step(_, _, false) =>
              path.workToFunRoute(i+1) match {
                case bus_stop_step(_, _, false) =>
                  busUsed = true
                  index = i
                case _ =>
                  
              }
            case _ =>
              
          }
        }
        if(busUsed) {
          println("Bus usato nella tratta da lavoro a svago: step " + index + " e " + (index+1))
        }
        else {
          for(i <- 0 until path.funToHomeRoute.length-1) {
            path.funToHomeRoute(i) match {
              case bus_stop_step(_, _, false) =>
                path.funToHomeRoute(i+1) match {
                  case bus_stop_step(_, _, false) =>
                    busUsed = true
                    index = i
                  case _ =>
                    
                }
              case _ =>
                
            }
          }
          if(busUsed) {
            println("Bus usato nella tratta da svago a casa: step " + index + " e " + (index+1))
          }
        }
      }
    }
    var houseplace = ""
    path.houseToWorkRoute.head match {
      case zone_step(zone, _) =>
        houseplace = zone.id
    }
    var workplace = ""
    path.workToFunRoute.head match {
      case zone_step(zone, _) =>
        workplace = zone.id
    }
    var funplace = ""
    path.funToHomeRoute.head match {
      case zone_step(zone, _) =>
        funplace = zone.id
    }
    println("Houseplace: " + houseplace)
    println("Workplace: " + workplace)
    println("Funplace: " + funplace)
  }
  
  /*"A pedestrian path" should "include tram transport when possible" in {
    var tramUsed = false
    var flag = false
    var path : pedestrian_route = null
    while(tramUsed == false) {
      flag = false
      path = null
      while(flag == false) {
        val tuple = createPedestrianRoute()
        flag = tuple._2
        path = tuple._1
      }
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
        }
      }
    }
    var houseplace = ""
    path.houseToWorkRoute.head match {
      case zone_step(zone, _) =>
        houseplace = zone.id
    }
    var workplace = ""
    path.workToFunRoute.head match {
      case zone_step(zone, _) =>
        workplace = zone.id
    }
    var funplace = ""
    path.funToHomeRoute.head match {
      case zone_step(zone, _) =>
        funplace = zone.id
    }
    println("Houseplace: " + houseplace)
    println("Workplace: " + workplace)
    println("Funplace: " + funplace)
  }*/
  
}