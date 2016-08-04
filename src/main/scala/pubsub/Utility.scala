package pubsub

import map.Domain.position._
import map.Domain._
import map.Routes._

/**
 * @author Matteo Pozza
 * Metodi di utilità per l'interfaccia grafica
 */
object Utility {
  
  def getGuiDirection(direction : direction) : String = {
    direction.position match {
      case `up` | `down` =>
        if(direction.beginToEnd) {
          return "right"
        }
        else {
          return "left"
        }
      case `left` | `right` =>
        if(direction.beginToEnd) {
          return "up"
        }
        else {
          return "down"
        }
    }
  }
  
  def getGuiDirection(first : point, second : point) : String = {
    // PRECONDIZIONE: devono avere una coordinata uguale
    if(first.x == second.x) {
      if(first.y > second.y) {
        return "down"
      }
      else {
        return "up"
      }
    }
    else {
      if(first.y != second.y) {
        println("i punti problematici sono: first " + first.x + " " + first.y + ", second " + second.x + " " + second.y)
      }
      assert(first.y == second.y)
      if(first.x > second.x) {
        return "left"
      }
      else {
        return "right"
      }
    }
  }
  
}