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
  
  // Data una lista di punti in input, restituisce la direzione della lista
  // Se la lista è minore di 1, la direzione è indifferente
  // Se la lista è maggiore di 1, viene restituita la direzione dei primi due punti che hanno una coordinata uguale
  def getGuiDirection(sequence : List[point]) : String = {
    if(sequence.length < 2) {
      return "up"
    }
    else {
      // trova i primi due punti con una coordinata uguale
      var first : point = null
      var second : point = null
      var index = 0
      var flag = false
      while(flag == false && index != sequence.length-1) {
        first = sequence(index)
        second = sequence(index + 1)
        if((first.x == second.x) || (first.y == second.y)) {
          flag = true
        }
        else {
          index = index + 1
        }
      }
      if(index == sequence.length-1) {
        // non vi sono due punti con almeno una coordinata uguale
        println("In un pezzo di percorso non vi sono almeno due punti con una coordinata uguale")
        return "up"
      }
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
  
}