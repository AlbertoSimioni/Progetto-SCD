package controllerActors

import common.CommonMessages._

/**
 * @author Matteo Pozza
 * Classe che racchiude i messaggi scambiati tra attori controller
 */
object Messages {
  
  // messaggi da e per il Controller
  case object StartSystem extends Command
  
  // messaggi da e per il DBEraser
  case object EraseDB extends Command
  case object EraseDBAck extends Command
  
  // messaggi da e per l'injector
  case object StartInjection extends Command
  case class CreatePedestrian(id : String) extends Command
  case class CreateCar(id : String) extends Command
  case class CreateBus(id : String, route : Int) extends Command
  case class CreateTram(id : String, route : Int) extends Command
  
}