package pubsub

/**
 * Created by Alberto on 20/07/2015.
 */
//Messaggi spediti tra publisher e subscribers
object Messages {
  //latitudine la posizione più a sinistra del mezzo
  //longitudine la posizione più in alto del mezzo
  sealed trait PubSubMessages
  case class CurrentTime(dayElapsed: Int, minutesElapsed: Int) extends PubSubMessages
  case class carPosition(id : String ,lat: Int,long: Int, direction: String) extends PubSubMessages
  case class busPosition(id : String ,lat: Int,long: Int, direction: String) extends PubSubMessages
  case class tramPosition(id : String ,lat: Int,long: Int, direction: String) extends PubSubMessages
  case class pedestrianPosition(id : String ,lat: Int,long: Int, direction: String) extends PubSubMessages
  case class hidePedestrian(id: String) extends PubSubMessages
  case class hideCar(id: String) extends PubSubMessages
  case class hideBus(id: String) extends PubSubMessages
  case class hideTram(id: String) extends PubSubMessages
  case class semaphoreState(id : String ,upGreen: Boolean,rightGreen: Boolean, downGreen: Boolean,leftGreen: Boolean) extends PubSubMessages
 // case class NewCar(id : String ,position: Int) extends PubSubMessages
}


