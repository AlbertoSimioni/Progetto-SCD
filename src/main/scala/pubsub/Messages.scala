package pubsub

/**
 * Created by Alberto on 20/07/2015.
 */
//Messaggi spediti tra publisher e subscribers
object Messages {
  sealed trait PubSubMessages
  case class CurrentTime(dayElapsed: Int, minutesElapsed: Int) extends PubSubMessages
  case class Moved(id : String ,position: Int) extends PubSubMessages
 // case class NewCar(id : String ,position: Int) extends PubSubMessages
}


