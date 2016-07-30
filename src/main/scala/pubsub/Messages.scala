package pubsub

/**
 * Created by Alberto on 20/07/2015.
 */
//Messaggi spediti tra publisher e subscribers
object Messages {
  sealed trait PubSubMessages
  case class carPosition(id : String ,lat: Int,long: Int, direction: String) extends PubSubMessages
  case class busPosition(id : String ,lat: Int,long: Int, direction: String) extends PubSubMessages
  case class tramPosition(id : String ,lat: Int,long: Int, direction: String) extends PubSubMessages
  case class pedestrianPosition(id : String ,lat: Int,long: Int, direction: String) extends PubSubMessages
  case class hidePedestrian(id: String, zoneID: String) extends PubSubMessages
  case class hideCar(id: String,zoneID: String) extends PubSubMessages
  case class hideBus(id: String) extends PubSubMessages
  case class hideTram(id: String) extends PubSubMessages
  case class semaphoreState(id : String ,upGreen: Boolean,rightGreen: Boolean, downGreen: Boolean,leftGreen: Boolean) extends PubSubMessages
  // messaggio che deve essere inviato dopo che una macchina o un pedone si sono svegliati dalla sosta in una zone
  case class entityAwaked(entityID: String, zoneID: String) extends PubSubMessages
}


