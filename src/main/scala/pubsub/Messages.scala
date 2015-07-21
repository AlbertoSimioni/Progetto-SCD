package pubsub

/**
 * Created by Alberto on 20/07/2015.
 */
object Messages {
  sealed trait PubSubMessages
  case class Moved(position: Int)
}
