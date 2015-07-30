package messagesFormatter

import pubsub.Messages._

/**
 * Created by Alberto on 29/07/2015.
 */
object BrowserMessagesFormatter {

  def CarMovedMessageFormat(m : Moved) : String =  {
    s"""{"type": "CarMoved", "info":{"id":"${m.id}","position":"${m.position}"}}"""
  }

  def NewCarMessageFormat(m : NewCar) : String =  {
    s"""{"type": "NewCar", "info":{"id":"${m.id}","position":"${m.position}"}}"""
  }
}
