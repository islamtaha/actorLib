
/*
 * @author eslam
 */

package lib

import java.util.EventObject

object MessageStatus extends Enumeration {
  type MessageStatus = Value
  val SENT, DELIVERED, COMPLETED, FAILED = Value
}

import MessageStatus._

class MessageEvent(source: Any, var message: Message, var status: MessageStatus) extends EventObject(source) {

  def getStatus(): MessageStatus = status 
    
}