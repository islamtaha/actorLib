

package lib/**
 * @author eslam
 */
trait Actor extends Runnable{
  
  var DEFAULT_CATEGORY: String = "default"
  
  def getName(): String
  
  def setName(name: String): Unit
  
  def getCategory(): String
  
  def setCategory(category: String): Unit
  
  def receive(): Boolean
  
  def willReceive(subject: String): Boolean
  
  def peekNext(): Message
  
  def peekNext(subject: String): Message
  
  def peekNext(subject: String, isRegExpr: Boolean): Message
  
  def remove(message: Message): Unit
  
  def activate(): Unit
  
  def deactivate(): Unit
  
  def setSuspended(f: Boolean): Unit
  
  def isSuspended(): Boolean
  
  def shutDown(): Unit
  
  def isShutdown(): Boolean
  
  def getMessageCount(): Int
  
  def getMaxMessageCount(): Int

}