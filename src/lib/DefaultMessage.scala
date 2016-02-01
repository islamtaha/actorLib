

package lib

import java.util.Date
import java.util.regex.Pattern
import java.util.regex.Matcher
/*
 * @author eslam
 */

case class DefaultMessage() extends Message {
   
  protected var data: Any = null
  protected var subject: String = null 
  
  
  override def getData(): Any = data

  
  protected def setData(data: Any): Unit = {
    this.data = data;
  }

 
  override def getSubject() = subject


  protected var source: Actor = null

  override def getSource(): Actor = source

  protected def setSource(source: Actor): Unit = {
    this.source = source
  }

  
  
  protected def setSubject(subject: String): Unit = {
    this.subject = subject;
  }  
  

  def this(subject: String) =  {
    this()
    this.subject = subject
  }
  
  def this(subject: String, data: Any) = {
    this(subject)
    this.data = data
  }  
  
   protected var delayUntil: Long = -1;
   
   def getDelayUntil(): Long = delayUntil
   
   def setDelayUntil(delayUntil: Long): Unit =  {
    val now: Long = new Date().getTime()
    if (delayUntil <= now) {
      throw new IllegalArgumentException("value should be in the future: " + delayUntil + " vs. " + now)
    }
    this.delayUntil = delayUntil
  }

  def assignSender(sender: Actor): Message = {
    val res: DefaultMessage = new DefaultMessage(subject, data);
    res.source = sender;
    return res;
  }
  
  
  protected def bodyString(): String =
    "source=" + source + ", subject=" + subject + ", data=" + data + ", delay=" + delayUntil

  override def toString(): String = getClass().getSimpleName() + "[" + bodyString() + "]"

  
  def subjectMatches(s: String): Boolean = if(subject != null) subject.equals(s) else false

  def subjectMatches(p: Pattern): Boolean = {
    var res: Boolean = false;
    if (p != null && subject != null) {
      val m: Matcher = p.matcher(subject);
      res = m.matches();
    }
    return res;
  }

  protected var listeners: List[PartialFunction[MessageEvent, Unit]] = Nil

  def addMessageListener(l: PartialFunction[MessageEvent, Unit]): Unit  = {
    if (!listeners.contains(l)) {
      listeners = l :: listeners
    }
  }
  
  def removeMessageListener(l: PartialFunction[MessageEvent, Unit]): Unit ={
    listeners = listeners.filter(_ != l)
  }
  
  def fireMessageListeners(e: MessageEvent): Unit =  {
    for(l <- listeners) {
      l(e)
    }
  }
  
}