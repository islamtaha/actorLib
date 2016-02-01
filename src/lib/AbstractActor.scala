package lib

import MessageStatus._
import scala.util.control.Breaks._
import java.util.regex.Pattern
import java.util.Date
import lib._
/*
 * @author eslam
 */
 abstract class AbstractActor extends Actor {
  val DEFAULT_MAX_MESSAGES = 100

  protected var messages: List[DefaultMessage] = Nil
  
  protected var manager: DefaultActorManager = null
  
  def getMessages(): Vector[DefaultMessage] = messages.toVector
  
  def getManager(): ActorManager = manager
  
  def setManager(manager: DefaultActorManager): Unit =  {
    if (this.manager != null && manager != null) {
      throw new IllegalStateException(
          "cannot change manager of attached actor");
    }
    this.manager = manager;
  }
  
  protected var name: String = null
  
  override def getName(): String = name

  override def setName(name: String): Unit =  {
    if (manager != null) {
      throw new IllegalStateException("cannot change name if manager set");
    }
    this.name = name;
  }
  
  protected var category = DEFAULT_CATEGORY;

    override def getCategory() : String = category  

  override def setCategory(category: String): Unit = {
    this.category = category;
  }

  protected var is_Active: Boolean = false

  def isActive(): Boolean = is_Active
  
  override def activate(): Unit = {
    is_Active = true
  }

  override def deactivate(): Unit = {
    is_Active = false;
  }
  
  protected var hasThread: Boolean = false

  def getHasThread(): Boolean = hasThread

  def setHasThread(hasThread: Boolean): Unit = {
    this.hasThread = hasThread;
  }
  
  
  var shutdown: Boolean= false

  override def isShutdown() = shutdown

  override def shutDown(): Unit = {
    shutdown = true
  }

  protected var suspended: Boolean = false

  override def setSuspended(f: Boolean): Unit = {
    suspended = f
  }

  override def isSuspended() = suspended

  override def peekNext(): Message = peekNext(null)

  override def peekNext(subject: String): Message = peekNext(subject, false)

  override def peekNext(subject: String, isRegExpr: Boolean): Message = {
    var res: Message = null
    if (isActive) {
      val p: Pattern = 
        if(subject != null)  
          (if(isRegExpr) 
            Pattern.compile(subject)
              else null)
                else null
      val now: Long = new Date().getTime();
      messages.synchronized {
           breakable{ for(m <- messages) { 
            if (m.getDelayUntil() <= now) {
              val matcher: Boolean = (subject == null) || ( if(isRegExpr) m.subjectMatches(p) else m.subjectMatches(subject))
              if (matcher) {
                res = m
                break
              }
            }
          }
         }
      }
    }
     res
  }

  override def remove(message: Message): Unit = {
    messages.synchronized {
      messages = messages.filter( x => x != message )
    }
  }

  
  protected def getMatch(subject: String, isRegExpr: Boolean): DefaultMessage = {
     var res: DefaultMessage = null
    messages.synchronized {
      res = (peekNext(subject, isRegExpr)).asInstanceOf[DefaultMessage]
    }
      res
  }
  
  protected def testMessage(): Message = getMatch(null, false)
  
  protected def loopBody(m: Message): Unit

  
  
  override def receive(): Boolean = {
    val m: Message = testMessage();
    val res: Boolean = m != null;
    if (res) {
      remove(m)
       val dm: DefaultMessage = m.asInstanceOf[DefaultMessage]
      try {
        dm.fireMessageListeners(new MessageEvent(this, dm, MessageStatus.DELIVERED))
        loopBody(m)
        dm.fireMessageListeners(new MessageEvent(this, dm, MessageStatus.COMPLETED))
      } catch {case e: Exception => 
        dm.fireMessageListeners(new MessageEvent(this, dm, MessageStatus.FAILED))
        println("loop exception" + e)
      }
    }
    manager.awaitMessage(this)
    res
  }

  override def getMessageCount(): Int = {
    messages.synchronized {
       messages.length
    }
  }
  
  override def getMaxMessageCount(): Int = DEFAULT_MAX_MESSAGES

  
  def addMessage(message: DefaultMessage): Unit = {
    if (message != null) {
      messages.synchronized {
        if (messages.length < getMaxMessageCount()) {
           messages = message :: messages
        } else {
          throw new IllegalStateException("too many messages, cannot add")
        }
      }
    } 
  }

  
  protected def runBody(): Unit = {
    val m: DefaultMessage = new DefaultMessage("init");
    getManager().send(m, null, this);
  }

  override def run(): Unit = {
    runBody()
    (getManager().asInstanceOf[DefaultActorManager]).awaitMessage(this)
  }


  protected def bodyString(): String = "name=" + name + ", category=" + category + ", messages=" + messages.length

  
  override def toString(): String = getClass().getSimpleName() + "[" + bodyString() + "]"

  override def willReceive(subj: String): Boolean = !isEmpty(subj)
 

  def isEmpty(s: CharSequence): Boolean = (s == null || s.length() == 0)
  
  
}