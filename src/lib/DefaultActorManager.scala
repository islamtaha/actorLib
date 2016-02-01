package lib


import java.io.FileInputStream
import java.io.IOException
import java.util.Properties
import java.lang.reflect.Method
import scala.collection.mutable.LinkedHashMap
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import scala.util.Random
import java.util.Date
import scala.util.control.Breaks._
import java.util.Collection
import scala.collection.immutable.TreeSet




/*
 * @author eslam
 */
class DefaultActorManager extends ActorManager {
  
  val DEFAULT_ACTOR_THREAD_COUNT = 10
  
  protected var threadGroup: ThreadGroup = null

  var running: Boolean = false
  var terminated: Boolean = false

  
  def getThreadGroup(): ThreadGroup = threadGroup

  protected def createThread(i: Int): Unit = addThread("actor" + i)

  
  
  
  def addThread(name: String): Thread = {
    var t: Thread = null
    actors.synchronized {
      if (trunnables.contains(name)) {
        throw new IllegalStateException("already exists: " + name)
      }
      var r: ActorRunnable = new ActorRunnable()
      trunnables += (name -> r)
      t = new Thread(threadGroup, r, name);
      threads = t :: threads
    }
    t.setDaemon(true)
    t.setPriority(getThreadPriority())
    t
  }

 
  
  def removeThread(name: String): Unit = {
    actors.synchronized {
      if (!trunnables.contains(name)) {
        throw new IllegalStateException("not running: " + name);
      }
      trunnables = trunnables - name
      var i: Iterator[Thread] = threads.iterator
      breakable {
        while(i.hasNext) {
          val nxt: Thread = i.next()
          if(nxt.getName().equals(name)) {
            threads = threads diff List(threads.indexOf(nxt))
            nxt.interrupt()
            break
          }
        }
      }
    }
  }

  
  def  initialize(): Unit = {
    initialize(null)
  }

  private var initialized: Boolean = false
  
   
  def initialize(options: Map[String, Any]): Unit = {
    if (!initialized) {
      initialized = true
      val count: Int = getThreadCount(options)
      val tg: ThreadGroup = new ThreadGroup("ActorManager" + (groupCount+=1))
      threadGroup = tg
      for (i <- 0 until count) {
        createThread(i)
      }
      running = true
      for (t <- threads) {
        t.start()
      }
      var Counter: Thread = new Thread(new Runnable(){
        override def run() : Unit ={
          while (running) {
            try {
              trendValue = sendCount - dispatchCount
              lastSendCount = sendCount
              sendCount = 0
              updateLastDispatchCount()
              Thread.sleep(1000)
            } catch { case e: InterruptedException => 
              breakable{
                break
              }
            }
          }
          sendCount = 0
          lastSendCount = 0
          clearDispatchCount()
        }
      })
      Counter.setDaemon(true);
      lastDispatchTime = new Date().getTime()
      lastSendTime = new Date().getTime()
      Counter.start()
    }
  }

  
  def  getThreadPriority(): Int = Math.max(Thread.MIN_PRIORITY, Thread.currentThread().getPriority() - 1)

  def getThreadCount(options: Map[String, Any]): Int = {
    var count: Int = 0
    var xcount = if(options != null) options(ACTOR_THREAD_COUNT) else null
    if (xcount != null) {
      if (xcount.isInstanceOf[Int]) {
        count = xcount.asInstanceOf[Int]
      } else {
        count = Integer.parseInt(xcount.toString())
      }
    }
    if (count == 0) {
      count = DEFAULT_ACTOR_THREAD_COUNT
    }
    count
  }


  
  
  protected var instance: DefaultActorManager = null

  protected val rand: Random = new Random()

    
  def isEmpty(m: java.util.Map[_, _]): Boolean = {
    return m == null || m.size == 0;
  }
    
    def getDefaultInstance(): DefaultActorManager = {
      if(instance == null){
        instance = new DefaultActorManager()
        var options: Map[String, Any] = Map()
        val p: Properties = new Properties()
        /*try {
        p.load(new FileInputStream("ActorManager.properties"))
        } catch { case e: IOException =>
          try {
            p.load(new FileInputStream("/resource/ActorManager.properties"))
          } catch { case e1: IOException =>  
            println("DefaultActorManager: no configutration: " + e.getMessage)
          }
        }
        * 
        */
        if (!isEmpty(p)) {
        options = Map[String, Any]()
        for (key <- p.keySet().toArray()) {
          val skey: String = key.asInstanceOf[String]
          options += (skey -> p.getProperty(skey))
        }
      }
      instance.initialize(options);
    }
     instance
  }
    
  val ACTOR_THREAD_COUNT: String = "threadCount"

  protected var actors: Map[String, AbstractActor] = Map()
  
  protected var runnables: Map[String, AbstractActor] = Map()

  protected var waiters: Map[String, AbstractActor] = Map()
 
  protected var trunnables: Map[String, ActorRunnable] = Map[String, ActorRunnable]()

  
  
  def getRunnable(name: String): ActorRunnable = trunnables(name)

  
  def getActiveRunnableCount(): Int = {
    var res: Int = 0
    actors.synchronized {
      for (key <- trunnables.keySet) {
        if (trunnables(key).hasThread) {
          res += 1
        }
      }
    }
    res
  }
  
  
  

  
  
  override def detachActor(a: Actor): Unit = {
    var actor = a
    if ((actor.asInstanceOf[AbstractActor]).getManager() != this) {
      throw new IllegalStateException("actor not owned by this manager")
    }
    val name: String = actor.getName()
    actors.synchronized{
      if (actors.contains(name)) {
        (actor.asInstanceOf[AbstractActor]).setManager(null)
        actors = actors - name
        runnables = runnables - name
        waiters = waiters - name
      } else {
        actor = null
      }
    }
    if (actor != null) {
      actor.deactivate()
    }
  }
  
  
  def detachAllActors(): Unit = {
    var xkeys: Set[String] = Set[String]()
    xkeys = actors.keySet
    val i: Iterator[String] = xkeys.iterator
    while (i.hasNext) {
      detachActor(actors(i.next()))
    }
    actors.synchronized{
      actors = Map[String, AbstractActor]()
      runnables = Map[String, AbstractActor]()
      waiters = Map[String, AbstractActor]()
    }
  }

  
  def getActors(): List[AbstractActor] = {
    var res: List[AbstractActor] = Nil
    actors.synchronized {
      for (key <- actors.keySet) {
       res = actors(key) :: res
      }
    }
    res
  }

  
  def randomizeActors(): Unit = {
    actors.synchronized{
      var xactors: List[AbstractActor] = getActors()
      var zactors: List[AbstractActor] = xactors.map{x => null}
      for (a <- xactors) {
        zactors.updated(rand.nextInt(zactors.length + 1), a);
      }
      actors = Map[String, AbstractActor]()
      for (a <- zactors) {
        actors += (a.getName() -> a)
      }
    }
  }

  
  protected var sentMessages: Map[String, List[Message]] = Map[String, List[Message]]()

  protected var recordSentMessages: Boolean = true

  def getRecordSentMessages(): Boolean = recordSentMessages
  
  def setRecordSentMessages(recordSentMessages: Boolean): Unit = {
    this.recordSentMessages = recordSentMessages
  }

  
  def getAndClearSentMessages(actor: Actor): List[Message] = {
    var res: List[Message] = List()
    sentMessages.synchronized{
      var l: List[Message] = sentMessages(actor.getName())
      if (!l.isEmpty) {
        res = res ++ l
        l = List[Message]()
      }
    }
    return if(res != List()) res else null
  }

  
  protected var lastSendTime: Long = 0
  protected var lastDispatchTime: Long = 0
  
  def getLastSendTime(): Long = lastSendTime

  def getLastDispatchTime(): Long = lastDispatchTime
  
  protected var sendCount: Int = 0
  protected var lastSendCount: Int = 0
  protected var dispatchCount: Int = 0
  protected var lastDispatchCount: Int = 0

  def getSendPerSecondCount(): Int = lastSendCount

  def getDispatchPerSecondCount(): Int = {
    actors.synchronized {
      return lastDispatchCount
    }
  }

  protected def incDispatchCount(): Unit = {
    actors.synchronized {
      dispatchCount += 1
      lastDispatchTime = new Date().getTime()
    }
  }

  protected def clearDispatchCount(): Unit = {
    actors.synchronized{
      dispatchCount = 0
      lastDispatchCount = 0
    }
  }
  
  
  protected def updateLastDispatchCount(): Unit = {
    actors.synchronized{
      lastDispatchCount = dispatchCount
      dispatchCount = 0
    }
  }


  
 
  override def send(message: Message, from: Actor, to: Actor): Int = {
    var count: Int = 0
    if (message != null) {
      var aa: AbstractActor = to.asInstanceOf[AbstractActor]
      if (aa != null) {
        if (!aa.isShutdown() && !aa.isSuspended() && aa.willReceive(message.getSubject())) {
          var xmessage: DefaultMessage = ((message.asInstanceOf[DefaultMessage]).assignSender(from)).asInstanceOf[DefaultMessage]
          aa.addMessage(xmessage)
          xmessage.fireMessageListeners(new lib.MessageEvent(aa, xmessage, MessageStatus.SENT))
          sendCount+=1
          lastSendTime = new Date().getTime()
          if (recordSentMessages) {
            sentMessages.synchronized {
              val aname: String = aa.getName()
              var l: List[Message]  = sentMessages(aname)
              if (l == null) {
                l = Nil
                sentMessages += (aname -> l)
              }
              if (l.length < 100) {
                xmessage :: l
              }
            }
          }
          count+=1
          actors.synchronized {
            actors.notifyAll()
          }
        }
      }
    }
    count
  }
  
 
  
  override def send(m: Message, from :Actor, to:  List[Actor]): Int = {
    var count: Int = 0
    for (a <- to) {
      count += send(m, from, a)
    }
    count
  }

  override def send(m: Message, from: Actor, to: Traversable[Actor]): Int = {
    var count: Int  = 0
    for (a <- to) {
      count += send(m, from, a)
    }
     count
  }

  
  
  override  def send(m: Message, from: Actor, category: String): Int = {
    var count: Int = 0
    val xactors: Map[String, Actor] = cloneActors()
    var catMembers: List[Actor] = Nil
    for (k <- xactors.keySet ) {
      val to: Actor = xactors.getOrElse(k, null)
      if (category.equals(to.getCategory()) && (to.getMessageCount() < to.getMaxMessageCount())) {
        catMembers = to :: catMembers
      }
    }
    var min: Int = Integer.MAX_VALUE
    var amin: Actor = null
    for (a <- catMembers) {
      val mcount: Int = a.getMessageCount();
      if (mcount < min) {
        min = mcount
        amin = a
      }
    }
    if (amin != null) {
      count += send(m, from, amin)
       } else {
       throw new IllegalStateException("no capable actors for category: " + category);
    }
    count
  }
  
  
  override def broadcast(m :Message, from: Actor): Int = {
    var count: Int = 0
    val xactors: Map[String, Actor] = cloneActors()
    for (k <- xactors.keySet ) {
      val to: Actor = xactors.getOrElse(k, null)
      count += send(m, from, to)
    }
    count
  }



  
  override def getCategories(): Set[String] = {
    val xactors: Map[String, Actor] = cloneActors()
    var res: Set[String] = TreeSet()
    for (k <- xactors.keySet ) {
      val a: Actor = xactors.getOrElse(k, null)
       res = res + a.getCategory()
    }
    res
  }
  


  
  def getCategorySize(n: String): Int = {
    val xactors: Map[String, Actor] = cloneActors()
    var res: Int = 0
    for (k <- xactors.keySet ) {
      val a: Actor = xactors.getOrElse(k, null) 
      if (a.getCategory().equals(n)) {
        res += 1
      }
    }
    res
  }
  
  
  def awaitMessage(actor: AbstractActor): Unit = {
    actors.synchronized {
      waiters = waiters + (actor.getName() -> actor)
    }
  }


  
  
  protected def cloneActors(): Map[String, Actor] = {
    var xactors: Map[String, Actor] = null
    actors.synchronized {
      xactors = actors
    }
    xactors
  }

  
  
  
  
  protected var threads: List[Thread] = Nil
  
  protected var groupCount: Int = 0

  def getThreads(): List[Thread] = threads
  
  
  override def terminate(): Unit = {
    terminated = true
    running = false
    for (t <- threads) {
      t.interrupt()
    }
    actors.synchronized {
      for (key <- actors.keySet) {
        actors(key).deactivate()
      }
    }
    sentMessages = Map[String, List[Message]]()
    sendCount = 0 
    lastSendCount = 0
    clearDispatchCount()
  }

  
  override def createActor(clazz: Class[_ <: Actor], name: String): Actor = 
    createActor(clazz, name, null)

    
  override def createAndStartActor(clazz: Class[_ <: Actor], name: String): Actor = 
    createAndStartActor(clazz, name, null)

  override def createAndStartActor(clazz: Class[_ <: Actor], name: String, options: Map[String, Any]): Actor = {
    val res: Actor = createActor(clazz, name, options)
    startActor(res)
    res
  }
  
  override def createActor(clazz: Class[_ <: Actor], name: String, options: Map[String, Any]): Actor = {
    var a: AbstractActor = null
    actors.synchronized {
      if (!actors.contains(name)) {
        try {
          a = clazz.newInstance().asInstanceOf[AbstractActor]
          a.setName(name)
          a.setManager(this)
        } catch  { case e: Exception => 
          throw
          if(e.isInstanceOf[RuntimeException]) 
            e.asInstanceOf[RuntimeException]
          else
            new RuntimeException("mapped exception: " + e, e)
        }
      } else {
        throw new IllegalArgumentException("name already in use: " + name);
      }
    }
    a
  }
  
  
  override def startActor(actor: Actor): Unit = {
    if ((actor.asInstanceOf[AbstractActor]).getManager() != this) {
      throw new IllegalStateException("actor not owned by this manager")
    }
    var name: String = actor.getName()
    actors.synchronized {
      if (actors.contains(name)) {
        throw new IllegalStateException("already started")
      }
      (actor.asInstanceOf[AbstractActor]).shutdown = false
      actors += (name -> actor.asInstanceOf[AbstractActor])
      runnables += (name ->  actor.asInstanceOf[AbstractActor])
    }
    actor.activate()
  }

  protected var trendValue: Int = 0
  protected var maxTrendValue: Int = 10

  def getTrendValue(): Int = trendValue

  def setTrendValue(trendValue: Int): Unit = {
    this.trendValue = trendValue;
  }

  def getMaxTrendValue(): Int = maxTrendValue

  def setMaxTrendValue(maxTrendValue: Int): Unit = {
    this.maxTrendValue = maxTrendValue;
  }
  


  
  
  override def terminateAndWait(): Unit = {
    println("terminateAndWait waiting on termination of" + threads.length + "threads")
    terminate()
    waitForThreads()
  }

  
  def waitForThreads(): Unit = {
    if (!terminated) {
      throw new IllegalStateException("not terminated");
    }
    for (t <- threads) {
      try {
        t.join()
      } catch {case e: InterruptedException =>
      }
    }
  }

  
  
  
  
  
  
  
  
   
  class ActorRunnable extends Runnable {
    
    var hasThread: Boolean = false
    var actor: AbstractActor = null

     
    
    def run(): Unit = {
      var delay:Int = 1
      while (running) {
        try {
          if (!procesNextActor()) {
           actors.synchronized{
              actors.wait(100)
            }
            delay = Math.max(5, delay + 1)
          } else {
            delay = 1
          }
        } catch { 
          case e: InterruptedException =>
          case e: Exception => println("procesNextActor exception " + e)
        }
      }
    }

    protected def procesNextActor(): Boolean = {
      var run: Boolean = false
      var wait: Boolean = false
      var res: Boolean = false
      actor = null
      actors.synchronized {
        breakable {
          for (key <- runnables.keySet) {
            actor = runnables(key)
            runnables = runnables - key
            break
          }
        }
      }
      if (actor != null) {
        run = true
        actor.setHasThread(true)
        hasThread = true
        try {
          actor.run()
        } finally {
          actor.setHasThread(false)
          hasThread = false
        }
      } else {
        actors.synchronized {
          breakable { 
            for (key <- waiters.keySet) {
              actor = waiters(key)
              waiters = waiters - key
              break
            }
          }
        }
        if (actor != null) {
          wait = true;
          actor.setHasThread(true);
          hasThread = true;
          try {
            res = actor.receive();
            if (res) {
              incDispatchCount();
            }
          } finally {
            actor.setHasThread(false);
            hasThread = false;
          }
        }
      }
       
      run || res;
    }
    
    
    
  }
  
 override def getActorCount(clazz: Class[_]): Int = {
    var result: Int = 0
    if(clazz != null){
      actors.synchronized {
        for(k <- actors.keySet){
          val a: Actor = actors(k)
          if(clazz.isAssignableFrom(a.getClass())){
            result+=1
          }
        }
      }
    }else {
      actors.synchronized{
          result = actors.size
      }
    }
    result
  }
  





}








