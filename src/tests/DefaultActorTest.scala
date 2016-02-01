package tests

import lib._
import scala.collection._
import scala.collection.convert.decorateAsScala._
import java.util.concurrent.ConcurrentHashMap
import scala.util.Random
import java.util.Date
import scala.util.control.Breaks._
import javax.swing.event.ChangeListener
import javax.swing.event.ChangeEvent
import lib.DefaultActorManager

/**
 * @author eslam
 */

class DefaultActorTest {

  

  protected var listeners: List[ChangeListener] = Nil

  def addChangeListener(l: ChangeListener): Unit = {
    if (!listeners.contains(l)) {
      listeners = l :: listeners 
    }
  }

  def removeChangeListener(l: ChangeListener): Unit = {
    listeners = listeners diff List(l)
  }

  protected def fireChangeListeners(e: ChangeEvent): Unit = {
    for (l <- listeners) {
      l.stateChanged(e)
    }
  }

  private var testActors : concurrent.Map[String, Actor] = new ConcurrentHashMap().asScala
  
  var actorManager: DefaultActorManager = null

  
  protected def getManager(): DefaultActorManager = {
    val am: DefaultActorManager =  if(actorManager != null) actorManager else new DefaultActorManager()
    am
  }

  
  
  
  
  protected var stepCount: Int = 120

  def setStepCount(stepCount: Int): Unit =  {
    this.stepCount = stepCount
  }

  def getStepCount(): Int = stepCount

  protected var threadCount: Int = 10

  def getThreadCount(): Int = threadCount

  def setThreadCount(threadCount: Int): Unit = {
    this.threadCount = threadCount
  }

  def setTestActors(testActors: concurrent.Map[String, Actor]): Unit = {
    this.testActors = testActors
  }

  def getTestActors(): concurrent.Map[String, Actor] = testActors
  

  def getActorManager(): DefaultActorManager = actorManager

  def setActorManager(actorManager: DefaultActorManager): Unit = {
    this.actorManager = actorManager
  }
  
  protected var done: Boolean = false

  def terminateRun(): Unit = {
    done = true
  }
  
  protected var title: String = null

  def getTitle(): String = title
  

  
  def run(args: Array[String]): Unit = {
    val instance: DefaultActorManager = new DefaultActorManager()
    var sc: Int = stepCount
    var tc: Int = threadCount
    var doTest: Boolean = false
    var doProduceConsume = false
    var doQuicksort = false
    var doMapReduce = false
    var doVirusScan = false
    
    title = ""
    for (i <- 0 until args.length) {
      var arg: String = args(i).toLowerCase()
      if (arg.startsWith("-")) {
        arg = arg.substring(1)
        if (arg.toLowerCase().startsWith("stepcount:")) {
          sc = Integer.parseInt(arg.substring("stepcount:".length()))
        } else if (arg.startsWith("sc:")) {
          sc = Integer.parseInt(arg.substring("sc:".length()));
        } else if (arg.toLowerCase().startsWith("threadcount:")) {
          tc = Integer.parseInt(arg.substring("threadcount:".length()))
        } else if (arg.startsWith("tc:")) {
          tc = Integer.parseInt(arg.substring("tc:".length()))
        } else {
          System.out.printf("Unknown switch: %s%n", arg)
        }
      } else {
        if (arg.equalsIgnoreCase("test") || arg.equalsIgnoreCase("countdown") || arg.equalsIgnoreCase("cd")) {
          doTest = true
        } else if (arg.equalsIgnoreCase("producerconsumer") || arg.equalsIgnoreCase("pc")) {
          doProduceConsume = true
        } else if (arg.equalsIgnoreCase("quicksort") || arg.equalsIgnoreCase("qs")) {
          doQuicksort = true
        } else if (arg.equalsIgnoreCase("mapreduce") || arg.equalsIgnoreCase("mr")) {
          doMapReduce = true
        } else if (arg.equalsIgnoreCase("virusscan") || arg.equalsIgnoreCase("vs")) {
          doVirusScan = true
        } else if (arg.equalsIgnoreCase("all")) {
          doProduceConsume = true
          doTest = true
          doMapReduce = true
          doQuicksort = true
          doVirusScan = true
        } else {
          System.out.printf("Unknown parameter: %s%n", arg)
        }
      }
    }
    if (!doTest && !doProduceConsume && !doQuicksort && !doMapReduce && !doVirusScan) {
      doTest = true
    }
    if (doTest) {
      if (title.length() > 0) {
        title += " "
      }
      title += "(Countdown Test)"
    }
    if (doProduceConsume) {
      if (title.length() > 0) {
        title += " "
      }
      title += "(Producer+Consumer)"
    }
    if (doQuicksort) {
      if (title.length() > 0) {
        title += " "
      }
      title += "(Quicksort)"
    }
    if (doMapReduce) {
      if (title.length() > 0) {
        title += " "
      }
      title += "(MapReduce)"
    }
    if (doVirusScan) {
      if (title.length() > 0) {
        title += " "
      }
      title += "(VirusScan)";
    }

    val am = getManager()
    try {
      var options: Map[String, Any] = Map[String, Any]()
      val helper: DefaultActorManager = new DefaultActorManager()
      options += (helper.ACTOR_THREAD_COUNT ->  tc)
      am.initialize(options.asInstanceOf[scala.collection.immutable.Map[String, Actor]])
      if (doTest) {
        for (i <- 0 until DefaultActorTest.COMMON_ACTOR_COUNT) {
          var a: Actor = am.createActor(classOf[TestActor], "common%02d".format(i))
          if (a.isInstanceOf[TestableActor]) {
            val ta =  a.asInstanceOf[TestableActor]
            ta.setActorTest(this)
          }
          a.setCategory(classOf[TestActor].getSimpleName())
          getTestActors().put(a.getName(), a)
          println("created: %s", a)
        }
        for (i <- 0 until DefaultActorTest.TEST_ACTOR_COUNT) {
          var a: Actor = am.createActor(classOf[TestActor], "actor%02d".format(i))
          if (a.isInstanceOf[TestableActor]) {
            val ta = a.asInstanceOf[TestableActor]
            ta.setActorTest(this)
          }
          getTestActors().put(a.getName(), a)
          println("created: %s", a)
        }
      }
      
     
      for(k <- getTestActors().keySet ){
        am.startActor(getTestActors()(k))
      }

      for (i <- sc until 0) {
        breakable{
          if (done) {
            break
          }
        }
        val now: Long = new Date().getTime()
        breakable{
        if (am.getActiveRunnableCount() == 0) {
          if (now - am.getLastDispatchTime() > DefaultActorTest.MAX_IDLE_SECONDS * 1000
              && now - am.getLastSendTime() > DefaultActorTest.MAX_IDLE_SECONDS * 1000) {
            break
          }
        }
        }
        setStepCount(i)
        fireChangeListeners(new ChangeEvent(this))
        if (i < 10 || i % 10 == 0) {
          println("main waiting: %d...", i)
        }
        DefaultActorTest.sleeper(1)
      }
      setStepCount(0)
      fireChangeListeners(new ChangeEvent(this))

      println("main terminating")
      am.terminateAndWait()
    } catch { case e: Exception =>
      e.printStackTrace()
    }

    

  }

 
  
}

object DefaultActorTest {
  
  def sleeper(seconds: Int): Unit = {
    val millis: Int = seconds * 1000 + -50 + nextInt(100)
    sleep(millis)
  }
    
  val rand: Random = new Random()

  def nextInt(limit: Int): Int = rand.nextInt(limit)
  
  def sleep(millis: Long): Unit = {
    if (millis >= 0) {
      try {
        Thread.sleep(millis)
      } catch {
        case e: InterruptedException => 
      }
    }
  }
  
  val MAX_IDLE_SECONDS: Int = 10

  
  val COMMON_ACTOR_COUNT: Int = 10
  val TEST_ACTOR_COUNT: Int = 25
  val PRODUCER_ACTOR_COUNT: Int = 25

  def main(args: Array[String]) = {
    val at: DefaultActorTest = new DefaultActorTest()
    at.run(args)
    println("Done")
  }


  
}