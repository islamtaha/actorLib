package tests

import lib._
import java.util.Date

/**
 * @author eslam
 */
class TestActor extends TestableActor {
   override def activate(): Unit = {
    println("TestActor activate: %s", this)
    super.activate()
  }

  override def deactivate(): Unit = {
    println("TestActor deactivate: %s", this)
    super.deactivate()
  }
  
  protected override def runBody(): Unit = {
    println("TestActor:%s runBody: %s", getName(), this)
    DefaultActorTest.sleeper(1)
    val m: DefaultMessage = new DefaultMessage("init", 8)
    getManager().send(m, null, this)
  }

  
    
  protected override def loopBody(m: Message): Unit = {
    DefaultActorTest.sleeper(1)
    val subject = m.getSubject()
    if ("repeat".equals(subject)) {
      val count: Int =  m.getData().asInstanceOf[Int]
      println("TestActor:%s repeat(%d) %s: %s", getName(), count, m,this)
      if (count > 0) {
      val  m = new DefaultMessage("repeat", count - 1)
       val toName: String = "actor" + DefaultActorTest.nextInt(1000)
        val to: Actor = actorTest.getTestActors()(toName)
        if (to != null) {
          getManager().send(m, this, to)
        } else {
          println("repeat:%s to is null: %s", getName(), toName)
        }
      }
    } else if ("init".equals(subject)) {
      var count: Int =  m.getData().asInstanceOf[Int]
      count = DefaultActorTest.nextInt(count) + 1
      println("TestActor:%s init(%d): %s", getName(), count, this)
      for (i <- 0 until count) {
        DefaultActorTest.sleeper(1)
        val m = new DefaultMessage("repeat", count)
        val toName = "actor"+ DefaultActorTest.nextInt(1000)
        val to = actorTest.getTestActors().get(toName)
        if (to != null) {
          getManager().send(m, this, to)
        } else {
          println("init:%s to is null: %s", getName(), toName)
        }
        val dm: DefaultMessage = new DefaultMessage("repeat", count)
        dm.setDelayUntil(new Date().getTime() + (DefaultActorTest.nextInt(5) + 1) * 1000)
        getManager().send(dm, this, this.getClass().getSimpleName())
      }
    } else {
      println("TestActor:%s loopBody unknown subject: %s", getName(), subject)
    }
  }

}
