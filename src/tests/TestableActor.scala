package tests

import lib.AbstractActor
import lib.Actor



/**
 * @author eslam
 */
abstract class TestableActor extends AbstractActor {
  var actorTest: DefaultActorTest = null
  val x: Actor = null
  def getActorTest(): DefaultActorTest = actorTest

  def setActorTest(actorTest: DefaultActorTest): Unit = {
    this.actorTest = actorTest
  }

}
