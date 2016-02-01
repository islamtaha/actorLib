

/*
 * @author eslam
 */
package lib

trait ActorManager {
 
  def createActor(clazz: Class[_ <: Actor], name: String): Actor
  
  def createAndStartActor(clazz: Class[_ <: Actor], name: String): Actor

  def createActor(clazz: Class[_ <: Actor], name: String, options: Map[String, Any]): Actor
  
  def createAndStartActor(clazz: Class[_ <: Actor], name: String, options: Map[String, Any]): Actor
  
  def startActor(actor: Actor): Unit
  
  def detachActor(actor: Actor): Unit

  def send(message: Message, from: Actor, to: Actor): Int
  
  def send(message: Message, from: Actor, to: List[Actor]): Int
  
  def send(message: Message, from: Actor, to: Traversable[Actor]): Int
  
  def send(message: Message, from: Actor, category: String): Int
  
  def broadcast(message: Message, from: Actor): Int
  
  def getCategories(): Set[String]

  def initialize(): Unit

  def initialize(options: Map[String, Any]): Unit
  
  def terminateAndWait(): Unit
  
  def terminate(): Unit
  
  def getActorCount(clazz: Class[_]): Int
  
}  