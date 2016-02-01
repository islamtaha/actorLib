

package lib
/*
 * @author eslam
 */
trait Message extends Any {
  def getSource(): Actor
  
  def getSubject(): String
  
  def getData(): Any
}