package value
import context._
import expression._
import value._

//Notifications are acknowledgements (e.g., "ok", "done", "unspecified") returned by expressions that produce side effects such as updating a variable, store, or environmen
case class Notification() extends Value:
  override def toString: String = super.toString


object Notification:
  val OK = Chars("ok")
  val DONE = Chars("done")
  val UNSPECIFIED =Chars("unspecified")