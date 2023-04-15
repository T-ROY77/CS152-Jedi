package value
import context._
import expression._
import value._

//Notifications are acknowledgements (e.g., "ok", "done", "unspecified") returned by expressions that produce side effects such as updating a variable, store, or environmen
object Notification extends Value:
  val OK = Chars("ok")
  val DONE = Chars("done")
  val UNSPECIFIED =Chars("unspecified")

  def ==(other: Value): Boole =
    Boole(false)

  override def toString(): String =
    " "

  override def hashCode(): Int =
    0