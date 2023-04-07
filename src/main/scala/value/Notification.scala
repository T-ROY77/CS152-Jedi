package value

case class Notification() extends Value:
  override def toString: String = super.toString


object Notification extends Value:
  val OK = "ok"
  val DONE = "done"
  val UNSPECIFIED = "unspecified"
