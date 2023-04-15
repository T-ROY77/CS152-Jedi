package value
import context._
import expression._
import value._

//A variable is a value that contains another value
case class Variable(value: Value) extends Value:
  override def toString: String = super.toString