package value
import context._
import expression._
import value._

//A variable is a value that contains another value
case class Variable(value: Value) extends Value:
  def ==(other: Value): Boole =
    Boole(false)

  override def toString(): String =
    value.toString()

  override def hashCode(): Int =
    value.hashCode()