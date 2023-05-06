package value

import context._
import expression._
import value._

//A variable is a value that contains another value
case class Variable(content: Value) extends Value:
  def ==(other: Value): Boole =
    Boole(false)

  override def toString(): String =
    "[" + content.toString() + "]"

  override def hashCode(): Int =
    content.hashCode()