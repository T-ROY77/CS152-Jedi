package value
import context._
import expression._
import value._


import scala.collection.mutable

case class Store(buff: mutable.Buffer[Value]) extends Value:
  def ==(other: Value): Boole =
    Boole(false)

  override def toString(): String =
    buff.toString()

  override def hashCode(): Int =
    buff.hashCode()
