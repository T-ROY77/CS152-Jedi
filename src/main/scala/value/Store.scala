package value
import context._
import expression._
import value._


import scala.collection.mutable

case class Store(buff: mutable.Buffer[Value]) extends Value:
  override def toString: String = super.toString
