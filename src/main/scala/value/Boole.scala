package value
import context._
import expression._
import value._

case class Boole(value: Boolean) extends Literal:




  def &&(other: Value): Boole =
    other match
      case x: Boole => Boole(this.value && x.value)
      case _ => throw new TypeException("Boolean operand required")

  def ||(other: Value): Boole =
    other match
      case x: Boole => Boole(this.value || x.value)
      case _ => throw new TypeException("Boolean operand required")

  def unary_! = Boole(!this.value)
  
  def equals(other: Value): Boole =
    other match
      case x: Boole => Boole(this.value == x.value)
      case _ => throw new TypeException("Boolean operand required")

  override def toString(): String =
    value.toString()

  override def hashCode(): Int =
    value.hashCode()

object Boole:
  val TRUE = Boole(true)
  val FALSE = Boole(false)