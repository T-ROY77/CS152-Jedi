package value

import context.TypeException
import expression.Literal

case class Boole(value: Boolean) extends Literal with Value:
  def &&(other: Value): Boole =
    other match
      case x: Boole => Boole(this.value && x.value)
      case _ => throw new TypeException("Boolean operand required")

  def ||(other: Value): Boole =
    other match
      case x: Boole => Boole(this.value || x.value)
      case _ => throw new TypeException("Boolean operand required")

  def unary_! = Boole(!this.value)