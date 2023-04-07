package value

import context._

case class Inexact(value: Double) extends Numeric with Ordered[Value] :

  override def +(other: Value): Addable =
    other match
      case x: Exact => Exact(this.value.toInt + x.value)
      case x: Inexact => Inexact(this.value + x.value)
      case _ => throw new TypeException("Numeric operand required")

  override def /(other: Value): Numeric =
    other match
      case x: Exact =>
        if (x.value == 0) throw IllegalValueException("Can't divide by 0")
        else Exact(this.value.toInt / x.value)
      case x: Inexact =>
        if (x.value == 0.0) throw IllegalValueException("Can't divide by 0")
        else Inexact(this.value / x.value)
      case _ => throw new TypeException("Numeric operand required")

  override def unary_- = Inexact(-this.value)

  def compare(other: Value): Int =
    other match
      case x: Exact => this.value.toInt.compare(x.value)
      case x: Inexact => this.value.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")

// *, -, equals, toString, hashCode