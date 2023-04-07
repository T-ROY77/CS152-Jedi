package value

import context._

case class Exact(value: Int) extends Numeric with Ordered[Value] :

  override def +(other: Value): Addable =
    other match
      case x: Exact => Exact(this.value + x.value)
      case x: Inexact => Inexact(this.value.toDouble + x.value)
      case _ => throw new TypeException("Numeric operand required")

  override def /(other: Value): Numeric =
    other match
      case x: Exact =>
        if (x.value == 0) throw IllegalValueException("Can't divide by 0")
        else Exact(this.value / x.value)
      case x: Inexact =>
        if (x.value == 0.0) throw IllegalValueException("Can't divide by 0")
        else Inexact(this.value.toDouble / x.value)
      case _ => throw new TypeException("Numeric operand required")

  override def unary_- = Exact(-this.value)

  def compare(other: Value): Int =
    other match
      case x: Exact => this.value.compare(x.value)
      case x: Inexact => this.value.toDouble.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")

// *, -, equals, toString, hashCode