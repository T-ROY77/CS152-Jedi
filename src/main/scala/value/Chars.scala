package value
import context._
import expression._
import value._


case class Chars(value: String) extends Addable with Ordered[Value] :
  
  def +(other: Value): Addable =
    other match
      case x: Addable => Chars(value.concat(x.toString()))
      case _ => throw new TypeException("Inputs to + must be addable")

  def size(): Exact =
    Exact(value.length)

  def compare(other: Value): Int =
    other match
      case x: Chars => this.value.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")
  
  def subChars(to: Exact, from: Exact): Chars =
    Chars(value.substring(to.value, from.value))


  def ==(other: Value): Boole =
    other match
      case x: Boole => Boole(this.value.equals(x.value))
      case _ => throw new TypeException("Chars operand required")

  override def toString(): String =
    value

  override def hashCode(): Int =
    value.hashCode()
