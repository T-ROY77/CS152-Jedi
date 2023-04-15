package value
import context._
import expression._
import value._


case class Chars(value: String) extends Addable with Ordered[Value] :
  
  def size(): Exact =
    Exact(value.length)

  def compare(other: Value): Int =
    other match
      case x: Chars => this.value.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")
  
  def subChars(to: Exact, from: Exact): Chars =
    Chars(value.substring(to.value, from.value))
