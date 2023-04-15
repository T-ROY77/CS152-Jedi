package value
import context._
import expression._
import value._


case class Chars(value: String) extends Addable with Ordered[Value] :
  
  def size(): Exact =
    Exact(value.length)
  
  
  //def subChars(to: Exact, from: Exact): Chars =
