package value
import context._
import expression._
import value._

trait Addable extends Literal:
  def +(other: Value): Addable
