package value
import context._
import expression._
import value._


trait Numeric extends Addable:
  def *(other: Value): Numeric =
    this * other

  def -(other: Value): Numeric =
    this - other

  def /(other: Value): Numeric =
    this / other

  def unary_- = this
