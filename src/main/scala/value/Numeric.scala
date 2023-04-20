package value

import context._
import expression._
import value._


trait Numeric extends Addable:
  def *(other: Value): Numeric

  def -(other: Value): Numeric

  def /(other: Value): Numeric

  def unary_- : Numeric



