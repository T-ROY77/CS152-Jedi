package value

trait Numeric extends Addable:
  def *(other: Value): Numeric =
    this * other

  def -(other: Value): Numeric =
    this - other

  def /(other: Value): Numeric =
    this / other

  def unary_- = this
