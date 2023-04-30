package value

import context._
import expression._
import value._


trait Value:

  //def ==(other: Value): Value

  def toString(): String

  def hashCode(): Int

