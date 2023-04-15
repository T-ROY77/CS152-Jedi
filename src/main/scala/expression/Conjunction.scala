package expression
import context._
import expression._
import value._


case class Conjunction(operand: Expression) extends SpecialForm: