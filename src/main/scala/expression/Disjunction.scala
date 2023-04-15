package expression
import context._
import expression._
import value._


case class Disjunction(operand: Expression) extends SpecialForm: