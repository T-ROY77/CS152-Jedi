package expression
import context._
import expression._
import value._


case class Conditional(condition: Expression) extends SpecialForm: