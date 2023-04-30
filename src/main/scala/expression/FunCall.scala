package expression

import context._
import expression._
import value._

//****************
// execute code not working


case class FunCall(operator: Identifier, operands: List[Expression]) extends Expression:

  def execute(env: Environment) =
    val arguments = operands.map(_.execute(env))
    val maybeClosure = operator.execute(env)
    maybeClosure match {
      case closure: Closure => closure.apply(arguments)
      case _ => alu.execute(operator, arguments)
    }

