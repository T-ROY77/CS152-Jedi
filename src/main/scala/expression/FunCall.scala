package expression

import context._
import expression._
import value._

case class FunCall(operator: Identifier, operands: List[Expression]) extends Expression:

    def execute(env: Environment) =
      val arguments = operands.map(_.execute(env))
      try {
        val maybeClosure = operator.execute(env)
        maybeClosure match {
                case closure: Closure => closure.apply(arguments)
              }
      }catch {
        case e: Exception => alu.execute(operator, arguments)
      }



