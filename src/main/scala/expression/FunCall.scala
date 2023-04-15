package expression

import context._
import value._

case class FunCall(operator: Identifier, operands: List[Expression]) extends Expression {
  def execute(env: Environment): Value = {
    val arguments = operands.map(_.execute(env))
    alu.execute(operator, arguments)
  }
}
