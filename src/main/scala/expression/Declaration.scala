package expression

import context._
import expression._
import value._


case class Declaration(identifier: Identifier, expr: Expression) extends SpecialForm:
  override def execute(env: Environment): Value =
    val value = expr.execute(env)
    val vals: List[Value] = List(value)
    val ids: List[Identifier] = List(identifier)
    env.bulkPut(ids, vals)
    Chars("ok")