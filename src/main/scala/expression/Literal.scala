package expression

import context._
import expression._
import value._

trait Literal extends Expression with Value:
  override def execute(env: Environment): Value = this


