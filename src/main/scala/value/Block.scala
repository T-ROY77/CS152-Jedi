package value

import context._
import expression._
import value._

class Block(expressions: List[Expression])  extends SpecialForm:

  def execute(env: Environment): Value =
    var lastValue: Value = null
    for (exp <- expressions) {
      lastValue = exp.execute(env)
    }
    lastValue


  override def toString(): String =
    execute.toString()

  override def hashCode(): Int =
    execute.hashCode()
