package expression
import context._
import expression._
import value._


case class Conjunction(operands: List[Expression]) extends SpecialForm:
  override def execute(env: Environment): Value =
    if (operands.isEmpty) true
    else if (!operands.head) false
    else Conjunction(operands.tail: _*)