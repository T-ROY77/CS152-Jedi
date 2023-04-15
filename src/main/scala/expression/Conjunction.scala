package expression
import context._
import expression._
import value._

//not sure
case class Conjunction(operands: List[Expression]) extends Expression:
  def execute(env: Environment): Value =
    operands.foldLeft(Boole(true)) { (acc, expr) =>
      if (acc.execute(env) == Boole(true)) acc
      else if (expr.execute(env) != Boole(true)) Boole(false)
      else acc
    }
