package value

import context._
import expression._
import value._

class Closure(parameters: List[Identifier], body: Expression, defEnv: Environment) extends Value:

  def apply(args: List[Value]): Value =
    val tempEnv = new Environment(defEnv)
    tempEnv.bulkPut(parameters, args)
    body.execute(tempEnv)

