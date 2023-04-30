package value

import context._
import expression._
import value._

class Lambda(body: Expression, parameters: List[Identifier]) extends SpecialForm:

  def execute(env: Environment): Closure =
    new Closure(parameters, body, env)



