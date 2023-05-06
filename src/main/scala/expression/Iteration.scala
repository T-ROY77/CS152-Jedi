package expression

import context._
import expression._
import value._


case class Iteration(condition: Expression, body: Expression) extends SpecialForm :

  override def execute(env: Environment): Value =
    while(condition.execute(env) != Boole(true)){
      body.execute(env)
    }
    Notification.DONE

  override def toString(): String =
    condition.toString()
