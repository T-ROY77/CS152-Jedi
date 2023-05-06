package expression

import context._
import expression._
import value._

//***************************
//execute function

case class Assignment(vbl: Identifier, update: Expression) extends SpecialForm :

  override def execute(env: Environment): Value =
    update.execute(env)
    Notification.OK


  override def toString(): String =
    vbl.toString()