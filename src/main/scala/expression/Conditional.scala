package expression
import context._
import expression._
import value._


case class Conditional(condition: Expression, consequent: Expression, alternative: Expression) extends SpecialForm:
  override def execute(env: Environment): Value =
    val cond = condition.execute(env)
    
    if (cond == null) Notification.UNSPECIFIED
    else if(cond == Boole(true)) consequent.execute(env) 
    else alternative.execute(env)