package expression
import context._
import expression._
import value._

case class Identifier(name: String) extends Expression {
  override def toString = name
  def execute(env: Environment) = env(this);
}
