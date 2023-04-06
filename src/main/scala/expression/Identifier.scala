package expression

case class Identifier(val name: String) extends Expression {
  override def toString = name
  // def execute(env: Environment) = ???
}
