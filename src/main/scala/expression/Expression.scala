package expression

trait Expression: 
  def execute(env: Environment): Value
