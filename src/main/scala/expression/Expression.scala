package expression
import context._
import expression._
import value._


trait Expression: 
  def execute(env: Environment): Value
