package context
import expression.Identifier
import value.Value

class Environment extends collection.mutable.HashMap[Identifier, Value]
