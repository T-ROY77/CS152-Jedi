package context
import context._
import expression._
import value._

import scala.collection.mutable.*

class Environment(var extension: Environment = null) extends HashMap[Identifier, Value]:

  // used by closures to bind parameters to arguments
  def bulkPut(params: List[Identifier], args: List[Value]) =
    if (params.length != args.length) throw TypeException("# arguments != #parameters")
    for(i <- 0 until params.length) this.put(params(i), args(i))


  override def contains(name: Identifier): Boolean =
    super.contains(name) || (extension != null && extension.contains(name))


  override def apply(name: Identifier): Value =
    if (super.contains(name)) super.apply(name)
    else if (extension != null) extension.apply(name)
    else throw UndefinedException(name)