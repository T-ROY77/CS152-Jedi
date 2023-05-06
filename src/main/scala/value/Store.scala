package value

import context._
import expression._
import value._

import scala.collection.mutable.ArrayBuffer

case class Store(elems: ArrayBuffer[Value] = ArrayBuffer[Value]()) extends Value :


  // adds e to the end of store
  def add(e: Value) = elems += e


  // inserts e at position pos in this
  def put(e: Value, pos: Integer) = elems.insert(pos, e)

  // removes element at position pos from this
  def rem(pos: Integer) = elems.remove(pos)

  // returns element at position pos in this
  def get(pos: Integer): Value = elems(pos)

  // returns true ie this contains e
  def contains(e: Value): Boole = Boole(elems.contains(e))

  // returns the size of this
  def size: Integer = elems.size

  // returns "{e0 e1 e2 ...}"
  override def toString = "{" + elems.mkString(" ") + "}"

  // returns store containing the elements of this transformed by trans
  def map(trans: Closure): Store =
    val newStore = new Store()
    elems.foreach(e => newStore.add(trans.apply(List(e))))
    newStore

  // returns store containing the elements of this that passed test
  def filter(test: Closure): Store =
    val newStore = new Store()
    elems.foreach(e => if (test.apply(List(e)) == Boole(true)) newStore.add(e))
    newStore

  def ==(other: Value): Boole =
    Boole(false)

  override def hashCode(): Int =
    elems.hashCode()
