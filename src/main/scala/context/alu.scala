package context

import context.*
import expression.*
import value.*

import scala.collection.mutable.ArrayBuffer


object alu:

  def execute(opcode: Identifier, args: List[Value]): Value =
    opcode.name match
      case "add" => add(args)            // n-ary
      case "mul" => mul(args)            // n-ary
      case "sub" => sub(args)            // n-ary
      case "div" => div(args)            // n-ary
      case "less" => less(args)          // binary
      case "equals" => same(args)        // binary
      case "more" => more(args)          // binary
      case "unequals" => unequals(args)  // binary
      case "not" => not(args)            // unary
      // variables
      case "dereference" => dereference(args)
      case "var" => makeVar(args)
      // primitive I/O ops:
      case "write" => write(args)
      // case "prompt" => prompt(args)
      // case "read" => read(args)
      // store ops
      case "store" => store(args)
      case "put" => put(args)
      case "rem" => rem(args)
      case "contains" => contains(args)
      case "map" => map(args)
      case "filter" => filter(args)
      case "get" => get(args)
      case "addLast" => addLast(args)
      case "size" => size(args)
      case _ => throw new Exception(opcode.name)


  private def add(args: List[Value]): Value =
    def helper(result: Addable, unseen: List[Value]): Addable =
      if (unseen.isEmpty) result
      else unseen.head match
        case h: Addable => helper(result + h, unseen.tail)
        case _ => throw TypeException("Inputs to + must be addable")

    if (args.size < 2) throw new TypeException("2 or more inputs required by +")
    args(0) match
      case n: Addable => helper(n, args.tail)
      case _ => throw new TypeException("Inputs to + must be addable")

  private def mul(args: List[Value]): Value =
    def helper(result: value.Numeric, unseen: List[Value]): value.Numeric =
      if (unseen.isEmpty) result
      else unseen.head match
        case h: value.Numeric => helper(result * h, unseen.tail)
        case _ => throw TypeException("Inputs to * must be numeric")

    if (args.size < 2) throw new TypeException("2 or more inputs required by *")
    args(0) match
      case n: value.Numeric => helper(n, args.tail)
      case _ => throw new TypeException("Inputs to * must be numeric")

  private def sub(args: List[Value]): Value =
    def helper(result: Numeric, unseen: List[Value]): Numeric =
      if (unseen.isEmpty) result
      else unseen.head match
        case h: Numeric => helper(result - h, unseen.tail)
        case _ => throw TypeException("Inputs to + must be Numeric")

    if (args.size < 2) throw new TypeException("2 or more inputs required by -")
    args(0) match
      case n: Numeric => helper(n, args.tail)
      case _ => throw new TypeException("Inputs to - must be Numeric")

  private def div(args: List[Value]): Value =
    def helper(result: Numeric, unseen: List[Value]): Numeric =
      if (unseen.isEmpty) result
      else unseen.head match
        case h: Numeric => helper(result / h, unseen.tail)
        case _ => throw TypeException("Inputs to / must be Numeric")

    if (args.size < 2) throw new TypeException("2 or more inputs required by /")
    args(0) match
      case n: Numeric => helper(n, args.tail)
      case _ => throw new TypeException("Inputs to / must be Numeric")

  private def less(args: List[Value]): Value =
    if (args.size != 2) throw new TypeException("2 inputs required by <")
    args(0) match
      case x: Ordered[Value] => Boole(x < args(1))
      case _ => throw TypeException("Inputs to < must be orderable")

  private def write(args: List[Value]): Value = { println(args(0)); Notification.DONE }

  private def same(args: List[Value]): Value =
    if (args.size != 2) throw new TypeException("2 inputs required by same")
    args(0) match
      case x: Ordered[Value] => Boole(x.equals(args(1)))
      case _ => throw TypeException("Inputs to same must be orderable")

  private def more(args: List[Value]): Value =
    if (args.size != 2) throw new TypeException("2 inputs required by more")
    args(0) match
      case x: Ordered[Value] => Boole(x > args(1))
      case _ => throw TypeException("Inputs to more must be orderable")

  private def unequals(args: List[Value]): Value =
    if (args.size != 2) throw new TypeException("2 inputs required by unequals")
    Boole(args(0) != args)

  private def not(args: List[Value]): Value =
    if (args.size != 1) throw new TypeException("1 input required by not")
    args(0) match
      case x: Numeric => -x
      case _ => throw TypeException("Inputs to not must be Numeric")

  // variable ops

  // returns the content of args(0)
  private def dereference(args: List[Value]): Value = args(0)

  // creates a new variable cobtaining args(0)
  private def makeVar(args: List[Value]) = new Variable(args(0))


  // store ops

  // returns a new store containing args
  private def store(args: List[Value]): Store =
    val buff = args.toBuffer
    val newStore = new Store(buff.asInstanceOf[ArrayBuffer[Value]])
    newStore


  // put(v: Value, p: Integer, s: Store) calls s.put(v, p)
  private def put(args: List[Value]) = {
    if (args.size != 3)
       throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
    if(!args(1).isInstanceOf[Integer] || !args(2).isInstanceOf[Store])
       throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
    args(2).asInstanceOf[Store].put(args(0), args(1).asInstanceOf[Integer])
    Notification.DONE
  }

  // rem(p: Integer, s: Store) calls s.rem(p)
  private def rem(args: List[Value]) =
    if (args.size != 2)
      throw new TypeException("expected signature: rem(p: integer, s: Store)")
    if (!args(0).isInstanceOf[Integer] || !args(1).isInstanceOf[Store])
      throw new TypeException("expected signature: rem(p: integer, s: Store)")
    args(1).asInstanceOf[Store].rem(args(0).asInstanceOf[Integer])
    Notification.DONE


  // get(p: Integer, s: Store) calls s.get(p)
  private def get(args: List[Value]) =
    if (args.size != 2)
      throw new TypeException("expected signature: get(p: integer, s: Store)")
    if (!args(0).isInstanceOf[Integer] || !args(1).isInstanceOf[Store])
      throw new TypeException("expected signature: get(p: integer, s: Store)")
    args(1).asInstanceOf[Store].get(args(0).asInstanceOf[Integer])
    Notification.DONE


  // map(f: Closure, s: Store) calls s.map(f)
  private def map(args: List[Value]) =
    if (args.size != 2)
      throw new TypeException("expected signature: map(f: Closure, s: Store)")
    if (!args(0).isInstanceOf[Closure] || !args(1).isInstanceOf[Store])
      throw new TypeException("expected signature: map(f: Closure, s: Store)")
    args(1).asInstanceOf[Store].map(args(0).asInstanceOf[Closure])
    Notification.DONE

  // filter(f: Closure, s: Store) calls s.filter(f)
  private def filter(args: List[Value]) =
    if (args.size != 2)
      throw new TypeException("expected signature: filter(f: Closure, s: Store)")
    if (!args(0).isInstanceOf[Closure] || !args(1).isInstanceOf[Store])
      throw new TypeException("expected signature: filter(f: Closure, s: Store)")
    args(1).asInstanceOf[Store].filter(args(0).asInstanceOf[Closure])
    Notification.DONE

  // contains(v: Value, s: Store) calls s.contains(v)
  private def contains(args: List[Value]) =
    if (args.size != 2)
      throw new TypeException("expected signature: contains(v: Value, s: Store)")
    if (!args(0).isInstanceOf[Value] || !args(1).isInstanceOf[Store])
      throw new TypeException("expected signature: contains(v: Value, s: Store)")
    args(1).asInstanceOf[Store].contains(args(0))
    Notification.DONE

  // addLast(v: Value, s: Store) calls s.add(v)
  private def addLast(args: List[Value]) =
    if (args.size != 2)
      throw new TypeException("expected signature: addLast(v: Value, s: Store)")
    if (!args(0).isInstanceOf[Value] || !args(1).isInstanceOf[Store])
      throw new TypeException("expected signature: addLast(v: Value, s: Store)")
    args(1).asInstanceOf[Store].add(args(0))
    Notification.DONE

  // size(s: Store) calls s.size
  private def size(args: List[Value]) =
    if (args.size != 1)
      throw new TypeException("expected signature: size(s: Store)")
    if (!args(0).isInstanceOf[Store])
      throw new TypeException("expected signature: size(s: Store)")
    args(1).asInstanceOf[Store].size
    Notification.DONE

