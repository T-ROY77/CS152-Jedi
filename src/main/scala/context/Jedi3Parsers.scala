package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi3Parsers extends Jedi2Parsers {

  // assignment ::= identifier ~ ":=" ~ expression

  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression

  // dereference ::= "[" ~ expression ~ "]"


  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")
  override def term: Parser[Expression]  = lambda | freeze | delay | funCall | block |  assignment | dereference | literal | "("~>expression<~")"

}