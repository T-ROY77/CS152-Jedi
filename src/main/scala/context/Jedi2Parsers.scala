package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi2Parsers extends Jedi1Parsers {

  // params parser
  def params: Parser[List[Identifier]] =
    "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
      case None => Nil
      case Some(id ~ ids) => id :: ids
    }

  // lambda parser
  def lambda: Parser[Expression] =
    "lambda" ~> params ~ expression ^^ {
      case params ~ body => Lambda(body, params)
    }

  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"
  def block: Parser[Expression] =
    "{" ~> expression ~ rep(";" ~> expression) <~ "}" ^^ {
      case e ~ Nil => e
      case e ~ exprs => Block(e :: exprs)
    }

  // override of term parser
  override def term: Parser[Expression] = lambda | funCall | block | literal | "(" ~> expression <~ ")"
}
