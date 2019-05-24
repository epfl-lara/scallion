
package example.calculator

import scallion._
import scallion.util._

sealed trait Token
case class NumberToken(value: Int) extends Token
case class OperatorToken(operator: Char) extends Token
case class ParenthesisToken(isOpen: Boolean) extends Token
case object SpaceToken extends Token
case class UnknownToken(content: String) extends Token

object CalcLexer extends Lexers[Token, Char, Unit] with CharRegExps {

  val lexer = Lexer(
    // Operators
    oneOf("-+/*!")
      |> { cs => OperatorToken(cs.head) },

    // Parentheses
    elem('(') |> ParenthesisToken(true),
    elem(')') |> ParenthesisToken(false),

    // Spaces
    many1(whiteSpace) |> SpaceToken,

    // Numbers
    {
      elem('0') |
      nonZero ~ many(digit)
    }
      |> { cs => NumberToken(cs.mkString.toInt) }
  )


  def apply(it: Iterator[Char]): Iterator[Token] = {
    val source = new IteratorSource((), it) {
      override def increment(pos: Unit, char: Char): Unit = ()
    }

    lexer(source, (content, range) => UnknownToken(content.mkString), _ == SpaceToken)
  }
}

sealed abstract class TokenClass(text: String) {
  override def toString = text
}
case object NumberClass extends TokenClass("<number>")
case class OperatorClass(op: Char) extends TokenClass(op.toString)
case class ParenthesisClass(isOpen: Boolean) extends TokenClass(if (isOpen) "(" else ")")
case object OtherClass extends TokenClass("?")

object CalcParser extends Parsers[Token, TokenClass] with Operators
    with Graphs[TokenClass] with Grammars[TokenClass] {

  override def getKind(token: Token): TokenClass = token match {
    case NumberToken(_) => NumberClass
    case OperatorToken(c) => OperatorClass(c)
    case ParenthesisToken(o) => ParenthesisClass(o)
    case _ => OtherClass
  }

  val number = accept(NumberClass) {
    case NumberToken(n) => n
  }

  val plus = accept(OperatorClass('+')) {
    case _ => (x: Int, y: Int) => x + y
  }

  val minus = accept(OperatorClass('-')) {
    case _ => (x: Int, y: Int) => x - y
  }

  val times = accept(OperatorClass('*')) {
    case _ => (x: Int, y: Int) => x * y
  }

  val div = accept(OperatorClass('/')) {
    case _ => (x: Int, y: Int) => x / y
  }

  val fac = accept(OperatorClass('!')) {
    case _ => (x: Int) => 1.to(x).product
  }

  val uMinus = accept(OperatorClass('-')) {
    case _ => (x: Int) => -x
  }

  val uPlus = accept(OperatorClass('+')) {
    case _ => (x: Int) => x
  }

  val open = elem(ParenthesisClass(true))
  val close = elem(ParenthesisClass(false))

  lazy val basic: Parser[Int] = number | open ~>~ value ~<~ close

  lazy val value: Parser[Int] = recursive {
    operators(prefixes(uPlus | uMinus, postfixes(basic, fac)))(
      times | div is LeftAssociative,
      plus | minus is LeftAssociative)
  }

  def apply(it: Iterator[Token]): Option[Int] = value(it) match {
    case Parsed(value, _) => Some(value)
    case _ => None
  }
}