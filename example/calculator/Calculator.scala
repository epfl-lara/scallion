/* Copyright 2019 EPFL, Lausanne
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package example.calculator

import scallion._
import silex._

/* In this example, we show a lexer and parser for a calculator. */

// The tokens.
sealed trait Token
case class NumberToken(value: Int) extends Token {  // Numbers.
  override def toString = value.toString
}
case class OperatorToken(operator: Char) extends Token {  // Single char operators.
  override def toString = "" + operator
}
case class ParenthesisToken(isOpen: Boolean) extends Token {  // Parentheses.
  override def toString = if (isOpen) "(" else ")"
}
case object SpaceToken extends Token {  // Spaces.
  override def toString = " "
}
case class UnknownToken(content: String) extends Token {  // Unknowns.
  override def toString = "?"
}

// The following object describes the tokens of the calculator language,
// and provides methods to tokenize and display token sequences.
object CalcLexer extends Lexers with CharRegExps {

  type Token = example.calculator.Token  // Tokens.
  type Position = Unit  // Positions. Ignored here.

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
  ) onError {
    (cs, _) => UnknownToken(cs.mkString)
  }


  def apply(it: Iterator[Char]): Iterator[Token] = {
    val source = Source.fromIterator(it, NoPositioner)

    val tokens = lexer(source)

    tokens.filter((token: Token) => token != SpaceToken)
  }

  def unapply(it: Iterable[Token]): String = {
    it.mkString("")
  }
}

// Token kind. Models groups of tokens equivalent for the parser.
sealed abstract class TokenKind(text: String) {
  override def toString = text
}
case object NumberKind extends TokenKind("<number>")
case class OperatorKind(op: Char) extends TokenKind(op.toString)
case class ParenthesisKind(isOpen: Boolean) extends TokenKind(if (isOpen) "(" else ")")
case object OtherKind extends TokenKind("?")

// Expressions of our language.
sealed abstract class Expr
case class LitExpr(value: Int) extends Expr
case class BinaryExpr(op: Char, left: Expr, right: Expr) extends Expr
case class UnaryExpr(op: Char, inner: Expr) extends Expr

// The following object describes the syntax of the calculator language,
// and provides methods to parse and pretty print expressions.
object CalcParser extends Parsers {

  type Token = example.calculator.Token  // Type of tokens.
  type Kind = TokenKind  // Type of token kinds.

  import SafeImplicits._

  // Returns the kind of tokens.
  override def getKind(token: Token): TokenKind = token match {
    case NumberToken(_) => NumberKind
    case OperatorToken(c) => OperatorKind(c)
    case ParenthesisToken(o) => ParenthesisKind(o)
    case _ => OtherKind
  }

  // Syntax for a single number expression.
  val number: Syntax[Expr] = accept(NumberKind)({
    case NumberToken(n) => LitExpr(n)
  }, {
    case LitExpr(n) => Seq(NumberToken(n))
    case _ => Seq()
  })

  // Method that returns the syntax for a single character operator.
  def operator(char: Char): Syntax[Char] = accept(OperatorKind(char))({
    case _ => char
  }, {
    case `char` => Seq(OperatorToken(char))
    case _ => Seq()
  })

  val plus = operator('+')

  val minus = operator('-')

  val times = operator('*')

  val div = operator('/')

  val fac = operator('!')

  // Syntaxes for parentheses.
  def parens(isOpen: Boolean) = elem(ParenthesisKind(isOpen)).unit(ParenthesisToken(isOpen))

  val open = parens(true)

  val close = parens(false)

  // Syntax for basic expressions.
  lazy val basic: Syntax[Expr] = number | open.skip ~ expr ~ close.skip

  // Basic expression postfixed by factorial operators.
  lazy val postfixed: Syntax[Expr] = postfixes(basic, fac)({
    case (e, op) => UnaryExpr(op, e)
  }, {
    case UnaryExpr(op, e) => (e, op)
  })

  // Syntax for all expressions.
  lazy val expr: Syntax[Expr] = recursive {
    // Operators is a library combinator
    // used to build a syntax with
    // operator precedence and associativity.
    operators(postfixed)(
      times | div is LeftAssociative,
      plus | minus is LeftAssociative
    )({
      case (l, op, r) => BinaryExpr(op, l, r)
    }, {
      case BinaryExpr(op, l, r) => (l, op, r)
    })
  }

  // The LL(1) parser.
  val parser = Parser(expr)

  // The pretty printer.
  val printer = PrettyPrinter(expr)

  // Pretty prints expressions.
  def unapply(value: Expr): Iterator[Seq[Token]] = printer(value)

  // Parses expressions.
  def apply(it: Iterator[Token]): Option[Expr] = parser(it) match {
    case Parsed(value, _) => Some(value)
    case _ => None
  }
}

object Calculator {
  def main(args: Array[String]) {
    println("Parsing and pretty printing calculator expressions.")
    val expressions = Seq(
      "1 + 2",
      "3!",
      "(4 + 5) + 6",
      "7 + 8 * 9",
      "(3 * 2!) + 4 * 5 - (6! + 2) + 2")
    for (e <- expressions) {
      println("Raw expression: " + e)
      val parsed = CalcParser(CalcLexer(e.iterator)).get
      println("Parsed: " + parsed)
      val pretty = CalcParser.unapply(parsed).map(CalcLexer.unapply(_)).next()
      println("Pretty: " + pretty)
    }
  }
}
