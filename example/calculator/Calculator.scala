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

import scallion.input._
import scallion.lexical._
import scallion.syntactic._

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
  ) onError {
    (cs, _) => UnknownToken(cs.mkString)
  }


  def apply(it: Iterator[Char]): Iterator[Token] = {
    val source = Source.fromIterator(it, NoPositioner)

    val tokens = lexer(source)

    tokens.filter((token: Token) => token != SpaceToken)
  }
}

sealed abstract class TokenClass(text: String) {
  override def toString = text
}
case object NumberClass extends TokenClass("<number>")
case class OperatorClass(op: Char) extends TokenClass(op.toString)
case class ParenthesisClass(isOpen: Boolean) extends TokenClass(if (isOpen) "(" else ")")
case object OtherClass extends TokenClass("?")

sealed abstract class Expr
case class LitExpr(value: Int) extends Expr
case class BinaryExpr(op: Char, left: Expr, right: Expr) extends Expr
case class UnaryExpr(op: Char, inner: Expr) extends Expr

object CalcSyntax extends Syntaxes[Token, TokenClass] with Operators {

  type S[A] = Syntax[A, A]

  override def getKind(token: Token): TokenClass = token match {
    case NumberToken(_) => NumberClass
    case OperatorToken(c) => OperatorClass(c)
    case ParenthesisToken(o) => ParenthesisClass(o)
    case _ => OtherClass
  }

  val number: S[Expr] = accept(NumberClass) {
    case NumberToken(n) => LitExpr(n)
  } contramap {
    case LitExpr(n) => Seq(NumberToken(n))
    case _ => Seq()
  }

  def binOp(char: Char): Syntax[Char, (Expr, Expr) => Expr] = accept(OperatorClass(char)) {
    case _ => (l: Expr, r: Expr) => BinaryExpr(char, l, r)
  } contramap {
    case `char` => Seq(OperatorToken(char))
    case _ => Seq()
  }

  val plus = binOp('+')

  val minus = binOp('-')

  val times = binOp('*')

  val div = binOp('/')

  val fac: Syntax[Char, Expr => Expr] = accept(OperatorClass('!')) {
    case _ => (x: Expr) => UnaryExpr('!', x)
  } contramap {
    case '!' => Seq(OperatorToken('!'))
    case _ => Seq()
  }

  def parens(isOpen: Boolean) = elem(ParenthesisClass(isOpen)).unit(ParenthesisToken(isOpen))
  val open = parens(true)
  val close = parens(false)

  lazy val basic: S[Expr] = number | open ~>~ value ~<~ close

  lazy val postfixed: S[Expr] = postfixes[Expr, Char, Expr](basic, fac, {
    case UnaryExpr(op, e) => (e, op)
  })

  val reverses: PartialFunction[Expr, (Expr, Char, Expr)] = {
    case BinaryExpr(op, l, r) => (l, op, r)
  }

  lazy val value: S[Expr] = recursive {
    operators(postfixed, reverses)(
      times | div is LeftAssociative,
      plus | minus is LeftAssociative)
  }

  def unapply(expr: Expr): Iterator[Seq[Token]] = value.tokensOf(expr)

  def apply(it: Iterator[Token]): Option[Expr] = value(it) match {
    case Parsed(value, _) => Some(value)
    case _ => None
  }
}