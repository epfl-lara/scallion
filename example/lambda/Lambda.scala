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

package example.lambda

import scallion.input._
import scallion.lexing._
import scallion.parsing._
import scallion.parsing.unfolds._

sealed trait Token
case object LambdaToken extends Token
case object DotToken extends Token
case class IdentifierToken(name: String) extends Token
case class ParenthesisToken(isOpen: Boolean) extends Token
case object SpaceToken extends Token
case class UnknownToken(content: String) extends Token

object LambdaLexer extends Lexers[Token, Char, Unit] with CharRegExps {

  val lexer = Lexer(
    // Lambda
    elem('\\') |> { cs => LambdaToken },

    // Dot
    elem('.') |> { cs => DotToken },

    // Parentheses
    elem('(') |> ParenthesisToken(true),
    elem(')') |> ParenthesisToken(false),

    // Spaces
    many1(whiteSpace) |> SpaceToken,

    // Identifiers
    many1(elem(_.isLetter)) |> { cs => IdentifierToken(cs.mkString) }

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
case object IdentifierClass extends TokenClass("<id>")
case object LambdaClass extends TokenClass(".")
case object DotClass extends TokenClass(".")
case class ParenthesisClass(isOpen: Boolean) extends TokenClass(if (isOpen) "(" else ")")
case object OtherClass extends TokenClass("?")

sealed abstract class Expr
case class Var(name: String) extends Expr
case class App(left: Expr, right: Expr) extends Expr
case class Abs(name: String, body: Expr) extends Expr

object LambdaParser extends Parsers[Token, TokenClass] {

  override def getKind(token: Token): TokenClass = token match {
    case IdentifierToken(_) => IdentifierClass
    case DotToken => DotClass
    case LambdaToken => LambdaClass
    case ParenthesisToken(o) => ParenthesisClass(o)
    case _ => OtherClass
  }

  val name: InvParser[String] = accept(IdentifierClass) {
    case IdentifierToken(n) => n
  } contramap {
    case n => Seq(IdentifierToken(n))
  }

  val lambda: InvParser[Unit] = elem(LambdaClass).always(LambdaToken)

  val dot: InvParser[Unit] = elem(DotClass).always(DotToken)

  def parens(isOpen: Boolean): InvParser[Unit] =
    elem(ParenthesisClass(isOpen)).always(ParenthesisToken(isOpen))

  val open = parens(true)
  val close = parens(false)

  lazy val variable: InvParser[Expr] = name.map {
    case n => Var(n)
  } contramap {
    case Var(n) => Seq(n)
    case _ => Seq()
  }

  lazy val expr: InvParser[Expr] = recursive(lambdaExpr | appExpr)

  lazy val basic: InvParser[Expr] = variable | open ~>~ expr ~<~ close

  lazy val lambdaExpr: InvParser[Expr] = (lambda ~>~ many1(name) ~<~ dot ~ expr).map {
    case ns ~ e => ns.foldRight(e) {
      case (n, acc) => Abs(n, acc)
    }
  } contramap {
    case acc@Abs(_, _) => {
      unfoldRight[String, Expr] {
        case Abs(n, acc) => (n, acc)
      }(acc)
    }
    case _ => Seq()
  }

  lazy val appExpr: InvParser[Expr] = many1(basic).map {
    xs => xs.reduceLeft(App(_, _))
  } contramap {
    acc => {
      unreduceLeft[Expr] {
        case App(l, r) => (l, r)
      }(acc)
    }
  }

  def unapply(value: Expr): Iterator[String] = expr.tokensOf(value).map(pretty(_))

  def pretty(tokens: Seq[Token]): String = {

    val space: ((Token, Token)) => String = {
      case (IdentifierToken(_), IdentifierToken(_)) => " "
      case _ => ""
    }

    val spaces = "" +: tokens.zip(tokens.tail).map(space)

    val strings = tokens.map {
      case LambdaToken => "\\"
      case IdentifierToken(n) => n
      case DotToken => "."
      case ParenthesisToken(isOpen) => if (isOpen) "(" else ")"
      case _ => "?"
    }

    spaces.zip(strings).map(x => x._1 + x._2).mkString("")
  }

  def apply(text: String): Option[Expr] = expr(LambdaLexer(text.toIterator)).getValue
}