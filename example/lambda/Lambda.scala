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
import scallion.syntactic._
import scallion.syntactic.Unfolds._

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

object LambdaSyntax extends Syntaxes[Token, TokenClass] {

  type Syntax[A] = Parser[A, A]

  override def getKind(token: Token): TokenClass = token match {
    case IdentifierToken(_) => IdentifierClass
    case DotToken => DotClass
    case LambdaToken => LambdaClass
    case ParenthesisToken(o) => ParenthesisClass(o)
    case _ => OtherClass
  }

  // Accept any token of the kind IdentifierClass,
  val name: Syntax[String] = accept(IdentifierClass) {
    // Extract the string from them.
    case IdentifierToken(n) => n
  } contramap {
    // Generates all tokens which could have led to the string.
    // (In this case, only one.)
    case n => Seq(IdentifierToken(n))
  }

  // Accept any token of the kind LambdaClass, which will always be LambdaToken.
  val lambda: Syntax[Unit] = elem(LambdaClass).always(LambdaToken)

  // Accept any token of the kind DotClass, which will always be DotToken.
  val dot: Syntax[Unit] = elem(DotClass).always(DotToken)

  // Accepts an open or a close parenthesis.
  def parens(isOpen: Boolean): Syntax[Unit] =
    elem(ParenthesisClass(isOpen)).always(ParenthesisToken(isOpen))

  // Open parenthesis.
  val open = parens(true)

  // Close parenthesis.
  val close = parens(false)

  // Turn a name into an expression.
  val variable: Syntax[Expr] = name.map {
    // Turn the string into a variable.
    case n => Var(n)
  } contramap {
    // Turn the expression into all strings that could have generated it.
    case Var(n) => Seq(n)
    case _ => Seq()
  }

  // The syntax for expressions, which is the main syntax.
  lazy val expr: Syntax[Expr] = recursive {
    // Accepts either a lambda expression or an application.
    // `appExpr` also includes single basic expressions.
    lambdaExpr | appExpr
  }

  // Basic expressions. Simply a variable or an expression in parenthesis.
  lazy val basic: Syntax[Expr] = variable | open ~>~ expr ~<~ close

  // Lambda expression.
  lazy val lambdaExpr: Syntax[Expr] = (lambda ~>~ many1(name) ~<~ dot ~ expr).map {
    // Given a sequence of names and the expression body, we create the corresponding lambda.
    case ns ~ e => ns.foldRight(e) {  // We do so by using `foldRight`.
      case (n, acc) => Abs(n, acc)  // Create an `Abs` from the name and body.
    }
  } contramap {
    // We provide the inverse transformation.
    // Given an expression, we decompose it into all its arguments.
    case acc@Abs(_, _) => {
      // To do so, we simply use `unfoldRight`.
      unfoldRight[String, Expr] {
        case Abs(n, acc) => (n, acc)  // We split the `Abs` into its two components.
      }(acc)
    }
    // If the value is not an `Abs`, we have no inverses.
    case _ => Seq()
  }

  // Application, which consists of a sequence of at least one basic expressions.
  lazy val appExpr: Syntax[Expr] = many1(basic).map {
    // We reduce all expressions into a single one using `reduceLeft`.
    xs => xs.reduceLeft(App(_, _))
  } contramap {
    // We provide also the inverse operation.
    // We unfold arguments using `unreduceLeft`.
    acc => {
      // We use `unreduceLeft` to unpack the value.
      unreduceLeft[Expr] {
        case App(l, r) => (l, r)  // We split the `App` into its two components.
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