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

import scallion._
import scallion.util.Unfolds._
import silex._

/* In this example, we show a lexer and parser for lambda calculus. */


// The tokens used in our lambda calculus language.
sealed trait Token
case object LambdaToken extends Token  // A single lambda.
case object DotToken extends Token  // A single dot.
case class IdentifierToken(name: String) extends Token  // A (string) identifier.
case class ParenthesisToken(isOpen: Boolean) extends Token // A parenthesis.
case object SpaceToken extends Token  // Space.
case class UnknownToken(content: String) extends Token  // Unknown.

// The following object describes the tokens of lambda calculus,
// and provides methods to tokenize and display token sequences.
object LambdaLexer extends Lexers with CharRegExps {

  type Token = example.lambda.Token  // The type of tokens.
  type Position = Unit  // The type of positions. In this example, we ignore positions.

  // Description of the lexer.
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

  // Tokenize a sequence of characters into a sequence of tokens.
  def apply(it: Iterator[Char]): Iterator[Token] = {
    val source = Source.fromIterator(it, NoPositioner)

    val tokens = lexer(source)

    tokens.filter((token: Token) => token != SpaceToken)
  }

  // Turns a sequence of tokens into a sequence of characters.
  def unapply(tokens: Iterator[Token]): String = {
    val ts = tokens.toSeq

    val space: ((Token, Token)) => String = {
      case (IdentifierToken(_), IdentifierToken(_)) => " "
      case (DotToken, _) => " "
      case _ => ""
    }

    val spaces = "" +: ts.zip(ts.tail).map(space)

    val strings = ts.map {
      case LambdaToken => "\\"
      case IdentifierToken(n) => n
      case DotToken => "."
      case ParenthesisToken(isOpen) => if (isOpen) "(" else ")"
      case _ => "?"
    }

    spaces.zip(strings).map(x => x._1 + x._2).mkString("")
  }
}

// Token kind. Models groups of tokens equivalent for the parser.
sealed abstract class TokenKind(text: String) {
  override def toString = text
}
case object IdentifierKind extends TokenKind("<id>")
case object LambdaKind extends TokenKind("\\")
case object DotKind extends TokenKind(".")
case class ParenthesisKind(isOpen: Boolean) extends TokenKind(if (isOpen) "(" else ")")
case object OtherKind extends TokenKind("?")

// The lambda calculus expressions.
sealed abstract class Expr
case class Var(name: String) extends Expr  // Variables.
case class App(left: Expr, right: Expr) extends Expr  // Function application.
case class Abs(name: String, body: Expr) extends Expr  // Lambda abstraction.

// The following object describes the syntax of lambda calculus,
// and provides methods to parse and pretty print expressions.
object LambdaSyntax extends Parsers {

  type Token = example.lambda.Token  // The type of tokens.
  type Kind = TokenKind  // The type of token types.

  import SafeImplicits._

  // Returns the kind of tokens.
  override def getKind(token: Token): TokenKind = token match {
    case IdentifierToken(_) => IdentifierKind
    case DotToken => DotKind
    case LambdaToken => LambdaKind
    case ParenthesisToken(o) => ParenthesisKind(o)
    case _ => OtherKind
  }

  // Accept any token of the kind IdentifierKind,
  val name: Syntax[String] = accept(IdentifierKind)({
    // Extract the string from them.
    case IdentifierToken(n) => n
  }, {
    // Generates all tokens which could have led to the string.
    // (In this case, only one.)
    case n => Seq(IdentifierToken(n))
  })

  // Accept any token of the kind LambdaKind, which will always be LambdaToken.
  val lambda: Syntax[Unit] = elem(LambdaKind).unit(LambdaToken)

  // Accept any token of the kind DotKind, which will always be DotToken.
  val dot: Syntax[Unit] = elem(DotKind).unit(DotToken)

  // Accepts an open or a close parenthesis.
  def parens(isOpen: Boolean): Syntax[Unit] =
    elem(ParenthesisKind(isOpen)).unit(ParenthesisToken(isOpen))

  // Open parenthesis.
  val open = parens(true)

  // Close parenthesis.
  val close = parens(false)

  // Turn a name into an expression.
  val variable: Syntax[Expr] = name.map({
    // Turn the string into a variable.
    case n => Var(n)
  }, {
    // Turn the expression into all strings that could have generated it.
    case Var(n) => Seq(n)
    case _ => Seq()
  })

  // The syntax for expressions, which is the main syntax.
  lazy val expr: Syntax[Expr] = recursive {
    // Accepts either a lambda expression or an application.
    // `appExpr` also includes single basic expressions.
    lambdaExpr | appExpr
  }

  // Basic expressions. Simply a variable or an expression in parenthesis.
  lazy val basic: Syntax[Expr] = variable | open.skip ~ expr ~ close.skip

  // Lambda expression.
  lazy val lambdaExpr: Syntax[Expr] = (lambda.skip ~ many1(name) ~ dot.skip ~ expr).map({
    // Given a sequence of names and the expression body, we create the corresponding lambda.
    case ns ~ e => ns.foldRight(e) {  // We do so by using `foldRight`.
      case (n, acc) => Abs(n, acc)  // Create an `Abs` from the name and body.
    }
  }, {
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
  })

  // Application, which consists of a sequence of at least one basic expressions.
  lazy val appExpr: Syntax[Expr] = many1(basic).map({
    // We reduce all expressions into a single one using `reduceLeft`.
    xs => xs.reduceLeft(App(_, _))
  }, {
    // We provide also the inverse operation.
    // We unfold arguments using `unreduceLeft`.
    acc => {
      // We use `unreduceLeft` to unpack the value.
      unreduceLeft[Expr] {
        case App(l, r) => (l, r)  // We split the `App` into its two components.
      }(acc)
    }
  })

  // Create the LL1 parser from the syntax description.
  val parser = Parser(expr)

  // Create the pretty printer from the syntax description.
  val printer = PrettyPrinter(expr)

  // Returns the pretty printed representation of an expression.
  def unapply(value: Expr): Option[String] =
    printer(value).take(1).map(LambdaLexer.unapply(_)).toList.headOption

  // Parses an expression.
  def apply(text: String): Option[Expr] =
    parser(LambdaLexer(text.iterator)).getValue
}

// Main class.
object LambdaCalculus {
  def main(args: Array[String]) {
    println("Parsing and pretty printing expressions...")

    // Original text.
    val str = """\ f. \x. (f (x) x)"""
    println("Original string: " + str)

    // Parsing the expression.
    val e = LambdaSyntax("""\ f. \x. (f (x) x)""").get
    println("Parsed expression tree: " + e)

    // Pretty printing the expression.
    val pretty = LambdaSyntax.unapply(e).get
    println("Pretty printed expression: " + pretty)
  }
}
