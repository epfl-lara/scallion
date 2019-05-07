package scallion

object ExDefs extends predefs.StringPredefs {
  sealed abstract class Token(val repr: String) extends HasRepr[String]
  case object If extends Token("if")
  case object Else extends Token("else")
  case object While extends Token("while")
  case object Space extends Token(" ")
  case object TrueLit extends Token("true")
  case object FalseLit extends Token("false")
  case class Punctuation(char: Char) extends Token("" + char)
  case class Identifier(name: String) extends Token(name)
  case class NumberLit(value: String) extends Token(value)
  case class StringLit(value: String) extends Token("\"" + value + "\"")
  case class Operator(value: String) extends Token(value)
  case object Error extends Token("<Error>")
  case object End extends Token("")
  case class Comment(text: String) extends Token("<Comment>")

  sealed abstract class Expr
  case class IntegerLiteral(value: BigInt) extends Expr
  case class BooleanLiteral(value: Boolean) extends Expr
  case class IfExpr(condition: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr
  case class Plus(lhs: Expr, rhs: Expr) extends Expr
  case class Minus(lhs: Expr, rhs: Expr) extends Expr
  case class UPlus(expr: Expr) extends Expr
  case class UMinus(expr: Expr) extends Expr
  case class Times(lhs: Expr, rhs: Expr) extends Expr
  case class Div(lhs: Expr, rhs: Expr) extends Expr
}

import ExDefs._

object ExLexer extends Lexers[Token, Char, Position] {
  val ErrorToken = Error
  val EndToken = End

  val lexer = Lexer(
    // Keywords
    word("if")    |> If,
    word("else")  |> Else,
    word("while") |> While,
    word("true")  |> TrueLit,
    word("false") |> FalseLit,

    // Operators
    elem("+-*/<>") | word(">=") | word("<=") | word("==")
      |> { cs => Operator(cs.mkString("")) },

    // Punctuation
    elem(".,(){}!;?")
      |> { cs => Punctuation(cs.head) },

    // String literal
    elem('"') ~
    many(elem(c => c != '"' && c != '\\') | elem('\\') ~ elem("\\\"")) ~
    elem('"')
      |> { cs => StringLit(cs.tail.init.mkString("")) },

    // Identifiers
    elem(_.isLetter) ~ many(elem(_.isLetterOrDigit) | elem('_'))
      |> { cs => Identifier(cs.mkString("")) },

    // Number literal
    many1(elem(_.isDigit)) ~ opt(elem('.') ~ many(elem(_.isDigit)))
      |> { cs => NumberLit(cs.mkString("")) },

    // Single line comment.
    word("//") ~ many(elem(_ != '\n'))
      |> { cs => Comment(cs.mkString("")) },

    // Multiline comments.
    word("/*") ~
    many(elem(_ != '*') | elem('*') ~ elem(_ != '/')) ~
    opt(elem('*')) ~
    word("*/")
      |> { cs => Comment(cs.mkString("")) },

    // Space
    many1(elem(_.isWhitespace)) |> Space
  )

  def run(text: String): Iterator[(Token, (Position, Position))] = {
    val source = new StringSource(text)
    lexer(source, skipToken={
      case Space => true
      case Comment(_) => true
      case _ => false
    })
  }
}

object ExParser extends Parsers[Token, Position, String, String] {
  val ErrorToken = Error
  val EndToken = End

  lazy val arithUnOp: Parser[Expr => Expr] =
    accepts(_ => "Expected an operator", "+", "-") {
      case Operator("+") => UPlus(_)
      case Operator("-") => UMinus(_)
    }

  lazy val ifParser: Parser[Expr] = {
    elem(If, _ => "Expected an if expression.") >>
    inParens(exprParser) &&
    exprParser &&
    elem(Else, _ => "Expected an else branch.") >>
    exprParser
  } map {
    case c && t && e => IfExpr(c, t , e)
  }

  lazy val literalParser: Parser[Expr] =
    integerLiteralParser | booleanLiteralParser

  lazy val integerLiteralParser: Parser[Expr] =
    accepts(_ => "Expected a number literal", "0", "1") {
      case NumberLit(n) => IntegerLiteral(BigInt(n))
    }

  lazy val booleanLiteralParser: Parser[Expr] =
    accepts(_ => "Expected a boolean literal", "true", "false") {
      case TrueLit => BooleanLiteral(true)
      case FalseLit => BooleanLiteral(false)
    }

  def inParens[A](parser: Parser[A]): Parser[A] =
    elem(Punctuation('('), _ => "Expected an open parenthesis") >>
    parser <<
    elem(Punctuation(')'), _ => "Expected a close parenthesis")

  def binOp(repr: String, op: (Expr, Expr) => Expr):
      Parser[(Expr, Expr) => Expr] =
    elem(Operator(repr), _ => "Expected operator " + repr).map(_ => op)

  lazy val nonOpExprParser =
    oneOf(_ => "Expected an expression")(
      ifParser,
      literalParser,
      inParens(exprParser))

  lazy val exprParser: Parser[Expr] = rec {
    operators(prefixes(arithUnOp, nonOpExprParser))(
      binOp("+", Plus(_, _))  | binOp("-", Minus(_, _)) |> Associativity.Left,
      binOp("*", Times(_, _)) | binOp("/", Div(_, _))   |> Associativity.Left)
  }

  val parser: Parser[Expr] = phrase(exprParser, _ => "Expected end of input")

  def run(tokens: Iterator[(Token, (Position, Position))]): ParseResult[Expr] = {
    val input = new Input(tokens)
    parser.parse(input)
  }
}

object Example {
  def run(text: String): Expr = {
    import ExParser._

    ExParser.run(ExLexer.run(text)) match {
      case Complete(expr)   => expr
      case Incomplete(rest) => throw new Exception(
        "Incomplete input. Can be completed by, for instance, " + rest.reprs.mkString(", ") + ".")
      case Failed((start, end), error) => {
        val line = text.drop(start.index - start.column).takeWhile(_ != '\n')
        val pos = if (start.row == end.row) line else line + "..."
        val count =
          if (start.row == end.row) Math.max(1, end.column - start.column)
          else pos.size - start.column
        val markers = " " * start.column + "^" * count
        throw new Exception(
          "Error: " + error + ". Line " + (start.row + 1) + ":\n" + pos + "\n" + markers)
      }
    }
  }
}