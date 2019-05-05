package scallion

object ExParsers extends Parsers with CharSources {

  type ErrorMessage = String

  sealed abstract class Token(val repr: Seq[Character]) extends HasRepr

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
  case object ErrorToken extends Token("<Error>")
  case object EndToken extends Token("")

  val tokenizer = Tokenizer(
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
    elem('"') ~ many(elem(c => c != '"' && c != '\\') | elem('\\') ~ elem("\\\"")) ~ elem('"')
      |> { cs => StringLit(cs.tail.init.mkString("")) },

    // Identifiers
    elem(_.isLetter) ~ many(elem(_.isLetterOrDigit) | elem('_'))
      |> { cs => Identifier(cs.mkString("")) },

    // Number literal
    many1(elem(_.isDigit)) ~ opt(elem('.') ~ many(elem(_.isDigit)))
      |> { cs => NumberLit(cs.mkString("")) },

    // Space
    many1(elem(_.isWhitespace)) |> Space
  )

  sealed abstract class Expr
  case class IntegerLiteral(value: BigInt) extends Expr
  case class BooleanLiteral(value: Boolean) extends Expr
  case class IfExpr(condition: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr
  case class Plus(lhs: Expr, rhs: Expr) extends Expr
  case class Minus(lhs: Expr, rhs: Expr) extends Expr
  case class UPlus(expr: Expr) extends Expr
  case class UMinus(expr: Expr) extends Expr

  lazy val space: Parser[Unit] = accepts(_ => "Expected a space.") {
    case Space => ()
  }

  lazy val arithBinOp: Parser[(Expr, Expr) => Expr] =
    accepts(_ => "Expected an operator", "+", "-") {
      case Operator("+") => Plus(_, _)
      case Operator("-") => Minus(_, _)
    }

  lazy val arithUnOp: Parser[Expr => Expr] =
    accepts(_ => "Expected an operator", "+", "-") {
      case Operator("+") => UPlus(_)
      case Operator("-") => UMinus(_)
    }

  lazy val ifParser: Parser[Expr] = {
    elem(If, _ => "Expected an if expression.") >>
    spaced(elem(Punctuation('('), _ => "Expected an open parenthesis.")) >>
    exprParser &&
    spaced(elem(Punctuation(')'), _ => "Expected a close parenthesis.")) >>
    exprParser &&
    spaced(elem(Else, _ => "Expected an else branch.")) >>
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

  def spaced[A](parser: Parser[A]): Parser[A] =
    opt(space) >> parser << opt(space)

  lazy val nonOpExprParser = (ifParser | literalParser).explain("Expected an expression.")

  lazy val exprParser: Parser[Expr] =
    infixesLeft(prefixes(arithUnOp, nonOpExprParser) << opt(space), arithBinOp << opt(space))

  lazy val parser: Parser[Expr] = phrase(spaced(exprParser), _ => "Expected end of input.")
}