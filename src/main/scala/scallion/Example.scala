package scallion

object ExParsers extends Parsers with CharSources {

  type Error = String

  sealed abstract class Token(val representation: Seq[Character])

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

  override def represent(token: Token): Seq[Character] = token.representation

  val tokenizer = Tokenizer(
    Producer(word("if"), cs => If),
    Producer(word("else"), cs => Else),
    Producer(word("while"), cs => While),
    Producer(word("true"), cs => TrueLit),
    Producer(word("false"), cs => FalseLit),
    Producer(many1(elem(_.isWhitespace)), cs => Space),
    Producer(elem("+-*/<>") | word(">=") | word("<=") | word("=="), cs => Operator(cs.mkString(""))),
    Producer(elem(".,(){}!;?"), cs => Punctuation(cs.head)),
    Producer(elem('"') ~ many(elem(c => c != '"' && c != '\\') | elem('\\') ~ elem("\\\"")) ~ elem('"'),
      cs => StringLit(cs.tail.init.mkString(""))),
    Producer(elem(_.isLetter) ~ many(elem(_.isLetterOrDigit) | elem('_')), cs => Identifier(cs.mkString(""))),
    Producer(many1(elem(_.isDigit)) ~ optional(elem('.') ~ many(elem(_.isDigit))), cs => NumberLit(cs.mkString("")))
  )

  sealed abstract class Expr
  case class IntegerLiteral(value: BigInt) extends Expr
  case class BooleanLiteral(value: Boolean) extends Expr
  case class IfExpr(condition: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr

  lazy val space: Parser[Unit] = accepts("Expected a space.", " ") {
    case Space => ()
  }

  lazy val ifParser: Parser[Expr] = {
    elem(If, "Expected an if expression.") >>
    spaced(elem(Punctuation('('), "Expected an open parenthesis.")) >>
    exprParser &&
    spaced(elem(Punctuation(')'), "Expected a close parenthesis.")) >>
    exprParser &&
    spaced(elem(Else, "Expected an else branch.")) >>
    exprParser
  } map {
    case c && t && e => IfExpr(c, t , e)
  }

  lazy val literalParser: Parser[Expr] =
    integerLiteralParser | booleanLiteralParser

  lazy val integerLiteralParser: Parser[Expr] =
    accepts("Expected a number literal", "0", "1") {
      case NumberLit(n) => IntegerLiteral(BigInt(n))
    }

  lazy val booleanLiteralParser: Parser[Expr] =
    accepts("Expected a boolean literal", "true", "false") {
      case TrueLit => BooleanLiteral(true)
      case FalseLit => BooleanLiteral(false)
    }

  def spaced[A](parser: Parser[A]): Parser[A] =
    opt(space) >> parser << opt(space)

  lazy val exprParser: Parser[Expr] =
    (ifParser | literalParser).explain("Expected an expression.")

  lazy val parser: Parser[Expr] = phrase(spaced(exprParser), "Expected end of input.")
}