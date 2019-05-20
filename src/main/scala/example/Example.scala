package example

import scala.language.implicitConversions

import scallion._

sealed abstract class Token
case class PunctuationToken(value: Char) extends Token
case class BooleanToken(value: Boolean) extends Token
case class NumberToken(value: Double) extends Token
case class StringToken(value: String) extends Token
case object NullToken extends Token
case object SpaceToken extends Token
case class UnknownToken(content: String) extends Token

sealed abstract class TokenClass
case class PunctuationClass(value: Char) extends TokenClass
case object BooleanClass extends TokenClass
case object NumberClass extends TokenClass
case object StringClass extends TokenClass
case object NullClass extends TokenClass
case object NoClass extends TokenClass

sealed abstract class Value
case class ArrayValue(elems: Seq[Value]) extends Value
case class ObjectValue(elems: Seq[(String, Value)]) extends Value
case class BooleanValue(value: Boolean) extends Value
case class NumberValue(value: Double) extends Value
case class StringValue(value: String) extends Value
case object NullValue extends Value

object JSONLexer extends Lexers[Token, Char, Int] {

  val digit = elem(_.isDigit)
  val nonZero = elem("123456789")
  val hex = elem("abcdefABCDEF") | digit

  val lexer = Lexer(
    // Punctuation
    elem("[]{},:")
      |> { cs => PunctuationToken(cs.head) },

    // Space
    many1(elem(_.isWhitespace))
      |> SpaceToken,

    // Booleans
    word("true") 
      |> BooleanToken(true),
    word("false")
      |> BooleanToken(false),

    // Null
    word("null")
      |> NullToken,

    // Strings
    elem('"') ~
    many {
      elem(c => c != '"' && c != '\\' && !c.isControl) |
      elem('\\') ~ (elem("\"\\/bfnrt") | elem('u') ~ hex.times(4))
    } ~
    elem('"')
      |> { cs => StringToken {
        val string = cs.mkString
        string.slice(1, string.length - 1)
      }},

    // Numbers
    opt {
      elem('-')
    } ~ 
    { 
      elem('0') |
      nonZero ~ many(digit)
    } ~
    opt {
      elem('.') ~ many1(digit)
    } ~
    opt {
      elem("eE") ~
      opt(elem("+-")) ~
      many1(digit)
    }
      |> { cs => NumberToken(cs.mkString.toDouble) }
  )

  def apply(it: Iterator[Char]): Iterator[Token] = {
    val source = new IteratorSource(0, it) {
      override def increment(pos: Int, char: Char): Int = pos + 1
    }

    lexer(source, (content, _) => UnknownToken(content.mkString), _ == SpaceToken)
  }
}

object JSONParser extends Parsers[Token, TokenClass] {

  override def getKind(token: Token): TokenClass = token match {
    case PunctuationToken(value) => PunctuationClass(value)
    case BooleanToken(_) => BooleanClass
    case NumberToken(_) => NumberClass
    case StringToken(_) => StringClass
    case NullToken => NullClass
    case _ => NoClass
  }

  val booleanValue = accept(BooleanClass) {
    case BooleanToken(value) => BooleanValue(value)
  }
  val numberValue = accept(NumberClass) {
    case NumberToken(value) => NumberValue(value)
  }
  val stringValue = accept(StringClass) {
    case StringToken(value) => StringValue(value)
  }
  val nullValue = accept(NullClass) {
    case NullToken => NullValue
  }
  val rawString = accept(StringClass) {
    case StringToken(value) => value
  }
  implicit def punctuation(char: Char) =
    elem(PunctuationClass(char))

  lazy val arrayValue: Parser[Value] =
    ('[' ~ repsep(value, ',') ~ ']').map {
      case _ ~ vs ~ _ => ArrayValue(vs)
    }

  lazy val binding: Parser[(String, Value)] =
    (rawString ~ ':' ~ value) map {
      case k ~ _ ~ v => (k, v)
    }
  lazy val objectValue: Parser[Value] =
    '{' ~>~ objectContent.map(ObjectValue(_)) ~<~ '}'
  lazy val objectContent: Parser[Seq[(String, Value)]] =
    (binding +: objectRest) | epsilon(Seq())
  lazy val objectRest: Parser[Seq[(String, Value)]] = recursive {
    ',' ~>~ binding +: objectRest | epsilon(Seq())
  }

  lazy val value: Parser[Value] = recursive {
    arrayValue | objectValue | booleanValue | numberValue | stringValue | nullValue
  }

  def apply(it: Iterator[Token]): ParseResult[Value] = value.parse(it)
}
