package json

import scala.util.parsing.combinator._
import scala.util.parsing.input._

class TokenReader(tokens: Iterator[Token]) extends Reader[Token] {
  lazy val value = tokens.next()
  override def first: Token = value
  override val atEnd: Boolean = !tokens.hasNext
  override def pos: Position = NoPosition
  override def rest: Reader[Token] = {
    first
    new TokenReader(tokens)
  }
}

class ScalaParser extends Parsers {
  type Elem = Token

  def stringValue: Parser[StringValue] = {
    accept("string", { case StringToken(value, _) => StringValue(value, (0, 0)) })
  }

  def nullValue: Parser[Value] = {
    accept("null", { case NullToken(_) => NullValue((0, 0)) })
  }

  def boolValue: Parser[Value] = {
    accept("boolean", { case BooleanToken(value, _) => BooleanValue(value, (0, 0)) })
  }

  def numberValue: Parser[Value] = {
    accept("number", { case NumberToken(value, _) => NumberValue(value, (0, 0)) })
  }

  def separator(char: Char): Parser[Token] = {
    accept("separator " + char, { case tk@SeparatorToken(`char`, _) => tk })
  }

  def binding: Parser[(StringValue, Value)] = {
    (stringValue ~ separator(':') ~ value) ^^ {
      case key ~ _ ~ value => (key, value)
    }
  }

  def arrayValue: Parser[Value] = {
    separator('[') ~ repsep(value, separator(',')) ~ separator(']') ^^ {
      case _ ~ values ~ _ => ArrayValue(values, (0, 0))
    }
  }

  def objectValue: Parser[Value] = {
    separator('{') ~ repsep(binding, separator(',')) ~ separator('}') ^^ {
      case _ ~ values ~ _ => ObjectValue(values, (0, 0))
    }
  }

  def value: Parser[Value] = {
    stringValue | nullValue | boolValue | numberValue | arrayValue | objectValue
  }

  def apply(tokens: Iterator[Token]): Option[Value] = {
    val input = new TokenReader(tokens)
    val res = value(input)
    if (res.successful) {
      Some(res.get)
    }
    else {
      None
    }
  }
}
