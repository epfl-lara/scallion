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

package scallion.json

import scala.language.implicitConversions

import scallion._
import silex._

sealed abstract class Token {
  val range: (Int, Int)
}
case class SeparatorToken(value: Char, range: (Int, Int)) extends Token
case class BooleanToken(value: Boolean, range: (Int, Int)) extends Token
case class NumberToken(value: Double, range: (Int, Int)) extends Token
case class StringToken(value: String, range: (Int, Int)) extends Token
case class NullToken(range: (Int, Int)) extends Token
case class SpaceToken(range: (Int, Int)) extends Token
case class UnknownToken(content: String, range: (Int, Int)) extends Token

sealed abstract class TokenClass(repr: String) {
  override def toString = repr
}
case class SeparatorClass(value: Char) extends TokenClass(value.toString)
case object BooleanClass extends TokenClass("<boolean>")
case object NumberClass extends TokenClass("<number>")
case object StringClass extends TokenClass("<string>")
case object NullClass extends TokenClass("<null>")
case object NoClass extends TokenClass("<error>")

sealed abstract class Value {
  val range: (Int, Int)
}
case class ArrayValue(elems: Seq[Value], range: (Int, Int)) extends Value
case class ObjectValue(elems: Seq[(StringValue, Value)], range: (Int, Int)) extends Value
case class BooleanValue(value: Boolean, range: (Int, Int)) extends Value
case class NumberValue(value: Double, range: (Int, Int)) extends Value
case class StringValue(value: String, range: (Int, Int)) extends Value
case class NullValue(range: (Int, Int)) extends Value

object JSONLexer extends Lexers with CharLexers {

  type Token = scallion.json.Token
  type Position = Int

  val lexer = Lexer(
    // Separator
    oneOf("[]{},:")
      |> { (cs, r) => SeparatorToken(cs.head, r) },

    // Space
    many1(whiteSpace)
      |> { (_, r) => SpaceToken(r) },

    // Booleans
    word("true")
      |> { (_, r) => BooleanToken(true, r) },
    word("false")
      |> { (_, r) => BooleanToken(false, r) },

    // Null
    word("null")
      |> { (_, r) => NullToken(r) },

    // Strings
    elem('"') ~
    many {
      elem(c => c != '"' && c != '\\' && !c.isControl) |
      elem('\\') ~ (oneOf("\"\\/bfnrt") | elem('u') ~ hex.times(4))
    } ~
    elem('"')
      |> { (cs, r) => {
        val string = cs.mkString
        StringToken(string.slice(1, string.length - 1), r)
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
      oneOf("eE") ~
      opt(oneOf("+-")) ~
      many1(digit)
    }
      |> { (cs, r) => NumberToken(cs.mkString.toDouble, r) }
  ) onError {
    (cs, r) => UnknownToken(cs.mkString, r)
  }

  def apply(it: Iterator[Char]): Iterator[Token] = {
    val source = Source.fromIterator(it, IndexPositioner)

    val tokens = lexer.spawn(source)

    tokens.filter(!_.isInstanceOf[SpaceToken])
  }
}

object JSONParser extends Parsers {

  type Token = scallion.json.Token
  type Kind = TokenClass

  import Implicits._

  override def getKind(token: Token): TokenClass = token match {
    case SeparatorToken(value, _) => SeparatorClass(value)
    case BooleanToken(_, _) => BooleanClass
    case NumberToken(_, _) => NumberClass
    case StringToken(_, _) => StringClass
    case NullToken(_) => NullClass
    case _ => NoClass
  }

  val booleanValue: Syntax[Value] = accept(BooleanClass) {
    case BooleanToken(value, range) => BooleanValue(value, range)
  }

  val numberValue: Syntax[Value] = accept(NumberClass) {
    case NumberToken(value, range) => NumberValue(value, range)
  }

  val stringValue: Syntax[StringValue] = accept(StringClass) {
    case StringToken(value, range) => StringValue(value, range)
  }

  val nullValue: Syntax[Value] = accept(NullClass) {
    case NullToken(range) => NullValue(range)
  }

  implicit def separator(char: Char): Syntax[Token] = elem(SeparatorClass(char))

  lazy val arrayValue: Syntax[Value] =
    ('[' ~ repsep(value, ',') ~ ']').map {
      case start ~ vs ~ end => ArrayValue(vs, (start.range._1, end.range._2))
    }

  lazy val binding: Syntax[(StringValue, Value)] =
    (stringValue ~ ':' ~ value).map {
      case key ~ _ ~ value => (key, value)
    }

  lazy val objectValue: Syntax[Value] =
    ('{' ~ repsep(binding, ',') ~ '}').map {
      case start ~ bs ~ end => ObjectValue(bs, (start.range._1, end.range._2))
    }

  lazy val value: Syntax[Value] = recursive {
    oneOf(arrayValue, objectValue, booleanValue, numberValue, stringValue.up[Value], nullValue)
  }

  val parser = Parser(value)

  def apply(it: Iterator[Token]): ParseResult[Value] = parser(it)
}
