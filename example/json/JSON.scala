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

package example.json

import scala.language.implicitConversions

import scallion.input._
import scallion.lexical._
import scallion.syntactic._

// First, we define the token for our language.
sealed abstract class Token {
  // We store the indices at which the tokens
  // starts (inclusive) and ends (exclusive).
  val range: (Int, Int)
}
case class SeparatorToken(value: Char, range: (Int, Int)) extends Token
case class BooleanToken(value: Boolean, range: (Int, Int)) extends Token
case class NumberToken(value: Double, range: (Int, Int)) extends Token
case class StringToken(value: String, range: (Int, Int)) extends Token
case class NullToken(range: (Int, Int)) extends Token
case class SpaceToken(range: (Int, Int)) extends Token
case class UnknownToken(content: String, range: (Int, Int)) extends Token

// Then, we define the lexer.
// The lexer converts sequences of characters into tokens.
object JSONLexer extends Lexers[Token, Char, Int] with CharRegExps {

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
    // When no regular expression match, we return this token.
    (cs, r) => UnknownToken(cs.mkString, r)
  }

  // The apply method converts an iterator over characters
  // to an iterator over tokens.
  def apply(it: Iterator[Char]): Iterator[Token] = {

    // We create a `Source` from the iterator,
    // which keeps track of positions.
    val source = Source.fromIterator(it, IndexPositioner)

    // We apply the lexer.
    // The spawn method creates a new thread for the
    // lexer so that in can run in parallel with the
    // parser.
    val tokens = lexer.spawn(source)

    // We filter out all spaces.
    tokens.filter(!_.isInstanceOf[SpaceToken])
  }
}


// Then, we define classes of tokens.
// Each class corresponds to a group of tokens, and
// abstracts away all irrelevant details.
// In the parser, tokens can be accepted or rejected only based
// on their token class.
sealed abstract class TokenClass(repr: String) {
  override def toString = repr
}
case class SeparatorClass(value: Char) extends TokenClass(value.toString)
case object BooleanClass extends TokenClass("<boolean>")
case object NumberClass extends TokenClass("<number>")
case object StringClass extends TokenClass("<string>")
case object NullClass extends TokenClass("<null>")
case object NoClass extends TokenClass("<error>")

// Then, we define JSON values, which will be the output of the parser.
sealed abstract class Value {
  val range: (Int, Int)
}
case class ArrayValue(elems: Seq[Value], range: (Int, Int)) extends Value
case class ObjectValue(elems: Seq[(StringValue, Value)], range: (Int, Int)) extends Value
case class BooleanValue(value: Boolean, range: (Int, Int)) extends Value
case class NumberValue(value: Double, range: (Int, Int)) extends Value
case class StringValue(value: String, range: (Int, Int)) extends Value
case class NullValue(range: (Int, Int)) extends Value

// Then, we define the JSON Parser.
object JSONParser extends Syntaxes[Token, TokenClass] {

  // We assign to each token a single token class.
  override def getKind(token: Token): TokenClass = token match {
    case SeparatorToken(value, _) => SeparatorClass(value)
    case BooleanToken(_, _) => BooleanClass
    case NumberToken(_, _) => NumberClass
    case StringToken(_, _) => StringClass
    case NullToken(_) => NullClass
    case _ => NoClass
  }

  // Syntax for booleans.
  // We accept tokens of the class `BooleanClass`,
  // and turn them into proper `Value`s.
  val booleanValue: Syntax[Value] = accept(BooleanClass) {
    case BooleanToken(value, range) => BooleanValue(value, range)
  }

  // Syntax for numbers.
  val numberValue: Syntax[Value] = accept(NumberClass) {
    case NumberToken(value, range) => NumberValue(value, range)
  }

  // Syntax for strings.
  val stringValue: Syntax[StringValue] = accept(StringClass) {
    case StringToken(value, range) => StringValue(value, range)
  }

  // Syntax for null.
  val nullValue: Syntax[Value] = accept(NullClass) {
    case NullToken(range) => NullValue(range)
  }

  // Implicit conversion from a single char to
  // syntax for a separator token.
  implicit def separator(char: Char): Syntax[Token] = elem(SeparatorClass(char))

  // Defines the syntax for arrays.
  lazy val arrayValue: Syntax[Value] =
    ('[' ~ repsep(value, ','.unit()) ~ ']').map {
      case start ~ vs ~ end => ArrayValue(vs, (start.range._1, end.range._2))
    }

  // Defines the syntax for key-value bindings.
  lazy val binding: Syntax[(StringValue, Value)] =
    (stringValue ~ ':' ~ value).map {
      case key ~ _ ~ value => (key, value)
    }

  // Defines the syntax for objects.
  lazy val objectValue: Syntax[Value] =
    ('{' ~ repsep(binding, ','.unit()) ~ '}').map {
      case start ~ bs ~ end => ObjectValue(bs, (start.range._1, end.range._2))
    }

  // Defines the complete syntax for JSON.
  // The `recursive` combinator is used since the syntax is recursive.
  lazy val value: Syntax[Value] = recursive {
    // We define the various cases.
    oneOf(
      arrayValue,
      objectValue,
      booleanValue,
      numberValue,
      stringValue.up[Value],  // We upcast the produced value from `StringValue` to `Value`.
      nullValue)
  }

  // We check that the parser is LL(1).
  // This would usually go in your test suite.
  // It ensures that the syntax is not ambiguous.
  assert(value.isLL1)

  // Turn the iterator of tokens into a value, if possible.
  def apply(it: Iterator[Token]): Option[Value] = value(it) match {
    case Parsed(value, syntax) => Some(value)  // The parse was successful.
    case UnexpectedToken(token, syntax) => None  // Encountered an unexpected `token`.
    case UnexpectedEnd(syntax) => None  // Encountered an unexpected end of input.
    // In each case, syntax contains a `Syntax[Value]` which can
    // be used to resume parsing at that point.
  }
}
