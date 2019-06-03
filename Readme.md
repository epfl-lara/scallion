<p align="center">
<img src="images/scallion.png" width="300px" alt="SCALL1ON" />
</p>

# Overview

Scallion is a library for writing lexers and parsers for *LL(1)* languages in Scala.

In Scallion, lexing and parsing are two distinct phases.
During lexing, the input stream of characters is turned into a stream of tokens.
During parsing, the stream of tokens is consumed and turned into a value.

# Tutorial: JSON Parser

In this short tutorial, we show how to write a JSON parser using Scallion.

## Tokens

We first define the tokens that will be produced by the lexer and consumed by the parser.

```scala
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
```

Each token contains a range, which indicates the indices at which the token starts and ends in the input stream.

## Lexing

Lexing is the process of converting a stream of characters into a stream of tokens.
The trait `Lexers` is used to write lexers that perform this task.

```scala
object JSONLexer extends Lexers[Token, Char, Int] {
```

`Lexers` is parameterized by three types: the type of tokens, the type of characters and the type of positions. In our case, tokens are of type `Token`, characters of type `Char` and positions of type `Int`.

Each lexer is built using `Lexer`. `Lexer` accepts any number of rules, each made up of a regular expression and a function to produce a token from the accepted string and range.

```scala
  val lexer = Lexer(
    // Separator
    elem("[]{},:")
      |> { (cs, r) => SeparatorToken(cs.head, r) },

    // Space
    many1(elem(_.isWhitespace))
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
      elem('\\') ~ (elem("\"\\/bfnrt") | elem('u') ~ hex.times(4))
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
      elem("eE") ~
      opt(elem("+-")) ~
      many1(digit)
    }
      |> { (cs, r) => NumberToken(cs.mkString.toDouble, r) }
  )
```

Finally, we define the `apply` method for the JSON lexer, which takes an input an iterator of characters and produces an iterator of tokens.

```scala
  def apply(it: Iterator[Char]): Iterator[Token] = {

    // Creates a source which keeps tracks of positions.
    val source = Source.fromIterator(it, IndexPositioner)

    lexer(
      // The source.
      source,

      // Token to produce in case of errors.
      (content, range) => UnknownToken(content.mkString, range),

      // Tokens to ignore.
      token => token.isInstanceOf[SpaceToken])
  }
}

```

### Example

```scala
scala> val src = scala.io.Source.fromString("""[123.45, "hello!", null]""")
src: scala.io.Source = non-empty iterator

scala> val tks = JSONLexer(src).toList
tks: List[Token] = List(PunctuationToken([,(0,1)), NumberToken(123.45,(1,7)), PunctuationToken(,,(7,8)), StringToken(hello!,(9,17)), PunctuationToken(,,(17,18)), NullToken((19,23)), PunctuationToken(],(23,24)))
```
## Parsing

First, we define token kinds. Each token will have a single corresponding token class.

```scala
sealed abstract class TokenClass
case class SeparatorClass(value: Char) extends TokenClass
case object BooleanClass extends TokenClass
case object NumberClass extends TokenClass
case object StringClass extends TokenClass
case object NullClass extends TokenClass
case object NoClass extends TokenClass
```

We also define JSON values:

```scala
sealed abstract class Value {
  val range: (Int, Int)
}
case class ArrayValue(elems: Seq[Value], range: (Int, Int)) extends Value
case class ObjectValue(elems: Seq[(StringValue, Value)], range: (Int, Int)) extends Value
case class BooleanValue(value: Boolean, range: (Int, Int)) extends Value
case class NumberValue(value: Double, range: (Int, Int)) extends Value
case class StringValue(value: String, range: (Int, Int)) extends Value
case class NullValue(range: (Int, Int)) extends Value
```

We then define the JSON parser. First we define a function that returns the kind of tokens.

```scala
object JSONParser extends Parsers[Token, TokenClass] {

  // Returns the `token`'s kind.
  override def getKind(token: Token): TokenClass = token match {
    case SeparatorToken(value, _) => SeparatorClass(value)
    case BooleanToken(_, _) => BooleanClass
    case NumberToken(_, _) => NumberClass
    case StringToken(_, _) => StringClass
    case NullToken(_) => NullClass
    case _ => NoClass
  }
```

Then, we define parsers for the different JSON values.

```scala
  val booleanValue = accept(BooleanClass) {
    case BooleanToken(value, range) => BooleanValue(value, range)
  }
  val numberValue = accept(NumberClass) {
    case NumberToken(value, range) => NumberValue(value, range)
  }
  val stringValue = accept(StringClass) {
    case StringToken(value, range) => StringValue(value, range)
  }
  val nullValue = accept(NullClass) {
    case NullToken(range) => NullValue(range)
  }
  implicit def separator(char: Char) = accept(SeparatorClass(char)) {
    case SeparatorToken(_, range) => range
  }

  lazy val arrayValue =
    ('[' ~ repsep(value, ',') ~ ']').map {
      case start ~ vs ~ end => ArrayValue(vs, (start._1, end._2))
    }

  lazy val binding =
    (stringValue ~ ':' ~ value).map {
      case key ~ _ ~ value => (key, value)
    }
  lazy val objectValue =
    ('{' ~ repsep(binding, ',') ~ '}').map {
      case start ~ bs ~ end => ObjectValue(bs, (start._1, end._2))
    }

  lazy val value: Parser[Value] = recursive {
    arrayValue | objectValue | booleanValue | numberValue | stringValue | nullValue
  }
```

Finally, we can define the `apply` method for the whole parser.

```scala
  def apply(it: Iterator[Token]): ParseResult[Value] = value(it)
}
```

### Example

```scala
scala> val src = scala.io.Source.fromString("""[123.45, "hello!", null]""")
src: scala.io.Source = non-empty iterator

scala> val res = JSONParser(JSONLexer(src))
res: example.JSONParser.ParseResult[example.Value] = Parsed(ArrayValue(Vector(NumberValue(123.45,(1,7)), StringValue(hello!,(9,17)), NullValue((19,23))),(0,24)),Success(ArrayValue(Vector(NumberValue(123.45,(1,7)), StringValue(hello!,(9,17)), NullValue((19,23))),(0,24))))
```
