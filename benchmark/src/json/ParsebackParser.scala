package json

import parseback._
import parseback.compat.cats._

class ParsebackParser {
  import parseback.LineStream

  import cats.Eval

  lazy val number: Parser[Value] = regex("""\d+""".r) ^^ { (_, n) => NumberValue(n.toDouble, (0, 0)) }

  lazy val string: Parser[StringValue] = regex(""""[^"]*"""".r) ^^ { (_, cs) => StringValue(cs.substring(1, cs.length - 1), (0, 0)) }

  lazy val nul: Parser[Value] = "null" ^^^ NullValue((0, 0))

  lazy val bool: Parser[Value] = "true" ^^^ BooleanValue(true, (0, 0)) | "false" ^^^ BooleanValue(false, (0, 0))

  lazy val binding: Parser[(StringValue, Value)] = (string <~ space <~ ":" <~ space) ~ value

  lazy val array: Parser[Value] = "[" ~> space ~> values <~ space <~ "]" ^^ { (_, vs) => ArrayValue(vs, (0, 0)) }

  lazy val values: Parser[Seq[Value]] = (value ~ (space ~> "," ~> space ~> values | () ^^^ List.empty[Value])) ^^ { (_, v, vs) => v +: vs } | () ^^^ List.empty[Value]

  lazy val space: Parser[String] = regex("""\s+""".r) | ""

  lazy val value: Parser[Value] = number | array | nul | bool | string | objekt

  lazy val bindings: Parser[Seq[(StringValue, Value)]] = (binding ~ (space ~> "," ~> space ~> bindings | () ^^^ List.empty[(StringValue, Value)])) ^^ { (_, k, v, vs) => (k, v) +: vs } | () ^^^ List.empty[(StringValue, Value)]

  lazy val objekt: Parser[Value] = "{" ~> space ~> bindings <~ space <~ "]" ^^ { (_, vs) => ObjectValue(vs, (0, 0)) }

  def apply(lines: Seq[String]) = {
    val input: LineStream[Eval] = LineStream(lines)
    value(input).value
  }
}
