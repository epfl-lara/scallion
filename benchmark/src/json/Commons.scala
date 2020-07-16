package json

import scala.annotation._
import scala.collection.mutable.ArrayBuffer

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

class JSONLexer(source: Iterator[Char]) {

  var in = source
  val out = new ArrayBuffer[Token]()

  var start = 0
  var end = 0

  @tailrec
  private def next(): Char = {
    var char = in.next(); end += 1
    if (char.isWhitespace) {
      start += 1
      next()
    }
    else {
      char
    }
  }

  @inline
  private def range = (start, end)

  @inline
  private def isHex(char: Char): Boolean =
    char.isDigit || (char >= 'A' && char <= 'F') || (char >= 'a' && char <= 'f')

  private def goNumber(char: Char): Unit = {
    val content = new StringBuilder()
    var c = char

    try {
      if (c == '-') {
        content += c; c = in.next(); end += 1
      }

      if (c == '0') {
        content += c; c = in.next(); end += 1
      }
      else {

        if (c < '0' || c > '9') { out.+=(UnknownToken(content.toString, range)); return }

        while (c >= '0' && c <= '9') {
          content += c; c = in.next(); end += 1
        }
      }

      if (c == '.') {
        content += c; c = in.next(); end += 1

        if (c < '0' || c > '9') { out.+=(UnknownToken(content.toString, range)); return }

        while (c >= '0' && c <= '9') {
          content += c; c = in.next(); end += 1
        }
      }

      if (c == 'E' || c == 'e') {
        content += c; c = in.next(); end += 1

        if (c == '+' || c == '-') {
          content += c; c = in.next(); end += 1
        }

        if (c < '0' || c > '9') { out.+=(UnknownToken(content.toString, range)); return }

        while (c >= '0' && c <= '9') {
          content += c; c = in.next(); end += 1
        }
      }

      end -= 1
      out.+=(NumberToken(content.toString.toDouble, range))
      end += 1
    }
    catch {
      case e: java.util.NoSuchElementException => {
        try {
          out.+=(NumberToken(content.toString.toDouble, range))
          start = end
        }
        catch {
          case _: Exception => ()
        }
        throw e
      }
    }

    start = end - 1

    while (c.isWhitespace) {
      c = in.next(); start = end; end += 1;
    }

    go(c)
  }

  private def go(char: Char): Unit = {
    var c = char
    char match {
      case '[' | ']' | '{' | '}' | ',' | ':' => out.+=(SeparatorToken(char, range))
      case 't' => {
        c = in.next(); end += 1
        if (c != 'r') { out.+=(UnknownToken("t" + c, range)); return }
        c = in.next(); end += 1
        if (c != 'u') { out.+=(UnknownToken("tr" + c, range)); return }
        c = in.next(); end += 1
        if (c != 'e') { out.+=(UnknownToken("tru" + c, range)); return }
        out.+=(BooleanToken(true, range))
      }
      case 'f' => {
        c = in.next(); end += 1
        if (c != 'a') { out.+=(UnknownToken("f" + c, range)); return }
        c = in.next(); end += 1
        if (c != 'l') { out.+=(UnknownToken("fa" + c, range)); return }
        c = in.next(); end += 1
        if (c != 's') { out.+=(UnknownToken("fal" + c, range)); return }
        c = in.next(); end += 1
        if (c != 'e') { out.+=(UnknownToken("fals" + c, range)); return }
        out.+=(BooleanToken(false, range))
      }
      case 'n' => {
        c = in.next(); end += 1
        if (c != 'u') { out.+=(UnknownToken("n" + c, range)); return }
        c = in.next(); end += 1
        if (c != 'l') { out.+=(UnknownToken("nu" + c, range)); return }
        c = in.next(); end += 1
        if (c != 'l') { out.+=(UnknownToken("nul" + c, range)); return }
        out.+=(NullToken(range))
      }
      case '"' => {
        val content = new StringBuilder()
        while (true) {
          c = in.next(); end += 1
          if (c == '"') { out.+=(StringToken(content.toString, range)); return }

          content += c

          if (c == '\\') {
            c = in.next(); end += 1; content += c
            c match {
              case '\\' | '"' | '/' | 'b' | 'f' | 'n' | 'r' | 't' => ()
              case 'u' => {
                c = in.next(); end += 1; content += c
                if (!isHex(c)) { out.+=(UnknownToken("\"" + content.toString, range)); return }
                c = in.next(); end += 1; content += c
                if (!isHex(c)) { out.+=(UnknownToken("\"" + content.toString, range)); return }
                c = in.next(); end += 1; content += c
                if (!isHex(c)) { out.+=(UnknownToken("\"" + content.toString, range)); return }
                c = in.next(); end += 1; content += c
                if (!isHex(c)) { out.+=(UnknownToken("\"" + content.toString, range)); return }
              }
              case _ => out.+=(UnknownToken("\"" + content.toString, range))
            }
          }
          else if (c.isControl) {
            out.+=(UnknownToken("\"" + content.toString, range)); return
          }
        }
      }
      case _ => goNumber(c)
    }
  }

  def toIterator: Iterator[Token] = {

    try {
      while(true) {
        start = end
        go(next())
      }
    }
    catch {
      case e: java.util.NoSuchElementException => {
        if (start != end) {
          out.+=(UnknownToken("", (start, end)))
        }
        // out.end()
      }
    }

    out.toIterator
  }
}

object JSONLexer {
  def apply(it: Iterator[Char]): Iterator[Token] =
    new JSONLexer(it).toIterator
}