
import json._

object Crashes {

  def main(args: Array[String]) {

    println("Making Scala Parser Combinators crash.")
    try {
      val tokens = JSONLexer(io.Source.fromFile("benchmark/resources/failing-scp.json"))
      val parser = new ScalaParser
      parser(tokens)
      println("No crash.")
    }
    catch {
      case e: Throwable =>
        println("Crashed...")
        println(e)
    }

    println("Making Parseback crash.")
    try {
      val parser = new ParsebackParser
      parser(scala.io.Source.fromFile("benchmark/resources/too-large-for-pwd.json").getLines().toSeq)
      println("No crash.")
    }
    catch {
      case e: Throwable =>
        println("Crashed...")
        println(e)
    }

    println("Trying to show that Parseback doesn't simply always crash.")
    try {
      val parser = new ParsebackParser
      parser(scala.io.Source.fromFile("benchmark/resources/largest-pwd.json").getLines().toSeq)
      println("No crash, as desired.")
    }
    catch {
      case e: Throwable =>
        println("Unexpectedly crashed.")
        println(e)
    }

    println("Making simple (non-zippy) LL(1) parsing with derivatives crash. (Warning: May take several minutes.)")
    try {
      val tokens = JSONLexer(io.Source.fromFile("benchmark/resources/normal-10M.json"))
      val parser = new ScallionParser
      parser.simpleApply(tokens)
      println("No crash.")
    }
    catch {
      case e: Throwable =>
        println("Crashed...")
        println(e)
    }
  }
}