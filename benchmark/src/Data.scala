
import json._

/** Prints token counts for the various JSON files. */
object Data {
  def main(args: Array[String]): Unit = {
     val files = Seq(
      "failing-scp", "largest-pwd", "too-large-for-pwd",
      "normal-100k", "normal-200k", "normal-300k", "normal-400k",
      "normal-500k", "normal-600k", "normal-700k", "normal-800k",
      "normal-900k", "normal-1M", "normal-10M")

    for (file <- files) {
      val ts = JSONLexer(io.Source.fromFile("benchmark/resources/" + file + ".json")).toArray
      println(file + ": " + ts.size + " tokens.")
    }
  }
}