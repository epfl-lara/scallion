import org.scalameter.api._
import org.scalameter.CurveData
import org.scalameter.picklers.Implicits._
import org.scalameter.utils.Tree

import json._

abstract class BenchmarkFiles extends Bench.OfflineReport {

  // Uncomment files from here.
  val files = Gen.enumeration("file")(
    "normal-100k",
    //"normal-200k",
    //"normal-300k",
    //"normal-400k",
    //"normal-500k",
    //"normal-600k",
    //"normal-700k",
    //"normal-800k",
    //"normal-900k",
    "normal-1M",
    "normal-10M",
  )
}

abstract class BenchmarkTokens extends BenchmarkFiles {
  val tokens = for {
    file <- files
  } yield JSONLexer(io.Source.fromFile("benchmark/resources/" + file + ".json")).toArray
}

class SimpleLL1PWD extends BenchmarkTokens {
  performance of "Simple LL(1) Parsing with Derivatives" in {
    measure method "parse" in {
      using(tokens) in { ts =>

        // Note that we filter out files with more than 100'000 tokens,
        // as the parser takes very long and most likely will fail
        // due to a stack overflow.
        // The stack overflow is check in Crashes.
        if (ts.size <= 100000) {
          val parser = new ScallionParser
          assert(parser.simpleApply(ts.toIterator).nonEmpty)
        }
      }
    }
  }
}

class ZippyLL1PWD extends BenchmarkTokens {
  performance of "Zippy LL(1) Parsing with Derivatives" in {
    measure method "parse" in {
      using(tokens) in { ts =>
        val parser = new ScallionParser
        assert(parser.apply(ts.toIterator).nonEmpty)
      }
    }
  }
}

class RecursiveDescent extends BenchmarkTokens {
  performance of "Recursive Descent (Scala Parser Combinators)" in {
    measure method "parse" in {
      using(tokens) in { ts =>
        val parser = new ScalaParser
        assert(parser(ts.toIterator).nonEmpty)
      }
    }
  }
}

class ZippyGenPWD extends BenchmarkTokens {
  performance of "Zippy (Generalized) Parsing with Derivatives" in {
    measure method "parse" in {
      using(tokens) in { ts =>
        val parser = new ScallionParser
        assert(parser.genApply(ts.toIterator).nonEmpty)
      }
    }
  }
}

class EndToEndBenchmarks extends BenchmarkFiles {

  performance of "JSON end-to-end parsing" in {

    measure method "Scallion parse" in {
      using(files) in { file =>
        val tokens = JSONLexer(io.Source.fromFile("benchmark/resources/" + file + ".json"))
        val parser = new ScallionParser
        assert(parser(tokens).nonEmpty)
      }
    }

    measure method "ANTLR parse" in {
      import org.antlr.v4.runtime._
      using(files) in { file =>
        val lexer = new json.antlr.JSONLexer(CharStreams.fromFileName("benchmark/resources/" + file + ".json"))
        val parser = new json.antlr.JSONParser(new CommonTokenStream(lexer))
        parser.json()
      }
    }
  }
}
