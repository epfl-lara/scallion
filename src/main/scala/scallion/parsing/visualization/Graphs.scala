
package scallion
package parsing
package visualization

import scala.collection.mutable.{Queue, StringBuilder}

/** Contains utilities to vizualize parsers as graphs using Graphviz.
  * Expected to be mixed-in [[scallion.parsing.Parsers]]. */
trait Graphs[Kind] { self: Parsers[_, Kind] =>

  private case class Node(id: Int, label: String, targets: Seq[Int])

  private type Graph = Seq[Node]

  import Parser._

  private def getGraph(parser: Parser[Any]): Graph = {
    var nextId = 0
    var nodes = Vector[Node]()
    val queue = new Queue[(Parser[Any], Int)]
    var ids = Map[Parser[Any], Int]()

    def inspect(next: Parser[Any]): Int = {
      if (!ids.contains(next)) {
        val res = nextId
        nextId += 1
        ids += next -> res
        queue.enqueue(next -> res)
        res
      }
      else {
        ids(next)
      }
    }

    inspect(parser)

    while(queue.nonEmpty) {
      val (current, id) = queue.dequeue()

      val (label, targets) = current match {
        case Failure => ("⊥", Seq())
        case Success(_) => ("𝛆", Seq())
        case Elem(kind) => (kind.toString, Seq())
        case Disjunction(left, right) => {

          val leftId = inspect(left)
          val rightId = inspect(right)

          ("|", Seq(leftId, rightId))
        }
        case Sequence(left, right) => {

          val leftId = inspect(left)
          val rightId = inspect(right)

          ("~", Seq(leftId, rightId))
        }
        case Concat(left, right) => {

          val leftId = inspect(left)
          val rightId = inspect(right)

          ("++", Seq(leftId, rightId))
        }
        case Transform(_, inner) => {
          val innerId = inspect(inner)

          ("map", Seq(innerId))
        }
        case r@Recursive(_) => {
          val innerId = inspect(r.inner)

          ("rec", Seq(innerId))
        }
      }

      nodes :+= Node(id, label, targets)
    }

    nodes
  }

  /** Returns a Graphviz representation of the parser. */
  private def toGraphviz(parser: Parser[Any]): String = {

    val graph = getGraph(parser)

    def addPorts[A](elems: Seq[A]): Seq[(A, String)] = {
      val n = elems.size

      if (n == 2) {
        elems.zip(Seq(":sw", ":se"))
      }
      else {
        elems.zip(Seq.tabulate(n)(i => ""))
      }
    }

    val builder = new StringBuilder()
    builder ++= "digraph G {\n"
    builder ++= "node [shape=box];\n"
    for {
      Node(id, _, targets) <- graph
      (target, port) <- addPorts(targets)
    } {
      builder ++= id.toString + port + " -> " + target.toString + ";\n"
    }
    for {
      Node(id, label, _) <- graph
    } {
      builder ++= id.toString + " [label=\"" + label + "\"];\n"
    }
    builder ++= "}\n"
    builder.toString
  }

  /** Produces a graph representation of the parser as a PDF file using `dot` from Graphviz.
    *
    * @param parser   The parser to display.
    * @param location The directory in which to save the files.
    * @param name     The name of the files. Will be postfixed by respectively `.dot` and `.pdf`.
    */
  def outputGraph(parser: Parser[Any], location: String, name: String): Unit = {
    import java.nio.file._
    import sys.process._

    val content = toGraphviz(parser)
    val dotPath = Paths.get(location, name + ".dot")
    val pdfPath = Paths.get(location, name + ".pdf")

    Files.write(dotPath, content.getBytes())

    ("dot " + dotPath + " -Tpdf -o" + pdfPath).!
  }
}