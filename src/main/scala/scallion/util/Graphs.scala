
package scallion
package util

import scala.collection.mutable.{Queue, StringBuilder}

trait Graphs[Kind] { self: Parsers[_, Kind] =>

  case class Node(id: Int, label: String, targets: Seq[Int])

  type Graph = Seq[Node]

  import Parser._
  def getGraph(parser: Parser[Any]): Graph = {
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

  def toDot(graph: Graph): String = {

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

  def display(graph: Graph, location: String, name: String): Unit = {
    import java.nio.file._
    import sys.process._

    val content = toDot(graph)
    val dotPath = Paths.get(location, name + ".dot")
    val pdfPath = Paths.get(location, name + ".pdf")
    
    Files.write(dotPath, content.getBytes())

    ("dot " + dotPath + " -Tpdf -o" + pdfPath) !
  }
}