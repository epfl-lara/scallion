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

package scallion
package parsing
package visualization

import scala.collection.mutable.{Queue, StringBuilder}

/** Contains utilities to visualize parsers as graphs using Graphviz. */
trait Graphs[Kind] { self: Parsers[_, Kind] =>

  /** Contains utilities to visualize parsers as graphs using Graphviz.
    *
    * @group visualization
    */
  object graphs {

    private case class Node(id: Int, label: String, targets: Seq[Int])
    private type Graph = Seq[Node]

    import Parser._

    private def getGraph(parser: Parser[Nothing, Any]): Graph = {
      var nextId = 0
      var nodes = Vector[Node]()
      val queue = new Queue[(Parser[Nothing, Any], Int)]
      var ids = Map[Parser[Nothing, Any], Int]()

      def inspect(next: Parser[Nothing, Any]): Int = {
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
          case Transform(_, _, inner) => {
            val innerId = inspect(inner)

            ("map", Seq(innerId))
          }
          case Recursive(inner) => {
            val innerId = inspect(inner)

            ("rec", Seq(innerId))
          }
        }

        nodes :+= Node(id, label, targets)
      }

      nodes
    }

    /** Returns a Graphviz representation of the parser. */
    private def toGraphviz(parser: Parser[Nothing, Any]): String = {

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
    def outputGraph(parser: Parser[Nothing, Any], location: String, name: String): Unit = {
      import java.nio.file._
      import sys.process._

      val content = toGraphviz(parser)
      val dotPath = Paths.get(location, name + ".dot")
      val pdfPath = Paths.get(location, name + ".pdf")

      Files.write(dotPath, content.getBytes())

      ("dot " + dotPath + " -Tpdf -o" + pdfPath).!
    }
  }
}