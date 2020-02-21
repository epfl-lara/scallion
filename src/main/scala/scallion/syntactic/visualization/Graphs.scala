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
package syntactic
package visualization

import scala.language.existentials

import scala.collection.mutable.{Queue, StringBuilder}

/** Contains utilities to visualize syntaxes as graphs using Graphviz. */
trait Graphs { self: Syntaxes =>

  /** Contains utilities to visualize syntaxes as graphs using Graphviz.
    *
    * @group visualization
    */
  object graphs {

    private case class Node(id: Int, label: String, targets: Seq[Int])
    private type Graph = Seq[Node]

    import Syntax._

    private def getGraph[A](syntax: Syntax[A]): Graph = {
      var nextId = 0
      var nodes = Vector[Node]()
      val queue = new Queue[(Syntax[_], Int)]
      var ids = Map[Syntax[_], Int]()

      def inspect[B](next: Syntax[B]): Int = {
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

      inspect(syntax)

      while(queue.nonEmpty) {
        import scala.language.existentials
        val (current: Syntax[_], id: Int) = queue.dequeue()

        val (label, targets) = current match {
          case Failure() => ("⊥", Seq())
          case Success(_, _) => ("𝛆", Seq())
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
          case Marked(mark, inner) =>  {
            val innerId = inspect(inner)

            ("marked (" + mark + ")", Seq(innerId))
          }
          case Transform(_, _, inner) => {
            val innerId = inspect(inner)

            ("map", Seq(innerId))
          }
          case Recursive(_, inner) => {
            val innerId = inspect(inner)

            ("rec", Seq(innerId))
          }
        }

        nodes :+= Node(id, label, targets)
      }

      nodes
    }

    private def escape(string: String): String =
      string.replace("\\", "\\\\")

    /** Returns a Graphviz representation of the syntax. */
    private def toGraphviz[A](syntax: Syntax[A]): String = {

      val graph = getGraph(syntax)

      def addPorts[B](elems: Seq[B]): Seq[(B, String)] = {
        val n = elems.size

        if (n == 2) {
          elems.zip(Seq(":sw", ":se"))
        }
        else {
          elems.zip(Seq.tabulate(n)(i => ""))
        }
      }

      val builder = new StringBuilder()
      builder ++= "subgraph cluster0 {\n"
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
        builder ++= id.toString + " [label=\"" + escape(label) + "\"];\n"
      }
      builder ++= "}\n"
      builder.toString
    }

    /** Produces a graph representation of the syntax as a PDF file using `dot` from Graphviz.
      *
      * @param syntax   The syntax to display.
      * @param location The directory in which to save the files.
      * @param name     The name of the files. Will be postfixed by respectively `.dot` and `.pdf`.
      */
    def outputGraph[A](syntax: Syntax[A], location: String, name: String): Unit = {
      import java.nio.file._
      import sys.process._

      val content = "digraph G {\n" + toGraphviz(syntax) + "}\n"
      val dotPath = Paths.get(location, name + ".dot")
      val pdfPath = Paths.get(location, name + ".pdf")

      Files.write(dotPath, content.getBytes())

      ("dot " + dotPath + " -Tpdf -o" + pdfPath).!
    }
  }
}