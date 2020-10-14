/* Copyright 2020 EPFL, Lausanne
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

import scala.io.AnsiColor._

/** Contains methods to produce reports to help debug LL(1) conflicts in syntaxes.
  *
  * @group debug
  */
trait Debug { self: Syntaxes with Parsing with Enumeration =>

  import Conflict._

  /** Prints a report of LL(1) conflicts in the syntax.
    *
    * @group debug
    */
  def debug[A](syntax: Syntax[A], showTrails: Boolean=true, ansi: Boolean=true): Unit =
    print(debugString(syntax, showTrails))

  /** Returns a report of LL(1) conflicts in the syntax.
    *
    * @group debug
    */
  def debugString[A](syntax: Syntax[A], showTrails: Boolean=true, ansi: Boolean=true): String = {
    val builder = new StringBuilder()

    val m: String => String = if (ansi) { (mod: String) => mod } else { (mod: String) => "" }

    builder ++= s"${m(BOLD)}=== LL(1) Conflicts Report ===${m(RESET)}\n\n"

    Parser.build(syntax) match {
      case Right(parser) => {
        builder ++= "The syntax does not contain any conflicts.\n"
        if (!parser.isProductive) {
          builder ++= s"${m(BOLD)}However, the syntax is not productive.${m(RESET)}\n"
          builder ++= "No sequences of tokens are recognized by this syntax.\n"
        }
      }
      case Left(conflicts) => {
        builder ++= s"${m(BOLD)}${m(RED)}The syntax is not LL(1).${m(RESET)}\n\n"

        val n = conflicts.size

        if (n == 1) {
          builder ++= "A single conflict has been found:\n"
        }
        else {
          builder ++= s"${n} conflicts have been found:\n"
        }

        for ((conflict, i) <- conflicts.zipWithIndex) {
          builder ++= s"\n${m(BOLD)}--- Conflict ${i+1}/$n ---${m(RESET)}\n\n"

          import scala.language.existentials

          val (root, kinds): (Syntax[_], Set[Kind]) = conflict match {
            case NullableConflict(source) => {
              builder ++= s"${m(BOLD)}Nullable/Nullable conflict.${m(RESET)}\n\n"
              builder ++= "Both branches of a disjunction are nullable.\n"
              (source, Set())
            }
            case FirstConflict(source, kinds) => {
              builder ++= s"${m(BOLD)}First/First conflict.${m(RESET)}\n\n"
              builder ++= "Both branches of a disjunction can start with the same token.\n\n"
              if (kinds.size == 1) {
                builder ++= "The ambiguous token kind is "
              }
              else {
                builder ++= "The ambiguous token kinds are "
              }
              builder ++= s"${kinds.mkString(", ")}.\n"
              (source, kinds)
            }
            case FollowConflict(source, root, kinds) => {
              builder ++= s"${m(BOLD)}First/Follow conflict.${m(RESET)}\n\n"
              builder ++= "The left branch of a sequence can stop or continue on the same token than the right side can start with.\n\n"
              if (kinds.size == 1) {
                builder ++= "The ambiguous token kind is "
              }
              else {
                builder ++= "The ambiguous token kinds are "
              }
              builder ++= s"${kinds.mkString(", ")}.\n"
              (root, kinds)
            }
          }
          builder ++= "\n"

          builder ++= "The source of the conflict can be traced to:\n\n"

          for (elem <- root.trace) {
            builder ++= "  " + elem.toString + "\n"
          }

          if (showTrails) {
            builder ++= "\n"
            val trails = Enumerator(syntax.prefixOf(conflict.source)).take(5).toList.distinct
            if (trails.nonEmpty) {
              if (kinds.size > 1) {
                builder ++= s"The following sequences lead to an ambiguity when followed by any of ${kinds.mkString(", ")}:\n\n"
              }
              else if (kinds.size == 1) {
                builder ++= s"The following sequences lead to an ambiguity when followed by of token of kind ${kinds.head}:\n\n"
              }
              else {
                builder ++= "The following sequences lead to an ambiguity:\n\n"
              }
              for ((trail, j) <- trails.zipWithIndex) {
                builder ++= s"  (${j+1}) ${if (trail.isEmpty)
                  "The empty sequence of tokens." else trail.mkString(" ")}\n"
              }
            }
          }
        }
      }
    }

    builder.toString
  }
}