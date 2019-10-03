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

package scallion.syntactic

/** Contains methods to produce reports to help debug LL(1) conflicts in syntaxes.
  *
  * @group debug
  */
trait Debug[Token, Kind] { self: Syntaxes[Token, Kind] =>

  import LL1Conflict._

  /** Prints a report of LL(1) conflicts in the syntax.
    *
    * @group debug
    */
  def debug[A](syntax: Syntax[A]): Unit =
    print(debugString(syntax))

  /** Returns a report of LL(1) conflicts in the syntax.
    *
    * @group debug
    */
  def debugString[A](syntax: Syntax[A]): String = {
    val builder = new StringBuilder()

    val conflicts = syntax.conflicts.toList

    builder ++= "=== LL(1) Check Report ===\n\n"

    if (conflicts.isEmpty) {
      builder ++= "The syntax is LL(1)"
      if (syntax.isProductive) {
        builder ++= " and productive.\n"
      }
      else {
        builder ++= ", but is not productive.\n"
        builder ++= "No sequences of tokens are recognized by this syntax.\n"
      }
    }
    else {
      builder ++= "The syntax is not LL(1).\n\n"

      val n = conflicts.size

      if (n == 1) {
        builder ++= "A single conflict has been found:\n"
      }
      else {
        builder ++= s"${n} conflicts have been found:\n"
      }

      for ((conflict, i) <- conflicts.zipWithIndex) {
        builder ++= s"\n--- Conflict ${i+1}/$n ---\n\n"
        conflict match {
          case NullableConflict(source) => {
            builder ++= "There exists a disjunction where both alternatives are nullable.\n"
            builder ++= s"The nullable value on the left branch is ${source.left.nullable.get},\n"
            builder ++= s"while the value on the right branch is ${source.right.nullable.get}.\n"
          }
          case FirstConflict(source, kinds) => {
            builder ++= "There exists a disjunction where "
            builder ++= "both alternatives can start with the same token.\n"
            if (kinds.size == 1) {
              builder ++= "The ambiguous token kind is "
            }
            else {
              builder ++= "The ambiguous token kinds are "
            }
            builder ++= s"${kinds.mkString(", ")}.\n"
            builder ++= "The first set of the left branch is "
            builder ++= s"{ ${source.left.first.mkString(", ")} },\n"
            builder ++= "while the first set of the right branch is "
            builder ++= s"{ ${source.right.first.mkString(", ")} }.\n"
          }
          case FollowConflict(source, root, kinds) => {
            builder ++= "There exists a nullable syntax directly followed by "
            builder ++= "a syntax that can start with the same token.\n"
            if (kinds.size == 1) {
              builder ++= "The ambiguous token kind is "
            }
            else {
              builder ++= "The ambiguous token kinds are "
            }
            builder ++= s"${kinds.mkString(", ")}.\n"
            builder ++= "The first set of the nullable syntax is "
            builder ++= s"{ ${source.first.mkString(", ")} },\n"
            builder ++= "while first set of the syntax that directly follows is "
            builder ++= s"{ ${root.right.first.mkString(", ")} }.\n"
          }
        }
        builder ++= "\n"

        val trails = conflict.witnessedFrom(syntax).take(3).toList.distinct
        if (trails.nonEmpty) {
          builder ++= "The following sequences lead to an ambiguity:\n"
          for ((trail, j) <- trails.zipWithIndex) {
            builder ++= s"  (${j+1}) ${if (trail.isEmpty)
              "The empty sequence of tokens" else trail.mkString(" ")}.\n"
          }
        }
      }
    }

    builder.toString
  }
}