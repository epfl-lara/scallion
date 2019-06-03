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
package input

/** Indicates the start position and how to increment positions.
  *
  * @group positioner
  */
trait Positioner[-Character, Position] {

  /** The start position. */
  val start: Position

  /** Increments a `position` by a single `character`. */
  def increment(position: Position, character: Character): Position
}

/** Position within a string.
  *
  * @param index  The index in the string. Zero-based.
  * @param line   The line number. Zero-based.
  * @param column The column number. Zero-based.
  *
  * @see See [[scallion.input.StringPositioner]] for the corresponding positioner.
  *
  * @group position
  */
case class StringPosition(index: Int, line: Int, column: Int) {

  @inline
  def +(char: Char): StringPosition =
    if (char == '\n') {
      StringPosition(index + 1, line + 1, 0)
    }
    else {
      StringPosition(index + 1, line, column + 1)
    }
}

/** Positioner for [[scallion.input.StringPosition]].
  *
  * @group positioner
  */
object StringPositioner extends Positioner[Char, StringPosition] {
  override val start: StringPosition = StringPosition(0, 0, 0)

  @inline
  override def increment(position: StringPosition, character: Char): StringPosition =
    position + character
}

/** General index-based positioner.
  *
  * @group positioner
  */
object IndexPositioner extends Positioner[Any, Int] {
  val start: Int = 0

  @inline
  def increment(position: Int, character: Any): Int = position + 1
}

/** Positioner for `Unit` positions.
  *
  * @group positioner
  */
object NoPositioner extends Positioner[Any, Unit] {
  val start: Unit = ()

  @inline
  def increment(position: Unit, character: Any): Unit = ()
}