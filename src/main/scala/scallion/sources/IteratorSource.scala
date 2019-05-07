package scallion
package sources

import scala.collection.mutable.ArrayBuffer

/** Source over an iterator. */
abstract class IteratorSource[Character, Position]
    (start: Position, it: Iterator[Character]) extends Source[Character, Position] {

  def increment(pos: Position, char: Character): Position

  private var buffer: ArrayBuffer[Character] = new ArrayBuffer()
  private var index: Int = 0
  private var basePos: Position = start
  private var aheadPos: Position = start

  /** Checks if the lookahead pointer is at the end of the sequence. */
  def atEnd: Boolean = index >= buffer.size && !it.hasNext

  /** Advances the lookahead pointer by one character in the sequence.
    *
    * @return The character that was advanced over.
    */
  def ahead(): Character = {
    if (index >= buffer.size) {
      val res = it.next()
      buffer += res
      index += 1
      aheadPos = increment(aheadPos, res)
      res
    }
    else {
      val res = buffer(index)
      index += 1
      aheadPos = increment(aheadPos, res)
      res
    }
  }

  /** Consumes all characters that are currently looked ahead.
    *
    * @return The sequence of characters.
    */
  def consume(): Seq[Character] = {
    val res = buffer.slice(0, index).toSeq
    buffer = buffer.drop(index)
    basePos = aheadPos
    index = 0
    res
  }

  /** Resets the lookahead pointer. */
  def back(): Unit = {
    aheadPos = basePos
    index = 0
  }

  /** Current position of the lookahead pointer in the source. */
  def currentPosition: Position = aheadPos
}