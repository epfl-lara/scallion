package scallion

import scala.collection.mutable.ArrayBuffer
import scala.io

/** Contains definitions related to input sources. */
trait Sources extends Positions {

  /** Type of characters handled by the sources. */
  type Character

  /** Represents a sequence of characters mutably traversed over.
    *
    * The sequence can be traversed only once, but arbitrary long lookaheads are supported.
    *
    * Sources conceptually contain two mutable pointers:
    * - The base pointer, which points past the fully traversed portion of the sequence.
    * - The lookahead pointer, which can point arbitrarily past the base pointer. 
    *
    * The base pointer can be advanced all the way through the lookahead pointer using `consume()`.
    * The sequence of characters advanced over are returned.
    *
    * The lookahead pointer can be advanced by one character at the time using `ahead()`,
    * and can be reset to the base pointer by a call to `back()`.
    */
  trait Source {

    /** Checks if the lookahead pointer is at the end of the sequence. */
    def atEnd: Boolean

    /** Advances the lookahead pointer by one character in the sequence.
      *
      * @return The character that was advanced over.
      */
    def ahead(): Character

    /** Consumes all characters that are currently looked ahead.
      *
      * @return The sequence of characters.
      */
    def consume(): Seq[Character]

    /** Resets the lookahead pointer. */
    def back(): Unit

    /** Current position of the lookahead pointer in the source. */
    def currentPosition: Position
  }

  /** Source over an iterator. */
  abstract class IteratorSource(start: Position, it: Iterator[Character]) extends Source {

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

  /** Source over multiple sources. */
  class SeqSource(sources: Seq[Source]) extends Source {
    require(sources.nonEmpty)

    var consumedSources = 0
    var activeSources = Seq(sources.head)
    var inactiveSources = sources.tail

    override def atEnd =
      (consumedSources + activeSources.length) == sources.length &&
      activeSources.last.atEnd
    
    override def ahead(): Character = {
      while (activeSources.last.atEnd) {
        activeSources = activeSources :+ inactiveSources.head
        inactiveSources = inactiveSources.tail
      }
      activeSources.last.ahead()
    }

    override def consume(): Seq[Character] = {
      val res = activeSources.flatMap(_.consume())
      consumedSources += activeSources.size - 1
      activeSources = Seq(activeSources.last)
      res
    }

    override def back(): Unit = {
      activeSources.foreach(_.back())
      activeSources = Seq(activeSources.head)
    }

    override def currentPosition: Position =
      activeSources.last.currentPosition
  }

  /** Supports subsequence lookups. */
  trait Retrievable {

    /** Look up a subsequence.
      *
      * @param start Start position, inclusive.
      * @param end   End position, exclusive.
      */
    def retrieve(start: Position, end: Position): Seq[Character]
  }
}

/** Contains sources for handling character sequences. */
trait StringSources extends Sources {
  type Character = Char

  /** Position in a string. */
  case class Position(index: Int, row: Int, column: Int) {
    def +(char: Character): Position = if (char == '\n') {
      Position(index + 1, row + 1, 0)
    } else {
      Position(index + 1, row, column + 1)
    }
  }

  /** Source over a string. */
  class StringSource(string: String) extends Source with Retrievable {

    // Base pointer.
    private var basePos: Position = Position(0, 0, 0)

    // Lookahead pointer.
    private var aheadPos: Position = basePos

    override def atEnd = aheadPos.index >= string.length

    override def ahead(): Character = {
      val value = string(aheadPos.index)
      aheadPos += value
      value
    }

    override def consume(): Seq[Character] = {
      val value = string.substring(basePos.index, aheadPos.index)
      basePos = aheadPos
      value
    }

    override def back(): Unit = {
      aheadPos = basePos
    }

    override def currentPosition: Position = aheadPos

    def retrieve(start: Position, end: Position): Seq[Character] = {
      string.substring(start.index, end.index)
    }
  }
}

/** Contains sources for handling character sequences from files. */
trait FileSources extends Sources {
  type Character = Char

  /** Position in a file. */
  case class Position(file: Option[String], index: Int, row: Int, column: Int) {
    def +(char: Character): Position = if (char == '\n') {
      Position(file, index + 1, row + 1, 0)
    } else {
      Position(file, index + 1, row, column + 1)
    }
  }

  /** Source over a file. */
  class FileSource(file: String) extends IteratorSource(Position(Some(file), 0, 0, 0), io.Source.fromFile(file)) {
    def increment(pos: Position, char: Character): Position = pos + char
  }

  /** Source over a string. */
  class StringSource(text: String) extends IteratorSource(Position(None, 0, 0, 0), text.toIterator) {
    def increment(pos: Position, char: Character): Position = pos + char
  }
}