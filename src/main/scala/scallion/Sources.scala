package scallion

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
trait CharSources extends Sources {
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