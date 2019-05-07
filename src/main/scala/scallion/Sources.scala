package scallion

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
trait Source[Character, Position] {

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