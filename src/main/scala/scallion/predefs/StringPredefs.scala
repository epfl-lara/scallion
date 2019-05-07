package scallion
package predefs

trait StringPredefs {

  /** Position in a string. */
  case class Position(index: Int, row: Int, column: Int) {
    def +(char: Char): Position = if (char == '\n') {
      Position(index + 1, row + 1, 0)
    } else {
      Position(index + 1, row, column + 1)
    }
  }

  /** Source over a string. */
  class StringSource(string: String) extends Source[Char, Position] {

    // Base pointer.
    private var basePos: Position = Position(0, 0, 0)

    // Lookahead pointer.
    private var aheadPos: Position = basePos

    override def atEnd = aheadPos.index >= string.length

    override def ahead(): Char = {
      val value = string(aheadPos.index)
      aheadPos += value
      value
    }

    override def consume(): Seq[Char] = {
      val value = string.substring(basePos.index, aheadPos.index)
      basePos = aheadPos
      value
    }

    override def back(): Unit = {
      aheadPos = basePos
    }

    override def currentPosition: Position = aheadPos

    def retrieve(start: Position, end: Position): Seq[Char] = {
      string.substring(start.index, end.index)
    }
  }
}