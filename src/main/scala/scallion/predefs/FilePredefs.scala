package scallion
package predefs

import scala.io

import scallion.sources.IteratorSource

trait FilePredefs {

  /** Position in a file. */
  case class Position(file: Option[String], index: Int, row: Int, column: Int) {
    def +(char: Char): Position = if (char == '\n') {
      Position(file, index + 1, row + 1, 0)
    } else {
      Position(file, index + 1, row, column + 1)
    }
  }

  /** Source over a file. */
  class FileSource(file: String)
      extends IteratorSource[Char, Position](
        Position(Some(file), 0, 0, 0),
        io.Source.fromFile(file)) {

    def increment(pos: Position, char: Char): Position = pos + char
  }

  /** Source over a string. */
  class StringSource(text: String)
      extends IteratorSource[Char, Position](Position(None, 0, 0, 0), text.toIterator) {
    def increment(pos: Position, char: Char): Position = pos + char
  }
}