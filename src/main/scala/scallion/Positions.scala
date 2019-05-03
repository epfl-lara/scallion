package scallion

/** Contains types relating to positions. */
trait Positions {
  type Position
  type Range = (Position, Position)
}