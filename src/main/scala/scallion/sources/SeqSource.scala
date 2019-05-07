package scallion
package sources

/** Source over multiple sources. */
class SeqSource[Character, Position](sources: Seq[Source[Character, Position]])
    extends Source[Character, Position] {
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