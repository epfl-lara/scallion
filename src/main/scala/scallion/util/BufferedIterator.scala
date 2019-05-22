package scallion.util

import scala.collection.mutable.Queue

class BufferedIterator[A] extends Iterator[A] {
  override def toString = "<iterator>"

  private var ended = false

  private val elements = new Queue[A]()

  override def hasNext: Boolean = synchronized {
    while (!ended && elements.isEmpty) {
      wait()
    }
    !elements.isEmpty
  }

  override def next(): A = synchronized {
    while (!ended && elements.isEmpty) {
      wait()
    }
    elements.dequeue()
  }

  def add(elem: A): Unit = synchronized {
    require(!ended)
    elements.enqueue(elem)
    notifyAll()
  }

  def end(): Unit = synchronized {
    ended = true
    notifyAll()
  }
}