/* Copyright 2019 EPFL, Lausanne */

package scallion.util.internal

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
    elements.enqueue(elem)
    notifyAll()
  }

  def addAll(elems: Seq[A]): Unit = synchronized {
    elements ++= elems
    notifyAll()
  }

  def end(): Unit = synchronized {
    ended = true
    notifyAll()
  }
}