/* Copyright 2019 EPFL, Lausanne */

package scallion.util

import scala.annotation._

class FilteredIterator[A](iterator: Iterator[A], predicate: A => Boolean) extends Iterator[A] {
  private var cacheNext: Option[A] = None
  private var ended = false

  @tailrec
  private def retrieveNext(): Unit = {
    if (!iterator.hasNext) {
      ended = true
    }
    else {
      val candidate = iterator.next()
      if (predicate(candidate)) {
        cacheNext = Some(candidate)
      }
      else {
        retrieveNext()
      }
    }
  }

  override def hasNext: Boolean = {
    if (ended) {
      false
    }
    else if (cacheNext.nonEmpty) {
      true
    }
    else {
      retrieveNext()
      hasNext
    }
  }

  override def next(): A = {
    if (ended) {
      throw new NoSuchElementException("next on empty iterator")
    }
    else if (cacheNext.nonEmpty) {
      val res = cacheNext.get
      cacheNext = None
      res
    }
    else {
      retrieveNext()
      next()
    }
  }
}