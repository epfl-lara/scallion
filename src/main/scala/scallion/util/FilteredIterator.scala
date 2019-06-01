/* Copyright 2019 EPFL, Lausanne
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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