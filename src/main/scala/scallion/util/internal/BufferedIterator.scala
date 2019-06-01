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