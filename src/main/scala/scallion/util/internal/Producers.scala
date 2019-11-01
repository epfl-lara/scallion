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

import scala.collection.mutable.ArrayBuffer

/** Result of a `peek`. */
private[internal] sealed trait Peek[+A] {

  /** Indicates if the result can not change. */
  def isStable: Boolean = this != Unavailable

  /** Returtns `true` if the next value is not available, `false` otherwise. */
  def isEmpty: Boolean = this match {
    case Available(_) => false
    case _ => true
  }

  /** Returtns `true` if the next value is available, `false` otherwise. */
  def nonEmpty: Boolean = !isEmpty

  /** Applies a function on the peeked value. */
  def map[B](f: A => B): Peek[B] = this match {
    case Available(value) => Available(f(value))
    case Terminated => Terminated
    case Unavailable => Unavailable
  }

  /** Applies a unit function on the peeked value, if any. */
  def foreach(f: A => Unit): Unit = this match {
    case Available(value) => f(value)
    case _ => ()
  }
}

/** Indicates that the next value is available. */
private[internal] case class Available[+A](value: A) extends Peek[A]

/** Indicates that the producer is terminated. */
private[internal] case object Terminated extends Peek[Nothing]

/** Indicates that the next value, if any, is not yet available. */
private[internal] case object Unavailable extends Peek[Nothing]

/** Producers are data structures that lazily produce values.
  *
  * The current value produced by the producer can be requested using `peek()`.
  * Peek will return an `Available(value)` response in case a value has been produced,
  * or a `Terminated` response in case the producer is done. We say in this case that the result
  * is stable. Up until `skip()` is called, subsequent calls to `peek()` will always return
  * the same value, and do so effeciently.
  * The `peek()` call can also result in an `Unavailable` answer, which means that in the current
  * state the next value, if any, is not yet available.
  * Once state has changed, later calls to `peek()` may result in a stable answer.
  *
  * To skip a value, the method `skip()` is used. The method can only be used after a call to `peek()`
  * has returned an `Available(value)`, and so only once per such call.
  *
  * Producers can be converted to iterators using `toIterator`.
  */
trait Producer[+A] { self =>

  /** Returns the next value to be produced, if any.
    *
    * If the result is *stable* (checked using `isStable`),
    * the result of invoking this method will not change until
    * `skip()` is called.
    */
  private[internal] def peek(): Peek[A]

  /** Skips the produced value.
    *
    * This method should only be called after
    * a successful call to `peek()`
    * (i.e. a call returning an `Available` value),
    * and so only once per such call.
    */
  private[internal] def skip(): Unit

  /** Apply a function on the produced values. */
  def map[B](f: A => B): Producer[B] = new Producer[B] {
    private var cache: Option[Peek[B]] = None

    override private[internal] def peek(): Peek[B] = cache match {
      case Some(peeked) => peeked
      case None => {
        val peeked = self.peek().map(f)
        if (peeked.isStable) {
          cache = Some(peeked)
        }
        peeked
      }
    }
    override private[internal] def skip(): Unit = {
      cache = None
      self.skip()
    }
  }

  /** Converts this producer to an iterator.
    *
    * `this` producer should no longer be used after
    * calling this method.
    */
  def toIterator: Iterator[A] = new Iterator[A] {
    private var cache: Option[Peek[A]] = None

    private def getCache(): Peek[A] = cache match {
      case Some(value) => value
      case None => {
        val value = peek()
        cache = Some(value)
        value
      }
    }

    override def hasNext: Boolean = getCache().nonEmpty

    override def next(): A = getCache() match {
      case Available(value) => {
        cache = None
        skip()
        value
      }
      case _ => throw new NoSuchElementException("Empty iterator.")
    }
  }
}

/** Contains utilities to build producers. */
object Producer {

  /** The empty producer. */
  val empty: Producer[Nothing] = new Producer[Nothing] {
    override private[internal] def peek(): Peek[Nothing] = Terminated
    override private[internal] def skip(): Unit = ()
  }

  /** Returns a new producer that produces the given `value` a single time. */
  def single[A](value: A): Producer[A] = new Producer[A] {
    private var result: Peek[A] = Available(value)
    override private[internal] def peek(): Peek[A] = result
    override private[internal] def skip(): Unit = result = Terminated
  }

  /** Returns a new producer that produces the values provided by the `iterator`. */
  def fromIterator[A](iterator: Iterator[A]): Producer[A] = new IteratorProducer(iterator)

  /** Returns a producer and a function that returns fresh views over the producer.
    *
    * @param producer The producer to duplicate. Should not be used again after this call.
    */
  def duplicate[A](producer: Producer[A]): (Producer[A], () => Producer[A]) = {
    val memorized = new MemoryProducer(producer)

    (memorized, () => memorized.createView())
  }

  /** Returns a lazy wrapper around a producer. */
  def lazily[A](producer: => Producer[A]): Producer[A] = new Producer[A] {
    private lazy val inner: Producer[A] = producer

    override private[internal] def peek(): Peek[A] = inner.peek()
    override private[internal] def skip(): Unit = inner.skip()
  }
}

/** Extra operations for producers of values with a
  * Posively Totally Preordered Semigroup (PTPS) defined on them.
  */
class ProducerOps[A](ptps: PTPS[A]) {

  import ptps._

  /** Union of two producers.
    *
    * If the two producers produce values in increasing order,
    * then the resulting producer will also produce values in order.
    */
  def union(left: Producer[A], right: Producer[A]): Producer[A] = new Producer[A] {

    // Contains the latest peeked value, and the side which produced it, if any.
    private var cache: Option[Peek[Either[A, A]]] = None

    private def getCache(): Peek[Either[A, A]] = cache match {
      case Some(peeked) => peeked  // Cache hit.
      case None => {
        // Cache miss. We peek() the value produced by both sides.
        val peeked = (left.peek(), right.peek()) match {
          case (Available(x), Available(y)) => {
            // Two values are available, we pick the smallest.
            if (lessEquiv(x, y)) {
              Available(Left(x))
            }
            else {
              Available(Right(y))
            }
          }
          // Only one value is available, we pick it.
          case (Available(x), _) => Available(Left(x))
          case (_, Available(y)) => Available(Right(y))
          // Both sides are terminated, therefore the union also is.
          case (Terminated, Terminated) => Terminated
          // Otherwise the result is not available.
          case _ => Unavailable
        }

        // Update the cache only if we are guaranteed that the result
        // will not change.
        if (peeked.isStable) {
          cache = Some(peeked)
        }

        peeked
      }
    }

    override private[internal] def peek(): Peek[A] = getCache() match {
      // Removes the information about which side produced the value.
      case Available(Left(value)) => Available(value)
      case Available(Right(value)) => Available(value)
      case Terminated => Terminated
      case Unavailable => Unavailable
    }

    override private[internal] def skip(): Unit = {
      cache.foreach { peeked =>
        peeked.foreach {
          // Skip only on the side the produced the value.
          case Left(_) => left.skip()
          case Right(_) => right.skip()
        }
      }

      cache = None
    }
  }

  /** Product of two producers.
    *
    * If the two producers produce values in increasing order,
    * then the resulting producer will produce their `append` in increasing order.
    */
  def product(left: Producer[A], right: Producer[A]): Producer[A] = new Producer[A] {
    // Create a main Producer and lazy views of that producer for the `right` producer.
    private val (mainRight, createRightView) = Producer.duplicate(right)

    // Buffer of producers of appended values.
    // Ultimately, there will be one entry in the buffer for each
    // value produced by `left`.
    private val appendProducers: ArrayBuffer[Producer[A]] = new ArrayBuffer()

    // Indicates if the latest value of `left` has been handled.
    private var included: Boolean = false

    // Indicates if the `left` producer has terminated.
    private var leftTerminated: Boolean = false

    // Cache of produced value and index of the producer that produced it.
    private var cache: Option[Peek[(Int, A)]] = None

    // Tentatively includes one more append producer.
    private def includeMore(): Unit = {
      left.peek() match {
        case Available(leftValue) => {
          // Creates a fresh producer of values from the `right`.
          val rightProducer = if (appendProducers.isEmpty) mainRight else createRightView()

          // Creates the next append producer.
          appendProducers += rightProducer.map(rightValue => append(leftValue, rightValue))

          included = true
          left.skip()
        }
        case Terminated => {
          included = true
          leftTerminated = true
        }
        case Unavailable => ()
      }
    }

    private def getCache(): Peek[(Int, A)] = cache match {
      case Some(peeked) => peeked  // Cache hit.
      case None => {

        // Cache miss.

        // Check if one more append producer must be included.
        if (!included) {
          includeMore()
        }

        // Keep track of the best answer we can give.
        var best: Peek[(Int, A)] = if (leftTerminated) Terminated else Unavailable

        for (i <- 0 until appendProducers.size) {
          appendProducers(i).peek() match {
            case Available(appendValue) => {  // A candidate append value is available.
              best match {
                // We update the best accordingly.
                case Available((_, bestValue)) => {
                  if (!lessEquiv(bestValue, appendValue)) {
                    best = Available((i, appendValue))
                  }
                }
                case _ => best = Available((i, appendValue))
              }
            }
            case Unavailable => {
              // A append producer is not yet available,
              // therefore we can no longer say that the
              // producer is terminated.
              if (best == Terminated) {
                best = Unavailable
              }
            }
            case Terminated => ()
          }
        }

        // Update the cache.
        if (best.isStable) {
          cache = Some(best)
        }

        best
      }
    }

    override private[internal] def peek(): Peek[A] = getCache() match {
      // Get rid of the information about which
      // append producer is responsible for the value.
      case Available((_, value)) => Available(value)
      case Terminated => Terminated
      case Unavailable => Unavailable
    }
    override private[internal] def skip(): Unit = cache.foreach { peeked =>
      peeked.foreach {
        case (i, _) => {
          // Only call `skip()` on the append producer that produced the value.
          appendProducers(i).skip()
          if (i == appendProducers.size - 1 && !leftTerminated) {
            included = false
          }
          cache = None
        }
      }
    }
  }
}

/** Producer with an internal memory of the value produced. */
private class MemoryProducer[A](producer: Producer[A]) extends Producer[A] {

  private val buffer: ArrayBuffer[A] = new ArrayBuffer[A](1)

  private var ended: Boolean = false

  private var stored: Boolean = false

  override private[internal] def peek(): Peek[A] = producer.peek() match {
    case res@Available(value) => {
      if (!stored) {
        buffer += value
        stored = true
      }
      res
    }
    case res@Terminated => {
      ended = true
      res
    }
    case res@Unavailable => res
  }

  override private[internal] def skip(): Unit = {
    producer.skip()
    stored = false
  }

  /** Creates a producer that can only produce the values
    * produced so far by `this` producer.
    */
  def createView(): Producer[A] = new Producer[A] {
    private var index = 0
    override private[internal] def peek(): Peek[A] = {
      if (index < buffer.size) {
        Available(buffer(index))
      }
      else if (ended) {
        Terminated
      }
      else {
        Unavailable
      }
    }
    override private[internal] def skip(): Unit = index += 1
  }
}

/** Producer that produces the values of an iterator. */
private class IteratorProducer[A](iterator: Iterator[A]) extends Producer[A] {
  private var cache: Option[Peek[A]] = None

  override private[internal] def peek(): Peek[A] = cache match {
    case Some(peeked) => peeked  // Cache hit.
    case None => {
      if (iterator.hasNext) {
        val value = iterator.next()
        val result = Available(value)
        cache = Some(result)
        result
      }
      else {
        cache = Some(Terminated)
        Terminated
      }
    }
  }

  override private[internal] def skip(): Unit = cache = None
}

/** Positively Totally Preordered Semigroup.
  *
  * Structure with a binary operation `append` and a relation `lessEquiv` which obey
  * the following laws:
  *
  *   - `append` is a semigroup operation, i.e.:
  *     - `append` is associative (`append(a, append(b, c)) == append(append(a, b), c)`).
  *   - `lessEquiv` is a total preorder, meaning that:
  *     - `lessEquiv` is reflexive (`lessEquiv(a, a) == true`).
  *     - `lessEquiv` is transitive (if `lessEquiv(a, b) == true` and `lessEquiv(b, c) == true`,
  *        then also `lessEquiv(a, c)) == true`).
  *     - `lessEquiv` is total, meaning that, for any `a` and `b`,
  *        either `lessEquiv(a, b) == true` or `lessEquiv(b, a) == true`.
  *   - `lessEquiv` is compatible with `append`, that is:
  *     - if `lessEquiv(a, b) == true`, then `lessEquiv(append(a, c), append(b, c)) == true`.
  *     - if `lessEquiv(a, b) == true`, then `lessEquiv(append(c, a), append(c, b)) == true`.
  *   - `append` is positive with respect to `lessEquiv`, that is:
  *     - `lessEquiv(a, append(a, b)) == true`.
  *     - `lessEquiv(b, append(a, b)) == true`.
  */
trait PTPS[A] {

  /** Checks if `left` is less or equivalent to `right`. */
  def lessEquiv(left: A, right: A): Boolean

  /** Combines `left` and `right` into a single value. */
  def append(left: A, right: A): A
}

/** Contains Positively Totally Preordered Semigroups (PTPSs) for various domains. */
object PTPS {

  /** Returns a PTPS for sequences.
    *
    * The relation `lessEquiv` compares sizes, while `append` concatenates.
    */
  def seqPTPS[A]: PTPS[Seq[A]] = new PTPS[Seq[A]] {
    override def lessEquiv(left: Seq[A], right: Seq[A]): Boolean =
      left.size <= right.size

    override def append(left: Seq[A], right: Seq[A]): Seq[A] =
      left ++ right
  }
}