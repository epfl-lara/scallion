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

trait Pipe[-A] {
  def feed(value: A): Unit
}

class TransformPipe[A, B](inner: Pipe[A], function: B => A) extends Pipe[B] {
  private var isFed: Boolean = false

  override def feed(value: B): Unit = if (!isFed) {
    isFed = true
    inner.feed(function(value))
  }
}

class MergePipe[A, B, C](inner: Pipe[C], merge: (A, B) => C) {

  private trait State
  private case object Fresh extends State
  private case class DoneLeft(value: A) extends State
  private case class DoneRight(value: B) extends State
  private case object Done extends State

  private var state: State = Fresh

  val left: Pipe[A] = new Pipe[A] {
    override def feed(value: A): Unit = state match {
      case Fresh =>
        state = DoneLeft(value)
      case DoneRight(other) =>
        state = Done
        inner.feed(merge(value, other))
      case _ => ()
    }
  }
  val right: Pipe[B] = new Pipe[B] {
    override def feed(value: B): Unit = state match {
      case Fresh =>
        state = DoneRight(value)
      case DoneLeft(other) =>
        state = Done
        inner.feed(merge(other, value))
      case _ => ()
    }
  }
}

class Point[A](val onComplete: Option[A] => Unit) extends Pipe[A] {
  private var state: Either[List[Pipe[A]], A] = Left(List())

  override def feed(value: A): Unit = state match {
    case Left(ps) =>
      state = Right(value)
      for (p <- ps) {
        p.feed(value)
      }
    case _ => ()
  }

  def register(pipe: Pipe[A]): Unit = state match {
    case Left(ps) =>
      state = Left(pipe +: ps)
    case Right(value) =>
      pipe.feed(value)
  }

  def get: Option[A] = state match {
    case Left(_) => None
    case Right(value) => Some(value)
  }

  def complete(): Unit = onComplete(get)
}