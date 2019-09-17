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


trait Cell[A] extends (A => Unit) {
  def register(action: A => Unit): Unit
  def complete(): Unit
}


class TransformOnce[A, B](callback: B => Unit, function: A => B) extends (A => Unit) {
  private var applied: Boolean = false

  override def apply(value: A): Unit = if (!applied) {
    applied = true
    callback(function(value))
  }
}


class MergeOnce[A, B](callback: (A, B) => Unit) {

  private trait State
  private case object Fresh extends State
  private case class DoneLeft(value: A) extends State
  private case class DoneRight(value: B) extends State
  private case object Done extends State

  private var state: State = Fresh

  val left: A => Unit = { (value: A) =>
    state match {
      case Fresh =>
        state = DoneLeft(value)
      case DoneRight(other) =>
        state = Done
        callback(value, other)
      case _ => ()
    }
  }

  val right: B => Unit = { (value: B) =>
    state match {
      case Fresh =>
        state = DoneRight(value)
      case DoneLeft(other) =>
        state = Done
        callback(other, value)
      case _ => ()
    }
  }
}


class CellOnce[A](onComplete: Option[A] => Unit) extends Cell[A] {
  private var state: Either[List[A => Unit], A] = Left(List())

  override def apply(value: A): Unit = state match {
    case Left(subscribers) =>
      state = Right(value)
      for (subscriber <- subscribers) {
        subscriber(value)
      }
    case _ => ()
  }

  override def register(subscriber: A => Unit): Unit = state match {
    case Left(subscribers) =>
      state = Left(subscriber +: subscribers)
    case Right(value) =>
      subscriber(value)
  }

  override def complete(): Unit = onComplete(state match {
    case Left(_) => None
    case Right(value) => Some(value)
  })
}


class CellUpgradableSet[A](onComplete: Set[A] => Unit) extends Cell[Set[A]] {
  private var subscribers: List[Set[A] => Unit] = List()
  private var state: Set[A] = Set()

  override def apply(value: Set[A]): Unit = {
    val diff = value -- state

    if (diff.nonEmpty) {
      state = state union diff
      subscribers.foreach(_.apply(diff))
    }
  }

  override def register(subscriber: Set[A] => Unit): Unit = {
    subscribers = subscriber +: subscribers

    if (state.nonEmpty) {
      subscriber(state)
    }
  }

  override def complete(): Unit = onComplete(state)
}