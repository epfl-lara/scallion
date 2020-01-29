/* Copyright 2020 EPFL, Lausanne
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

package scallion.util.internal.propagators

trait Cell[-I, +O, +S] extends (I => Unit) { self =>
  def apply(value: I): Unit
  def register(callback: O => Unit): Unit
  def get: S

  def map[P](function: O => P): Cell[I, P, S] = new Cell[I, P, S] {
    override def apply(value: I): Unit = self.apply(value)
    override def register(callback: P => Unit): Unit = self.register(function andThen callback)
    override def get: S = self.get
  }

  def contramap[J](function: J => I): Cell[J, O, S] = new Cell[J, O, S] {
    override def apply(value: J): Unit = self.apply(function(value))
    override def register(callback: O => Unit): Unit = self.register(callback)
    override def get: S = self.get
  }
}

class BooleanCell extends Cell[Unit, Unit, Boolean] {
  private var registered: List[Unit => Unit] = List()

  private var state: Boolean = false

  override def apply(value: Unit): Unit = {
    if (!state) {
      state = true
      registered.foreach(_.apply(()))
      registered = List()
    }
  }

  override def register(callback: Unit => Unit): Unit = {
    if (state) {
      callback(())
    }
    else {
      registered = callback :: registered
    }
  }

  override def get: Boolean = state
}

class OptionCell[A] extends Cell[A, A, Option[A]] {
  private var registered: List[A => Unit] = List()

  private var state: Option[A] = None

  override def apply(value: A): Unit = {
    if (state.isEmpty) {
      state = Some(value)
      registered.foreach(_.apply(value))
      registered = List()
    }
  }

  override def register(callback: A => Unit): Unit = {
    if (state.nonEmpty) {
      callback(state.get)
    }
    else {
      registered = callback :: registered
    }
  }

  override def get: Option[A] = state
}

class SetCell[A] extends Cell[Set[A], Set[A], Set[A]] {
  private var registered: List[Set[A] => Unit] = List()

  private var state: Set[A] = Set.empty

  override def apply(value: Set[A]): Unit = {
    val diff = value -- state

    if (diff.nonEmpty) {
      state = state union diff
      registered.foreach(_.apply(diff))
    }
  }

  override def register(callback: Set[A] => Unit): Unit = {
    registered = callback :: registered
    if (state.nonEmpty) {
      callback(state)
    }
  }

  override def get: Set[A] = state
}

class MergeOnceCell[A, B, C](merge: (A, B) => C) extends Cell[Either[A, B], C, Option[C]] {
  private var registered: List[C => Unit] = List()

  private var fromLeft: Option[A] = None
  private var fromRight: Option[B] = None
  private var merged: Option[C] = None

  override def apply(value: Either[A, B]): Unit = {

    value match {
      case Left(value) => fromLeft = Some(value)
      case Right(value) => fromRight = Some(value)
    }

    merged = for {
      l <- fromLeft
      r <- fromRight
    } yield merge(l, r)

    merged.foreach { (mergedValue: C) =>
      registered.foreach(_.apply(mergedValue))
    }
  }

  override def register(callback: C => Unit): Unit = {
    if (merged.isEmpty) {
      registered = callback :: registered
    }
    else {
      callback(merged.get)
    }
  }

  override def get: Option[C] = merged
}

class GatedCell[A] extends Cell[Option[A], A, List[A]] {
  private var registered: List[A => Unit] = List()

  private var values: List[A] = List()
  private var active: Boolean = false

  override def apply(value: Option[A]): Unit =
    value match {
      case Some(value) => {
        values = value :: values
        if (active) {
          registered.foreach(_.apply(value))
        }
      }
      case None => {
        if (!active) {
          active = true
          values.foreach { (value: A) =>
            registered.foreach(_.apply(value))
          }
        }
      }
    }

  override def register(callback: A => Unit): Unit = {
    registered = callback :: registered

    if (active) {
      values.foreach { (value: A) =>
        callback(value)
      }
    }
  }

  override def get: List[A] = values
}