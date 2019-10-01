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

package scallion.syntactic

import scala.annotation.tailrec

trait Focuses[Token, Kind] { self: Syntaxes[Token, Kind] =>

  import Syntax._

  private sealed trait HoledSyntax[A, B] {
    def +:[C](that: Layer[C, A]): HoledSyntax[C, B] =
      Layered(that, this)

    def isEmpty: Boolean
  }
  private case class Empty[A]() extends HoledSyntax[A, A] {
    override def toString: String = "Empty"
    override def isEmpty = true
  }
  private case class Layered[A, B, C](
    head: Layer[A, B],
    tail: HoledSyntax[B, C]) extends HoledSyntax[A, C] {
    override def toString: String = head.toString + " ::: " + tail.toString
    override def isEmpty = false
  }

  private case class LayeredSyntax[A, B](syntax: Syntax[A], layer: Layer[A, B])

  private sealed trait Layer[A, B] {
    def apply(value: A): Either[B, LayeredSyntax[_, B]]
    def apply(syntax: Syntax[A]): Syntax[B]
  }
  private case class ApplyFunction[A, B](function: A => B, inverse: B => Seq[A]) extends Layer[A, B] {
    override def apply(value: A): Either[B, LayeredSyntax[_, B]] = Left(function(value))
    override def apply(syntax: Syntax[A]): Syntax[B] = syntax.map(function, inverse)
  }
  private case class PrependValue[A, B](first: A) extends Layer[B, A ~ B] {
    override def apply(second: B): Either[A ~ B, LayeredSyntax[_, A ~ B]] = Left(first ~ second)
    override def apply(syntax: Syntax[B]): Syntax[A ~ B] = epsilon(first) ~ syntax
  }
  private case class FollowBy[A, B](second: Syntax[B]) extends Layer[A, A ~ B] {
    override def apply(first: A): Either[A ~ B, LayeredSyntax[_, A ~ B]] =
      Right(LayeredSyntax(second, PrependValue(first)))
    override def apply(syntax: Syntax[A]): Syntax[A ~ B] = syntax ~ second
  }
  private case class ConcatPrependValues[A](first: Seq[A]) extends Layer[Seq[A], Seq[A]] {
    override def apply(second: Seq[A]): Either[Seq[A], LayeredSyntax[_, Seq[A]]] = Left(first ++ second)
    override def apply(syntax: Syntax[Seq[A]]): Syntax[Seq[A]] = epsilon(first) ++ syntax
  }
  private case class ConcatFollowBy[A](second: Syntax[Seq[A]]) extends Layer[Seq[A], Seq[A]] {
    override def apply(first: Seq[A]): Either[Seq[A], LayeredSyntax[_, Seq[A]]] =
      Right(LayeredSyntax(second, ConcatPrependValues(first)))
    override def apply(syntax: Syntax[Seq[A]]): Syntax[Seq[A]] = syntax ++ second
  }

  object Focused {
    def apply[A](syntax: Syntax[A]): Focused[A] = new Focused(FocusedState(syntax, Empty()))
  }

  class Focused[A] private (state: FocusedState[A, _]) {

    def toSyntax: Syntax[A] = state.toSyntax

    def apply(tokens: Iterator[Token]): ParseResult[A] = {
      var current: FocusedState[A, _] = state

      while (tokens.hasNext) {
        val token = tokens.next()
        val kind = getKind(token)

        findFirst(current, kind) match {
          case None =>
            return UnexpectedToken(token, new Focused(current))
          case Some(toDerive: FocusedState[_, t]) =>
            current = foldStack(derive[t, A](toDerive.syntax, kind, toDerive.hole), token)
        }
      }

      result(current) match {
        case Some(value) => Parsed(value, new Focused(current))
        case None => UnexpectedEnd(new Focused(current))
      }
    }

    @tailrec
    private def findFirst[B](state: FocusedState[A, B], kind: Kind): Option[FocusedState[A, _]] = {
      if (state.syntax.first.contains(kind)) Some(state)
      else if (state.hole.isEmpty) None
      else state.syntax.nullable match {
        case None => None
        case Some(value) => findFirst(foldStack(state.hole, value), kind)
      }
    }

    @tailrec
    private def foldStack[B](hole: HoledSyntax[B, A], value: B): FocusedState[A, _] = hole match {
      case _: Empty[t] => FocusedState[t, t](epsilon[t](value), Empty())
      case Layered(layer: Layer[_, t], rest) => layer(value) match {
        case Left(newValue) => foldStack(rest, newValue)
        case Right(LayeredSyntax(syntax, layer)) => FocusedState(syntax, layer +: rest)
      }
    }

    private def result[C](current: FocusedState[A, C]): Option[A] = {

      @tailrec
      def go[B](syntax: Syntax[B], hole: HoledSyntax[B, A]): Option[A] = syntax.nullable match {
        case None => None
        case Some(value) => hole match {
          case _: Empty[t] => Some[t](value)
          case _ => foldStack(hole, value) match {
            case FocusedState(syntax: Syntax[t], rest) =>
              go[t](syntax, rest)
          }
        }
      }

      go[C](current.syntax, current.hole)
    }

    @tailrec
    private def derive[C, X](
        syntax: Syntax[C],
        kind: Kind,
        cs: HoledSyntax[C, X]): HoledSyntax[Token, X] =
      syntax match {
        case Elem(_) =>
          cs
        case Transform(function, inverse, inner) =>
          derive(inner, kind, ApplyFunction(function, inverse) +: cs)
        case Disjunction(left, right) =>
          if (left.first.contains(kind))
            derive(left, kind, cs)
          else
            derive(right, kind, cs)
        case Sequence(left: Syntax[ltype], right: Syntax[rtype]) =>
          if (left.first.contains(kind))
            derive(left, kind, FollowBy[ltype, rtype](right) +: cs)
          else
            derive(right, kind, PrependValue[ltype, rtype](left.nullable.get) +: cs)
        case Concat(left: Syntax[Seq[etype]], right) =>
          if (left.first.contains(kind))
            derive(left, kind, ConcatFollowBy(right) +: cs)
          else
            derive(right, kind, ConcatPrependValues[etype](left.nullable.get) +: cs)
        case Recursive(_, inner) =>
          derive(inner, kind, cs)
        case _ => throw new IllegalArgumentException("Unexpected syntax.")
      }
  }
  private case class FocusedState[A, B](syntax: Syntax[B], hole: HoledSyntax[B, A]) {

    def toSyntax: Syntax[A] = {

      @tailrec def go[C](syntax: Syntax[C], hole: HoledSyntax[C, A]): Syntax[A] = hole match {
        case _: Empty[t] => syntax
        case Layered(layer, rest) => go(layer(syntax), rest)
      }

      go(syntax, hole)
    }
  }
}