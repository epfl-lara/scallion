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

trait Continuations[Token, Kind] { self: Syntaxes[Token, Kind] =>

  import Syntax._

  private sealed trait ContinuationChain[A, B] {
    def +:[C](that: Continuation[C, A]): ContinuationChain[C, B] =
      ConsChain(that, this)

    def isEmpty: Boolean
  }
  private case class EmptyChain[A]() extends ContinuationChain[A, A] {
    override def toString: String = "Empty"
    override def isEmpty = true
  }
  private case class ConsChain[A, B, C](
    head: Continuation[A, B],
    tail: ContinuationChain[B, C]) extends ContinuationChain[A, C] {
    override def toString: String = head.toString + " ::: " + tail.toString
    override def isEmpty = false
  }

  private case class SyntaxCont[A, B](syntax: Syntax[A], cont: Continuation[A, B])

  private sealed trait Continuation[A, B] {
    def apply(value: A): Either[B, SyntaxCont[_, B]]
  }
  private case class ApplyFunction[A, B](function: A => B) extends Continuation[A, B] {
    override def apply(value: A): Either[B, SyntaxCont[_, B]] = Left(function(value))
  }
  private case class PrependValue[A, B](first: A) extends Continuation[B, A ~ B] {
    override def apply(second: B): Either[A ~ B, SyntaxCont[_, A ~ B]] = Left(first ~ second)
  }
  private case class FollowBy[A, B](syntax: Syntax[B]) extends Continuation[A, A ~ B] {
    override def apply(first: A): Either[A ~ B, SyntaxCont[_, A ~ B]] = Right(SyntaxCont(syntax, PrependValue(first)))
  }
  private case class ConcatPrependValues[A](first: Seq[A]) extends Continuation[Seq[A], Seq[A]] {
    override def apply(second: Seq[A]): Either[Seq[A], SyntaxCont[_, Seq[A]]] = Left(first ++ second)
  }
  private case class ConcatFollowBy[A](syntax: Syntax[Seq[A]]) extends Continuation[Seq[A], Seq[A]] {
    override def apply(first: Seq[A]): Either[Seq[A], SyntaxCont[_, Seq[A]]] = Right(SyntaxCont(syntax, ConcatPrependValues(first)))
  }

  object Continued {
    def apply[A](syntax: Syntax[A]): Continued[A] = new Continued(ContinuedState(syntax, EmptyChain()))
  }

  class Continued[A] private (state: ContinuedState[A, _]) extends Parser[Continued, A] {
    def syntax: Syntax[_] = state.syntax
    def conts: Any = state.chain

    override def apply(tokens: Iterator[Token]): ParseResult[Continued, A] = {
      var current: ContinuedState[A, _] = state

      while (tokens.hasNext) {
        val token = tokens.next()
        val kind = getKind(token)

        findFirst(current, kind) match {
          case None =>
            return UnexpectedToken(token, new Continued(current))
          case Some(toDerive: ContinuedState[_, t]) =>
            current = foldStack(derive[t](toDerive.syntax, token, kind, toDerive.chain), token)
        }
      }

      result(current) match {
        case Some(value) => Parsed(value, new Continued(current))
        case None => UnexpectedEnd(new Continued(current))
      }
    }

    @tailrec
    private def findFirst[B](state: ContinuedState[A, B], kind: Kind): Option[ContinuedState[A, _]] = {
      if (state.syntax.first.contains(kind)) Some(state)
      else if (state.chain.isEmpty) None
      else state.syntax.nullable match {
        case None => None
        case Some(value) => findFirst(foldStack(state.chain, value), kind)
      }
    }

    @tailrec
    private def foldStack[B](chain: ContinuationChain[B, A], value: B): ContinuedState[A, _] = chain match {
      case _: EmptyChain[t] => ContinuedState[t, t](epsilon[t](value), EmptyChain())
      case ConsChain(cont: Continuation[_, t], rest) => cont(value) match {
        case Left(newValue) => foldStack(rest, newValue)
        case Right(SyntaxCont(syntax, cont)) => ContinuedState(syntax, cont +: rest)
      }
    }

    private def result[C](current: ContinuedState[A, C]): Option[A] = {

      @tailrec
      def go[B](syntax: Syntax[B], chain: ContinuationChain[B, A]): Option[A] = syntax.nullable match {
        case None => None
        case Some(value) => chain match {
          case _: EmptyChain[t] => Some[t](value)
          case _ => foldStack(chain, value) match {
            case ContinuedState(syntax: Syntax[t], rest) =>
              go[t](syntax, rest)
          }
        }
      }

      go[C](current.syntax, current.chain)
    }

    @tailrec
    private def derive[C](
        syntax: Syntax[C],
        token: Token,
        kind: Kind,
        cs: ContinuationChain[C, A]): ContinuationChain[Token, A] =
      syntax match {
        case Elem(_) =>
          cs
        case Transform(function, _, inner) =>
          derive(inner, token, kind, ApplyFunction(function) +: cs)
        case Disjunction(left, right) =>
          if (left.first.contains(kind))
            derive(left, token, kind, cs)
          else
            derive(right, token, kind, cs)
        case Sequence(left: Syntax[ltype], right: Syntax[rtype]) =>
          if (left.first.contains(kind))
            derive(left, token, kind, FollowBy[ltype, rtype](right) +: cs)
          else
            derive(right, token, kind, PrependValue[ltype, rtype](left.nullable.get) +: cs)
        case Concat(left: Syntax[Seq[etype]], right) =>
          if (left.first.contains(kind))
            derive(left, token, kind, ConcatFollowBy(right) +: cs)
          else
            derive(right, token, kind, ConcatPrependValues[etype](left.nullable.get) +: cs)
        case Recursive(_, inner) =>
          derive(inner, token, kind, cs)
        case _ => throw new IllegalArgumentException("Unexpected syntax.")
      }
  }
  private case class ContinuedState[A, B](syntax: Syntax[B], chain: ContinuationChain[B, A])
}