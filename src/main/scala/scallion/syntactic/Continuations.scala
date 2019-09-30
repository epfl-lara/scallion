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

  private sealed trait Continuation[A, B] {
    def apply(value: A): Either[B, Syntax[B]]
  }
  private case class ApplyFunction[A, B](function: A => B) extends Continuation[A, B] {
    override def apply(value: A): Either[B, Syntax[B]] = Left(function(value))
  }
  private case class PrependValue[A, B](first: A) extends Continuation[B, A ~ B] {
    override def apply(second: B): Either[A ~ B, Syntax[A ~ B]] = Left(first ~ second)
  }
  private case class FollowBy[A, B](syntax: Syntax[B]) extends Continuation[A, A ~ B] {
    override def apply(first: A): Either[A ~ B, Syntax[A ~ B]] = Right(epsilon(first) ~ syntax)
  }
  private case class ConcatPrependValues[A](first: Seq[A]) extends Continuation[Seq[A], Seq[A]] {
    override def apply(second: Seq[A]): Either[Seq[A], Syntax[Seq[A]]] = Left(first ++ second)
  }
  private case class ConcatFollowBy[A](syntax: Syntax[Seq[A]]) extends Continuation[Seq[A], Seq[A]] {
    override def apply(first: Seq[A]): Either[Seq[A], Syntax[Seq[A]]] = Right(epsilon(first) ++ syntax)
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

        @tailrec
        def go[B](current: ContinuedState[A, B]): Option[ContinuedState[A, _]] = {
          derive(current.syntax, token, kind, current.chain) match {
            case Left(res) => res match {
              case Some(value) if !current.chain.isEmpty =>
                go(foldStack(current.chain, value))
              case _ =>
                None
            }
            case Right(newState) => Some(newState)
          }
        }

        go(current) match {
          case None => return UnexpectedToken(token, new Continued(current))
          case Some(newState) => current = newState
        }
      }

      result(current) match {
        case Some(value) => Parsed(value, new Continued(current))
        case None => UnexpectedEnd(new Continued(current))
      }
    }

    @tailrec
    private def foldStack[B](chain: ContinuationChain[B, A], value: B): ContinuedState[A, _] = chain match {
      case _: EmptyChain[t] => ContinuedState[t, t](epsilon[t](value), EmptyChain())
      case ConsChain(cont: Continuation[_, t], rest) => cont(value) match {
        case Left(newValue) => foldStack(rest, newValue)
        case Right(syntax) => ContinuedState(syntax, rest)
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

    private def derive[C](
        syntax: Syntax[C],
        token: Token,
        kind: Kind,
        cs: ContinuationChain[C, A]): Either[Option[C], ContinuedState[A, _]] = {
      syntax match {
        case Success(value, _) => Left(Some(value))
        case Failure() => Left(None)
        case Elem(other) =>
          if (other == kind) Right(ContinuedState(epsilon(token), cs))
          else Left(None)
        case Transform(function, _, inner) =>
          derive(inner, token, kind, ApplyFunction(function) +: cs) match {
            case Left(res) => Left(res.map(function))
            case Right(state) => Right(state)
          }
        case Disjunction(left, right) =>
          if (left.first.contains(kind))
            derive(left, token, kind, cs)
          else if (right.first.contains(kind))
            derive(right, token, kind, cs)
          else {
            Left(left.nullable orElse right.nullable)
          }
        case Sequence(left: Syntax[ltype], right: Syntax[rtype]) =>
          left.nullable match {
            case Some(value) if !left.first.contains(kind) =>
              derive(right, token, kind, PrependValue[ltype, rtype](value) +: cs) match {
                case Left(res) => Left(res.map(x => value ~ x))
                case Right(state) => Right(state)
              }
            case _ =>
              derive(left, token, kind, FollowBy[ltype, rtype](right) +: cs) match {
                case Left(res) => Left(None)
                case Right(state) => Right(state)
              }
          }
        case Concat(left: Syntax[Seq[etype]], right) =>
          left.nullable match {
            case Some(values) if !left.first.contains(kind) =>
              derive(right, token, kind, ConcatPrependValues[etype](values) +: cs) match {
                case Left(res) => Left(res.map(x => values ++ x))
                case Right(state) => Right(state)
              }
            case _ =>
              derive(left, token, kind, ConcatFollowBy[etype](right) +: cs) match {
                case Left(res) => Left(None)
                case Right(state) => Right(state)
              }
          }
        case Recursive(_, inner) => {
          derive(inner, token, kind, cs)
        }
      }
    }
  }
  private case class ContinuedState[A, B](syntax: Syntax[B], chain: ContinuationChain[B, A])
}