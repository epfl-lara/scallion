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

package scallion.syntactic

import scallion.util.internal._

/** Provides enumeration capabilites to syntaxes. */
trait Enumeration { self: Syntaxes =>

  import Syntax._

  /** Enumerator. */
  object Enumerator {

    private val ops = new ProducerOps[Seq[Kind]](PTPS.seqPTPS[Kind])

    /** Enumerates the sequences accepted by a syntax.
      *
      * The sequences are produced lazily and in order of increasing length.
      */
    def enumerate[A](syntax: Syntax[A]): Iterator[Seq[Kind]] = producer(syntax).iterator

    private[scallion] def producer[A](syntax: Syntax[A]): Producer[Seq[Kind]] = {

      def go[A](syntax: Syntax[A], recs: Map[RecId, () => Producer[Seq[Kind]]]): Producer[Seq[Kind]] =
        syntax match {
          case Success(_, _) => Producer.single(Vector())
          case Failure() => Producer.empty
          case Elem(kind) => Producer.single(Vector(kind))
          case Disjunction(left, right) => ops.union(go(left, recs), go(right, recs))
          case Sequence(left, right) => ops.product(go(left, recs), go(right, recs))
          case Marked(_, inner) => go(inner, recs)
          case Transform(_, _, inner) => go(inner, recs)
          case Recursive(id, inner) => recs.get(id) match {
            case Some(function) => function()
            case None => {
              lazy val pair: (Producer[Seq[Kind]], () => Producer[Seq[Kind]]) =
                Producer.duplicate(Producer.lazily {
                  go(inner, recs + (id -> pair._2))
                })
              pair._1
            }
          }
        }

      go(syntax, Map())
    }
  }

  /** Sequence with holes.
   *
   * @tparam A The type of values.
   * @tparam H The type of holes.
   */
  class HoledSeq[A, H] private(val weight: Int, val values: Vector[Either[Vector[A], H]]) {

    /** Concatenates `this` sequence and `that` sequence. */
    def ++(that: HoledSeq[A, H]): HoledSeq[A, H] = {
      val newValues =
        if (this.values.nonEmpty && that.values.nonEmpty &&
            this.values.last.isLeft && that.values.head.isLeft) {
          this.values.init ++ (Left(this.values.last.left.get ++ that.values.head.left.get) +: that.values.tail)
        }
        else {
          this.values ++ that.values
        }
      new HoledSeq(this.weight + that.weight, newValues)
    }
  }

  /** Factory of sequences with holes. */
  object HoledSeq {

    /** Empty sequence. */
    def empty[A, H]: HoledSeq[A, H] =
      new HoledSeq[A, H](0, Vector())

    /** Sequence without holes. */
    def values[A, H](values: Vector[A]): HoledSeq[A, H] =
      new HoledSeq[A, H](values.size, Vector(Left(values)))

    /** Sequence with a single hole.
      *
      * @param hole   The hole value.
      * @param weight The weight of the hole in terms of number of values.
      */
    def hole[A, H](hole: H, weight: Int = 1): HoledSeq[A, H] =
      new HoledSeq[A, H](weight, Vector(Right(hole)))
  }

  private class HoledSeqPTPS[A, H] extends PTPS[HoledSeq[A, H]] {
    override def lessEquiv(left: HoledSeq[A, H], right: HoledSeq[A, H]): Boolean =
      left.weight <= right.weight
    override def append(left: HoledSeq[A, H], right: HoledSeq[A, H]): HoledSeq[A, H] =
      left ++ right
  }

  /** Enumerator which replaces kind sequences of marked syntaxes by holes. */
  object HoleEnumerator {

    /** Enumerates the sequences accepted by a syntax.
      *
      * The sequences are produced lazily and in order of increasing weight.
      *
      * @param syntax The syntax to enumerate.
      * @param holes  Function which indicates which marked syntaxes to replace by holes.
      * @param holeWeight Weight of holes in terms of number of kinds.
      */
    def enumerate[A, H](syntax: Syntax[A], holes: Mark => Option[H], holeWeight: Int = 1): Iterator[HoledSeq[Kind, H]] = {

      val ops = new ProducerOps[HoledSeq[Kind, H]](new HoledSeqPTPS[Kind, H])

      def go[A](syntax: Syntax[A], recs: Map[RecId, () => Producer[HoledSeq[Kind, H]]]): Producer[HoledSeq[Kind, H]] =
        syntax match {
          case Success(_, _) => Producer.single(HoledSeq.empty)
          case Failure() => Producer.empty
          case Elem(kind) => Producer.single(HoledSeq.values(Vector(kind)))
          case Disjunction(left, right) => ops.union(go(left, recs), go(right, recs))
          case Sequence(left, right) => ops.product(go(left, recs), go(right, recs))
          case Marked(mark, inner) => holes(mark) match {
            case Some(hole) => Producer.single(HoledSeq.hole(hole, holeWeight))
            case None => go(inner, recs)
          }
          case Transform(_, _, inner) => go(inner, recs)
          case Recursive(id, inner) => recs.get(id) match {
            case Some(function) => function()
            case None => {
              lazy val pair: (Producer[HoledSeq[Kind, H]], () => Producer[HoledSeq[Kind, H]]) =
                Producer.duplicate(Producer.lazily {
                  go(inner, recs + (id -> pair._2))
                })
              pair._1
            }
          }
        }

      go(syntax, Map()).iterator
    }
  }

  /** Enumerator which doesn't reenter recursive syntaxes. */
  object NonReentrantEnumerator {

    /** Enumerates the sequences accepted by a syntax.
      *
      * The sequences are produced lazily and in order of increasing length.
      */
    def enumerate[A](syntax: Syntax[A]): Iterator[HoledSeq[Kind, RecId]] = {

      val ops = new ProducerOps[HoledSeq[Kind, RecId]](new HoledSeqPTPS[Kind, RecId])

      def go[A](syntax: Syntax[A], recs: Set[RecId]): Producer[HoledSeq[Kind, RecId]] =
        syntax match {
          case Success(_, _) => Producer.single(HoledSeq.empty)
          case Failure() => Producer.empty
          case Elem(kind) => Producer.single(HoledSeq.values(Vector(kind)))
          case Disjunction(left, right) => ops.union(go(left, recs), go(right, recs))
          case Sequence(left, right) => ops.product(go(left, recs), go(right, recs))
          case Marked(mark, inner) => go(inner, recs)
          case Transform(_, _, inner) => go(inner, recs)
          case Recursive(id, inner) =>
            if (recs.contains(id)) {
              Producer.single(HoledSeq.hole(id))
            }
            else {
              go(inner, recs + id)
            }
        }

      go(syntax, Set()).iterator
    }
  }
}