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

package scallion

import scala.collection.mutable.Queue

import scallion.util.internal.enums._

/** Provides enumeration capabilites to syntaxes.
  *
  * @group enumeration
  */
trait Enumeration { self: Syntaxes with Parsing =>

  /** Factory of iterators over sequences accepted by a syntax.
    *
    * @group enumeration
    */
  object Enumerator {
    import Syntax._

    /** Returns an iterator that iterates over sequences accepted by a syntax.
      *
      * @param syntax       The syntax from which sequences are taken.
      * @param kindFunction Function to convert kinds to the desired type.
      * @param markFunction Partial function to converted marked syntaxes into a single value.
      *
      * @group enumeration
      */
    def apply[A](syntax: Syntax[_], kindFunction: Kind => A)
        (markFunction: PartialFunction[Mark, A]): Iterator[Iterator[A]] = {
      var recs: Map[Int, EnvEntry[A]] = Map()
      val probe: Probe = new Probe
      val markLifted: Mark => Option[A] = markFunction.lift

      def go(syntax: Syntax[_], subscriber: Tree[A] => Unit): Cell = {
        if (!syntax.isProductive) {
          EmptyCell
        }
        else {
          syntax match {
            case Success(_) => EmptyCell
            case Failure() => EmptyCell
            case Elem(kind) => new ElemCell(kindFunction(kind), subscriber)
            case Disjunction(left, right) => {
              val res = new DisjunctionCell(subscriber)
              val l = go(left, (tree: Tree[A]) => res.informLeft(tree))
              val r = go(right, (tree: Tree[A]) => res.informRight(tree))
              res.setLeftCell(l)
              res.setRightCell(r)
              res
            }
            case Sequence(left, right) => {
              val res = new SequenceCell(left.isNullable, right.isNullable, probe, subscriber)
              val l = go(left, (tree: Tree[A]) => res.informLeft(tree))
              val r = go(right, (tree: Tree[A]) => res.informRight(tree))
              res.setLeftCell(l)
              res.setRightCell(r)
              res
            }
            case Transform(_, _, inner) => {
              go(inner, subscriber)
            }
            case Marked(mark, inner) => {
              markLifted(mark) match {
                case None => go(inner, subscriber)
                case Some(elem) => new ElemCell(elem, subscriber)
              }
            }
            case Recursive(id, inner) => recs.get(id) match {
              case Some(entry) => {
                entry.addVarCell(subscriber)
              }
              case None => {
                val entry = new EnvEntry[A](probe)
                recs += id -> entry
                val i = go(inner, (tree: Tree[A]) => entry.inform(tree))
                entry.setInner(i)
                entry.addVarCell(subscriber)
              }
            }
          }
        }
      }

      val queue: Queue[Tree[A]] = new Queue()
      val receive: Tree[A] => Unit = (tree: Tree[A]) => {
        queue.enqueue(tree)
      }
      val cell = go(syntax, receive)


      val it = new Runner[A](cell, queue, probe)
      if (syntax.isNullable) {
        Iterator(Empty.values) ++ it
      }
      else {
        it
      }
    }

    /** Returns an iterator that iterates over sequences of kinds accepted by a syntax.
      *
      * @param syntax       The syntax from which sequences are taken.
      *
      * @group enumeration
      */
    def apply(syntax: Syntax[_]): Iterator[Iterator[Kind]] = {
      apply(syntax, (kind: Kind) => kind)(PartialFunction.empty[Mark, Kind])
    }
  }
}