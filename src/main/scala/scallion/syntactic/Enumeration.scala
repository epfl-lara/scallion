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

import scala.collection.mutable.HashMap

import scallion.util.internal._

/** Provides enumeration capabilites to syntaxes. */
trait Enumeration { self: Syntaxes =>

  import Syntax._

  /** Enumerator object. */
  object Enumerator {

    private val ops = new ProducerOps[Seq[Kind]](PTPS.seqPTPS[Kind])

    /** Enumerates the sequences accepted by a syntax.
      *
      * The sequences are produced lazily and in order of increasing length.
      */
    def apply[A](syntax: Syntax[A]): Iterator[Seq[Kind]] = {

      val recs: HashMap[RecId, () => Producer[Seq[Kind]]] = new HashMap()

      def go[A](syntax: Syntax[A]): Producer[Seq[Kind]] = syntax match {
        case Success(_, _) => Producer.single(Vector())
        case Failure() => Producer.empty
        case Elem(kind) => Producer.single(Vector(kind))
        case Disjunction(left, right) => ops.union(go(left), go(right))
        case Sequence(left, right) => ops.product(go(left), go(right))
        case Transform(_, _, inner) => go(inner)
        case Recursive(id, inner) => recs.get(id) match {
          case Some(function) => function()
          case None => {
            val (producer, cloningFunction) =
              Producer.duplicate(Producer.lazily {
                go(inner)
              })
            recs += id -> cloningFunction
            producer
          }
        }
      }

      go(syntax).iterator
    }
  }
}