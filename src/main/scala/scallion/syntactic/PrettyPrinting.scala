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

/** Provides pretty printing capabilites to syntaxes. */
trait PrettyPrinting { self: Syntaxes =>

  import Syntax._

  /** Pretty printer. */
  object PrettyPrinter {

    private val ops = new ProducerOps[Seq[Token]](PTPS.seqPTPS[Token])

    /** Enumerates the sequences of tokens that describe a given value.
      *
      * The sequences are produced lazily and in order of increasing length.
      */
    def apply[A](syntax: Syntax[A], value: A): Iterator[Seq[Token]] = {

      val recs: HashMap[(RecId, Any), () => Producer[Seq[Token]]] = new HashMap()

      def go[A](syntax: Syntax[A], value: A): Producer[Seq[Token]] = syntax match {
        case Success(_, matches) => Producer.fromIterator(Iterator.fill(matches(value))(Vector()))
        case Failure() => Producer.empty
        case Elem(kind) => if (getKind(value) == kind) Producer.single(Vector(value)) else Producer.empty
        case Disjunction(left, right) => ops.union(go(left, value), go(right, value))
        case Sequence(left, right) => ops.product(go(left, value._1), go(right, value._2))
        case Transform(_, inverse, inner) => {
          val producers = inverse(value).map(go(inner, _))

          if (producers.isEmpty) {
            Producer.empty
          }
          else {
            producers.reduceLeft(ops.union(_, _))
          }
        }
        case Recursive(id, inner) => recs.get((id, value)) match {
          case Some(function) => function()
          case None => {
            val (producer, cloningFunction) =
              Producer.duplicate(Producer.lazily {
                go(inner, value)
              })
            recs += (id, value) -> cloningFunction
            producer
          }
        }
      }

      go(syntax, value).iterator
    }
  }

}