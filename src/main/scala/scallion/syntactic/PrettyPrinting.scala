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

/** Provides pretty printing capabilites to syntaxes. */
trait PrettyPrinting { self: Syntaxes =>

  import Syntax._

  /** Pretty printer. */
  class PrettyPrinter[A] private(syntax: Syntax[A]) {

    private val ops = new ProducerOps[Seq[Token]](PTPS.seqPTPS[Token])

    /** Enumerates the sequences of tokens that describe a given value.
      *
      * The sequences are produced lazily and in order of increasing length.
      */
    def apply(value: A): Iterator[Seq[Token]] = {

      def go[A](syntax: Syntax[A],
                value: A,
                recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]] =
        syntax match {
          case Success(_, matches) => Producer.fromIterator(Iterator.fill(matches(value))(Vector()))
          case Failure() => Producer.empty
          case Elem(kind) => if (getKind(value) == kind) Producer.single(Vector(value)) else Producer.empty
          case Disjunction(left, right) => ops.union(go(left, value, recs), go(right, value, recs))
          case Sequence(left, right) => ops.product(go(left, value._1, recs), go(right, value._2, recs))
          case Marked(_, inner) => go(inner, value, recs)
          case Transform(_, inverse, inner) => {
            val producers = inverse(value).map(go(inner, _, recs))

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
              lazy val pair: (Producer[Seq[Token]], () => Producer[Seq[Token]]) =
                Producer.duplicate(Producer.lazily {
                  go(inner, value, recs + ((id, value) -> pair._2))
                })
              pair._1
            }
          }
        }

      go(syntax, value, Map()).iterator
    }
  }

  /** Pretty printer factory. */
  object PrettyPrinter {
    def apply[A](syntax: Syntax[A]): PrettyPrinter[A] = new PrettyPrinter(syntax)
  }
}