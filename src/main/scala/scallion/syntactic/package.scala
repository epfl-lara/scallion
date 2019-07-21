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

package scallion

/** This package is used to write LL(1) parsers.
  *
  * To use the package, mix-in the [[scallion.parsing.Parsers]] trait.
  *
  * {{{
  * object MyParsers extends Parsers[Token, Kind] {
  *
  *   // Define the token kind of tokens.
  *   override def getKind(token: Token): Kind = ...
  *
  *   // Then define your parsers using combinators.
  *   lazy val myParser = ...
  * }
  * }}}
  *
  * @groupprio abstract 0
  * @groupname abstract Abstract Members
  *
  * @groupprio parser 1
  * @groupname parser Parser
  *
  * @groupprio result 2
  * @groupname result Parse Results
  *
  * @groupprio basic 3
  * @groupname basic Basic Parsers
  *
  * @groupprio combinator 4
  * @groupname combinator Combinators
  *
  * @groupprio conflict 10
  * @groupname conflict LL(1) Conflicts
  *
  * @groupprio visualization 20
  * @groupname visualization Visualization
  *
  * @groupprio pairs 30
  * @groupname pairs Pairs
  *
  * @groupprio other 100
  * @groupname other Others
  */
package object syntactic {

  /** Simply a pair.
    *
    * Can be used in infix position in pattern matching.
    *
    * @param _1 First element.
    * @param _2 Second element.
    *
    * @group pairs
    */
  case class ~[+A, +B](_1: A, _2: B) {

    /* Builds a pair. */
    def ~[C](next: C): (A ~ B) ~ C = syntactic.~(this, next)
  }

  /** Adds an `~` methods to build pairs. */
  implicit class PairDecorator[A](first: A) {

    /** Builds a pair. */
    def ~[B](second: B): A ~ B = syntactic.~(first, second)
  }
}