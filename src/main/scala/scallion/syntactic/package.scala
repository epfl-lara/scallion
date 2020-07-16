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

/** This package is used to describe syntax for LL(1) languages.
  *
  * To use the package, mix-in the [[scallion.syntactic.Syntaxes]] trait.
  *
  * {{{
  * object MySyntaxes extends Syntaxes {
  *
  *   // Type of tokens.
  *   type Token = MyToken
  *
  *   // Type of token kinds.
  *   type Kind = MyKind
  *
  *   // Define the token kind of tokens.
  *   override def getKind(token: Token): Kind = ...
  *
  *   // Then define your syntax using combinators.
  *   lazy val mySyntax = ...
  * }
  * }}}
  *
  * @groupprio parsing 1
  * @groupname parsing Parsing
  *
  * @groupprio printing 2
  * @groupname printing Printing
  *
  * @groupprio syntax 3
  * @groupname syntax Syntax
  *
  * @groupprio result 4
  * @groupname result Parse Results
  *
  * @groupprio basic 5
  * @groupname basic Basic Syntaxes
  *
  * @groupprio combinator 6
  * @groupname combinator Combinators
  *
  * @groupprio property 8
  * @groupname property Properties
  *
  * @groupprio conflict 10
  * @groupname conflict LL(1) Conflicts
  *
  * @groupprio debug 15
  * @groupname debug Debugging
  *
  * @groupprio visualization 20
  * @groupname visualization Visualization
  *
  * @groupprio pairs 30
  * @groupname pairs Pairs
  *
  * @groupprio alias 50
  * @groupname alias Type Aliases
  *
  * @groupprio implicit 90
  * @groupname implicit Implicits
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

  /** Adds an `~` methods to build pairs.
    *
    * @group pairs
    */
  implicit class PairDecorator[A](first: A) {

    /** Builds a pair. */
    def ~[B](second: B): A ~ B = syntactic.~(first, second)
  }

  trait Parsers extends Syntaxes with Parsing with Enumeration with PrettyPrinting with Operators with Debug
}