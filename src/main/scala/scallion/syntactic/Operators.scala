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

import scallion.syntactic.Unfolds._

/** Contains utilities to write syntaxes with infix, prefix and postfix operators.
  * Expected to be mixed-in to `Syntaxes`.
  *
  * @groupprio level 7
  * @groupname level Priority Levels
  * @groupdesc level Priority levels for the `operators` combinator.
  *
  * @groupprio assoc 8
  * @groupname assoc Associativity
  * @groupdesc assoc Associativity for priority levels.
  *
  * @group syntax
  */
trait Operators { self: Syntaxes[_, _] =>

  /** Associativity of an operator.
    *
    * @group assoc
    */
  sealed trait Associativity

  /** Left-associativity.
    *
    * @group assoc
    */
  case object LeftAssociative extends Associativity

  /** Right-associativity.
    *
    * @group assoc
    */
  case object RightAssociative extends Associativity

  /** Represents a precedence level with a syntax for the various `operator`s of that level
    * and an associativity.
    *
    * @group level
    */
  case class Level[Op](operator: Syntax[Op], associativity: Associativity)


  /** Implicitly decorates an `operator` syntax to add an `is` method
    * that indicates the associativity of the operator.
    *
    * {{{
    * val level = div | times is LeftAssociative
    * }}}
    *
    * @group level
    */
  implicit class LevelDecorator[Op](operator: Syntax[Op]) {

    /** Indicates the associativity of the operator. */
    def is(associativity: Associativity): Level[Op] = Level(operator, associativity)
  }

  /** Syntax that represents repetitions of `elem` separated by infix operators.
    *
    * The operators in earlier levels are considered to bind tighter than those in later levels.
    *
    * @param elem     Syntax for the operands.
    * @param function Function to apply an operation.
    * @param inverse  Function to reverse an operation.
    * @param levels   Operators (with associativity), in decreasing priority.
    *
    * @group combinator
    */
  def operators[Op, A](
      elem: Syntax[A],
      function: (A, Op, A) => A,
      inverse: PartialFunction[A, (A, Op, A)])
        (levels: Level[Op]*): Syntax[A] = {

    levels.foldLeft(elem) {
      case (acc, Level(op, assoc)) => assoc match {
        case LeftAssociative => infixLeft(acc, op, function, inverse)
        case RightAssociative => infixRight(acc, op, function, inverse)
      }
    }
  }

  /** Syntax that represents repetitions of `elem` separated by left-associative `op`.
    * The value returned is reduced left-to-right.
    *
    * @group combinator
    *
    * @param elem Syntax for the operands.
    * @param op   Syntax for the operators.
    * @param function Function to apply an operation.
    * @param inverse  Function to reverse an operation.
    */
  def infixLeft[Op, A](
      elem: Syntax[A],
      op: Syntax[Op],
      function: (A, Op, A) => A,
      inverse: PartialFunction[A, (A, Op, A)]): Syntax[A] =

    (elem ~ many(op ~ elem)).map({
      case first ~ opElems => opElems.foldLeft(first) {
        case (acc, (op ~ elem)) => function(acc, op, elem)
      }
    }, {
      case v => {
        val regrouped: PartialFunction[A, (A, Op ~ A)] = inverse andThen {
          case (a1, op, a2) => (a1, op ~ a2)
        }

        unfoldLeft(regrouped)(v)
      }
    })

  /** Syntax that represents repetitions of `elem` separated by right-associative `op`.
    * The value returned is reduced right-to-left.
    *
    * @param elem Syntax for the operands.
    * @param op   Syntax for the operators.
    * @param function Function to apply an operation.
    * @param inverse  Function to reverse an operation.    *
    * @group combinator
    */
  def infixRight[Op, A](
      elem: Syntax[A],
      op: Syntax[Op],
      function: (A, Op, A) => A,
      inverse: PartialFunction[A, (A, Op, A)]): Syntax[A] =

    (elem ~ many(op ~ elem)).map({
      case first ~ opElems => {
        val (ops, elems) = opElems.map(t => (t._1, t._2)).unzip
        val allElems = first +: elems
        val elemOps = allElems.zip(ops)
        elemOps.foldRight(allElems.last) {
          case ((elem, op), acc) => function(elem, op, acc)
        }
      }
    }, {
      case v => {
        val regrouped: PartialFunction[A, ((A, Op), A)] = inverse andThen {
          case (a1, op, a2) => ((a1, op), a2)
        }

        unfoldRight(regrouped)(v).map {
          case elemOps ~ last => {
            val (elems, ops) = elemOps.unzip

            val allElems = elems :+ last

            allElems.head ~ (ops.zip(allElems.tail).map {
              case (op, elem) => op ~ elem
            })
          }
        }
      }

    })

  /** Syntax that represents `elem` prefixed by any number of `op`.
    *
    * Operators are applied right-to-left.
    *
    * @param op       Syntax for the operators.
    * @param elem     Syntax for the operands.
    * @param function Function to apply an operation.
    * @param inverse  Function to reverse an operation.
    *
    * @group combinator
    */
  def prefixes[Op, A](
      op: Syntax[Op],
      elem: Syntax[A],
      function: (Op, A) => A,
      inverse: PartialFunction[A, (Op, A)] = PartialFunction.empty): Syntax[A] = {
    (many(op) ~ elem).map({
      case os ~ v => os.foldRight(v) {
        case (o, acc) => function(o, acc)
      }
    }, {
      case v => {
        unfoldRight(inverse)(v)
      }
    })
  }

  /** Syntax that represents `elem` postfixed by any number of `op`.
    *
    * Operators are applied left-to-right.
    *
    * @param elem     Syntax for the operands.
    * @param op       Syntax for the operators.
    * @param function Function to apply an operation.
    * @param inverse  Function to reverse an operation.
    *
    * @group combinator
    */
  def postfixes[Op, A](
      elem: Syntax[A],
      op: Syntax[Op],
      function: (A, Op) => A,
      inverse: PartialFunction[A, (A, Op)]): Syntax[A] = {
    (elem ~ many(op)).map({
      case v ~ os => os.foldLeft(v) {
        case (acc, o) => function(acc, o)
      }
    }, {
      case v => {
        unfoldLeft(inverse)(v)
      }
    })
  }
}