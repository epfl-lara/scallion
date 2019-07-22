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
  case class Level[-V, A](operator: Syntax[V, (A, A) => A], associativity: Associativity)


  /** Implicitly decorates an `operator` syntax to add an `is` method
    * that indicates the associativity of the operator.
    *
    * {{{
    * val level = div | times is LeftAssociative
    * }}}
    *
    * @group level
    */
  implicit class LevelDecorator[-V, A](operator: Syntax[V, (A, A) => A]) {

    /** Indicates the associativity of the operator. */
    def is(associativity: Associativity): Level[V, A] = Level(operator, associativity)
  }

  /** Syntax that represents repetitions of `elem` separated by infix operators.
    *
    * The operators in earlier levels are considered to bind tighter than those in later levels.
    *
    * @param elem   Syntax for the operands.
    * @param rev    Function to reverse an operation.
    * @param levels Operators (with associativity), in decreasing priority.
    *
    * @group combinator
    */
  def operators[V, W, A](elem: Syntax[V, A], rev: PartialFunction[V, (V, W, V)] = PartialFunction.empty)
                        (levels: Level[W, A]*): Syntax[V, A] = {

    levels.foldLeft(elem) {
      case (acc, Level(op, assoc)) => assoc match {
        case LeftAssociative => infixLeft(acc, op, rev)
        case RightAssociative => infixRight(acc, op, rev)
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
    * @param rev  Function to reverse an operation.
    */
  def infixLeft[V, W, A](
      elem: Syntax[V, A],
      op: Syntax[W, (A, A) => A],
      rev: PartialFunction[V, (V, W, V)] = PartialFunction.empty): Syntax[V, A] =

    (elem ~ many(op ~ elem)).bimap({
      case first ~ opElems => opElems.foldLeft(first) {
        case (acc, (op ~ elem)) => op(acc, elem)
      }
    }, {
      case v => {
        val regrouped: PartialFunction[V, (V, W ~ V)] = rev andThen {
          case (v1, w, v2) => (v1, w ~ v2)
        }

        unfoldLeft(regrouped)(v)
      }
    })

  /** Syntax that represents repetitions of `elem` separated by right-associative `op`.
    * The value returned is reduced right-to-left.
    *
    * @param elem Syntax for the operands.
    * @param op   Syntax for the operators.
    * @param rev  Function to reverse an operation.
    *
    * @group combinator
    */
  def infixRight[V, W, A](
      elem: Syntax[V, A],
      op: Syntax[W, (A, A) => A],
      rev: PartialFunction[V, (V, W, V)] = PartialFunction.empty): Syntax[V, A] =

    (elem ~ many(op ~ elem)).bimap({
      case first ~ opElems => {
        val (ops, elems) = opElems.map(t => (t._1, t._2)).unzip
        val allElems = first +: elems
        val elemOps = allElems.zip(ops)
        elemOps.foldRight(allElems.last) {
          case ((elem, op), acc) => op(elem, acc)
        }
      }
    }, {
      case v => {
        val regrouped: PartialFunction[V, ((V, W), V)] = rev andThen {
          case (v1, w, v2) => ((v1, w), v2)
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
    * @param op   Syntax for the operators.
    * @param elem Syntax for the operands.
    * @param rev  Function to reverse an operation.
    *
    * @group combinator
    */
  def prefixes[V, W, A](
      op: Syntax[W, A => A],
      elem: Syntax[V, A],
      rev: PartialFunction[V, (W, V)] = PartialFunction.empty): Syntax[V, A] = {
    (many(op) ~ elem).bimap({
      case os ~ v => os.foldRight(v) {
        case (o, acc) => o(acc)
      }
    }, {
      case v => {
        unfoldRight(rev)(v)
      }
    })
  }

  /** Syntax that represents `elem` postfixed by any number of `op`.
    *
    * Operators are applied left-to-right.
    *
    * @param elem Syntax for the operands.
    * @param op   Syntax for the operators.
    * @param rev  Function to reverse an operation.
    *
    * @group combinator
    */
  def postfixes[V, W, A](elem: Syntax[V, A], op: Syntax[W, A => A], rev: PartialFunction[V, (V, W)]): Syntax[V, A] = {
    (elem ~ many(op)).bimap({
      case v ~ os => os.foldLeft(v) {
        case (acc, o) => o(acc)
      }
    }, {
      case v => {
        unfoldLeft(rev)(v)
      }
    })
  }
}