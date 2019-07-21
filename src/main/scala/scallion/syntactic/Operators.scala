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

/** Contains utilities to write parsers with infix, prefix and postfix operators.
  * Expected to be mixed-in to `Parsers`.
  *
  * @groupprio level 7
  * @groupname level Priority Levels
  * @groupdesc level Priority levels for the `operators` combinator.
  *
  * @groupprio assoc 8
  * @groupname assoc Associativity
  * @groupdesc assoc Associativity for priority levels.
  *
  * @group parsing
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

  /** Represents a precedence level with a parser for the various `operator`s of that level
    * and an associativity.
    *
    * @group level
    */
  case class Level[-V, A](operator: Parser[V, (A, A) => A], associativity: Associativity)


  /** Implicitly decorates an `operator` parser to add an `is` methods
    * that indicates the associativity of the parser.
    *
    * {{{
    * val level = div | times is LeftAssociative
    * }}}
    *
    * @group level
    */
  implicit class LevelDecorator[-V, A](operator: Parser[V, (A, A) => A]) {

    /** Indicates the associativity of the operator. */
    def is(associativity: Associativity): Level[V, A] = Level(operator, associativity)
  }

  /** Parser that parses repetitions of `elem` separated by infix operators.
    *
    * The operators in earlier levels are considered to bind tighter than those in later levels.
    *
    * @group combinator
    */
  def operators[V, W, A](elem: Parser[V, A], rev: PartialFunction[V, (V, W, V)])
                        (levels: Level[W, A]*): Parser[V, A] = {

    levels.foldLeft(elem) {
      case (acc, Level(op, assoc)) => assoc match {
        case LeftAssociative => infixLeft(acc, op, rev)
        case RightAssociative => infixRight(acc, op, rev)
      }
    }
  }

  /** Parser that accepts repetitions of `elem` separated by left-associative `op`.
    * The value returned is reduced left-to-right.
    *
    * @group combinator
    */
  def infixLeft[V, W, A](
      elem: Parser[V, A],
      op: Parser[W, (A, A) => A],
      rev: PartialFunction[V, (V, W, V)]): Parser[V, A] =

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

  /** Parser that accepts repetitions of `elem` separated by right-associative `op`.
    * The value returned is reduced right-to-left.
    *
    * @group combinator
    */
  def infixRight[V, W, A](
      elem: Parser[V, A],
      op: Parser[W, (A, A) => A],
      rev: PartialFunction[V, (V, W, V)]): Parser[V, A] =

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

  /** Parser that parses `elem` prefixed by any number of `op`.
    *
    * Operators are applied right-to-left.
    *
    * @group combinator
    */
  def prefixes[V, W, A](op: Parser[W, A => A], elem: Parser[V, A], rev: PartialFunction[V, (W, V)]): Parser[V, A] = {
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

  /** Parser that parses `elem` postfixed by any number of `op`.
    *
    * Operators are applied left-to-right.
    *
    * @group combinator
    */
  def postfixes[V, W, A](elem: Parser[V, A], op: Parser[W, A => A], rev: PartialFunction[V, (V, W)]): Parser[V, A] = {
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