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

package scallion.parsing

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
trait Operators { self: Parsers[_, _] =>

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
  case class Level[A](operator: Parser[(A, A) => A], associativity: Associativity)


  /** Implicitly decorates an `operator` parser to add an `is` methods
    * that indicates the associativity of the parser.
    *
    * {{{
    * val level = div | times is LeftAssociative
    * }}}
    *
    * @group level
    */
  implicit class LevelDecorator[A](operator: Parser[(A, A) => A]) {

    /** Indicates the associativity of the operator. */
    def is(associativity: Associativity): Level[A] = Level(operator, associativity)
  }

  /** Parser that parses repetitions of `elem` separated by infix operators.
    *
    * The operators in earlier levels are considered to bind tighter than those in later levels.
    *
    * {{{
    * val boolExpr = operators(basicBoolExpr)(
    *   andOp is LeftAssociative,
    *   orOp is LeftAssociative,
    *   implyOp is RightAssociative)
    * }}}
    *
    * @group combinator
    */
  def operators[A](elem: Parser[A])(levels: Level[A]*): Parser[A] = {
    levels.foldLeft(elem) {
      case (acc, Level(op, assoc)) => assoc match {
        case LeftAssociative => infixLeft(acc, op)
        case RightAssociative => infixRight(acc, op)
      }
    }
  }

  /** Parser that accepts repetitions of `elem` separated by left-associative `op`.
    * The value returned is reduced left-to-right.
    *
    * @group combinator
    */
  def infixLeft[A](elem: Parser[A], op: Parser[(A, A) => A]): Parser[A] =
    (elem ~ many(op ~ elem)).map {
      case first ~ opElems => opElems.foldLeft(first) {
        case (acc, (op ~ elem)) => op(acc, elem)
      }
    }

  /** Parser that accepts repetitions of `elem` separated by right-associative `op`.
    * The value returned is reduced right-to-left.
    *
    * @group combinator
    */
  def infixRight[A](elem: Parser[A], op: Parser[(A, A) => A]): Parser[A] =
    (elem ~ many(op ~ elem)).map {
      case first ~ opElems => {
        val (ops, elems) = opElems.map(t => (t._1, t._2)).unzip
        val allElems = first +: elems
        val elemOps = allElems.zip(ops)
        elemOps.foldRight(allElems.last) {
          case ((elem, op), acc) => op(elem, acc)
        }
      }
    }

  /** Parser that parses `elem` prefixed by any number of `op`.
    *
    * Operators are applied right-to-left.
    *
    * @group combinator
    */
  def prefixes[A](op: Parser[A => A], elem: Parser[A]): Parser[A] = {
    many(op) ~ elem map {
      case os ~ v => os.foldRight(v) {
        case (o, acc) => o(acc)
      }
    }
  }

  /** Parser that parses `elem` postfixed by any number of `op`.
    *
    * Operators are applied left-to-right.
    *
    * @group combinator
    */
  def postfixes[A](elem: Parser[A], op: Parser[A => A]): Parser[A] = {
    elem ~ many(op) map {
      case v ~ os => os.foldLeft(v) {
        case (acc, o) => o(acc)
      }
    }
  }
}