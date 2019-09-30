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

import scala.language.higherKinds

/** Contains definitions relating to parsers. */
trait Parsers[Token, Kind] {

  /** Result of parsing.
    *
    * @group result
    */
  sealed trait ParseResult[R[_], A] {

    /** Parser for the rest of input. */
    val rest: R[A]

    /** Returns the parsed value, if any. */
    def getValue: Option[A] = this match {
      case Parsed(value, _) => Some(value)
      case _ => None
    }
  }

  /** Indicates that the input has been fully parsed, resulting in a `value`.
    *
    * A parser for subsequent input is also provided.
    *
    * @param value The value produced.
    * @param rest  Parser for more input.
    *
    * @group result
    */
  case class Parsed[R[_], A](value: A, rest: R[A]) extends ParseResult[R, A]

  /** Indicates that the provided `token` was not expected at that point.
    *
    * The parser at the point of error is returned.
    *
    * @param token The token at fault.
    * @param rest  Parser at the point of error.
    *
    * @group result
    */
  case class UnexpectedToken[R[_], A](token: Token, rest: R[A]) extends ParseResult[R, A]

  /** Indicates that end of input was unexpectedly encountered.
    *
    * The `syntax` for subsequent input is provided.
    *
    * @param syntax Syntax at the end of input.
    *
    * @group result
    */
  case class UnexpectedEnd[R[_], A](rest: R[A]) extends ParseResult[R, A]

  /** Processes iterators of tokens to try and produce a value. */
  trait Parser[R[_], A] {
    def apply(tokens: Iterator[Token]): ParseResult[R, A]
  }
}