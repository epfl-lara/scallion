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

import org.scalatest._

import scallion.syntactic._

object Tokens {
  sealed trait Token
  case class Num(value: Int) extends Token
  case class Bool(value: Boolean) extends Token
  case class Op(value: Char) extends Token
  case class Del(value: Char) extends Token
  case class Kw(value: String) extends Token

  sealed trait TokenClass
  case object NumClass extends TokenClass
  case object BoolClass extends TokenClass
  case class OperatorClass(value: Char) extends TokenClass
  case class DelimiterClass(value: Char) extends TokenClass
  case class KeywordClass(value: String) extends TokenClass
}
import Tokens._

class ParserTests extends FlatSpec with Inside with Syntaxes[Token, TokenClass] with Operators {

  override def getKind(token: Token): TokenClass = token match {
    case Num(_) => NumClass
    case Bool(_) => BoolClass
    case Op(value) => OperatorClass(value)
    case Del(value) => DelimiterClass(value)
    case Kw(value) => KeywordClass(value)
  }

  import Syntax._

  // elem

  "elem" should "parse tokens from the specified class" in {
    val parser = elem(NumClass)

    inside(parser(Seq(Num(1)).iterator)) {
      case Parsed(res, rest) => {
        assert(res == Num(1))
        assert(rest.first.isEmpty)
        assert(rest.toSyntax.first.isEmpty)
      }
    }
  }

  it should "not parse tokens from different classes" in {
    val parser = elem(NumClass)

    inside(parser(Seq(Bool(true)).iterator)) {
      case UnexpectedToken(token, rest) => {
        assert(token == Bool(true))
      }
    }
  }

  it should "correctly fail at the end of input" in {
    val parser = elem(NumClass)

    inside(parser(Seq().iterator)) {
      case UnexpectedEnd(rest) => {
        assert(rest.first == Set(NumClass))
        assert(rest.toSyntax.first == Set(NumClass))
      }
    }
  }

  it should "not be nullable" in {
    val parser = elem(NumClass)

    assert(parser.nullable.isEmpty)
  }

  it should "correctly define `first`" in {
    val parser = elem(NumClass)

    assert(parser.first == Set(NumClass))
  }

  it should "be LL(1)" in {
    val parser = elem(NumClass)

    assert(parser.isLL1)
  }

  // accept

  "accept" should "parser tokens from the specified class" in {
    val parser = accept(NumClass) {
      case Num(value) => value * 2
    }

    inside(parser(Seq(Num(1)).iterator)) {
      case Parsed(res, rest) => {
        assert(res == 2)
        assert(rest.first.isEmpty)
        assert(rest.toSyntax.first.isEmpty)
      }
    }
  }

  it should "not parse tokens from different classes" in {
    val parser: Syntax[Int] = accept(NumClass) {
      case Num(value) => value * 2
    }

    inside(parser(Seq(Bool(true)).iterator)) {
      case UnexpectedToken(token, rest) => {
        assert(token == Bool(true))
      }
    }
  }

  it should "correctly fail at the end of input" in {
    val parser: Syntax[Int] = accept(NumClass) {
      case Num(value) => value * 2
    }

    inside(parser(Seq().iterator)) {
      case UnexpectedEnd(rest) => {
        assert(rest.first == Set(NumClass))
        assert(rest.toSyntax.first == Set(NumClass))
      }
    }
  }

  it should "not be nullable" in {
    val parser = accept(NumClass) {
      case Num(value) => value * 2
    }

    assert(parser.nullable.isEmpty)
  }

  it should "correctly define `first`" in {
    val parser = accept(NumClass) {
      case Num(value) => value * 2
    }

    assert(parser.first == Set(NumClass))
  }

  it should "be LL(1)" in {
    val parser = accept(NumClass) {
      case Num(value) => value * 2
    }

    assert(parser.isLL1)
  }

  // epsilon

  "epsilon" should "correctly return value at the end of input" in {
    val parser = epsilon("ok")

    inside(parser(Seq().iterator)) {
      case Parsed(res, rest) => {
        assert(res == "ok")
      }
    }
  }

  it should "fail in case of remaining input" in {
    val parser = epsilon("ok")

    inside(parser(Seq(Bool(true)).iterator)) {
      case UnexpectedToken(token, rest) => {
        assert(token == Bool(true))
      }
    }
  }

  it should "be nullable" in {
    val parser = epsilon(17)

    assert(parser.nullable == Some(17))
  }

  it should "have an empty `first`" in {
    val parser = epsilon(17)

    assert(parser.first.isEmpty)
  }

  it should "be LL(1)" in {
    val parser = epsilon(17)

    assert(parser.isLL1)
  }

  // failure

  "failure" should "correctly fail in case of end of input" in {
    val parser = failure[Any]

    inside(parser(Seq().iterator)) {
      case UnexpectedEnd(rest) => {
        assert(!rest.isProductive)
        assert(!rest.toSyntax.isProductive)
      }
    }
  }

  it should "correctly fail in case of remaining input" in {
    val parser = failure[Any]

    inside(parser(Seq(Bool(true)).iterator)) {
      case UnexpectedToken(token, rest) => {
        assert(token == Bool(true))
      }
    }
  }

  it should "not be nullable" in {
    val parser = failure[Any]

    assert(parser.nullable.isEmpty)
  }

  it should "have an empty `first`" in {
    val parser = failure[Any]

    assert(parser.first.isEmpty)
  }

  it should "be LL(1)" in {
    val parser = failure[Any]

    assert(parser.isLL1)
  }

  // sequencing

  "sequencing" should "parse using the two parsers in sequence" in {
    val parser = elem(BoolClass) ~ elem(NumClass)

    inside(parser(Seq(Bool(true), Num(32)).iterator)) {
      case Parsed(first ~ second, rest) => {
        assert(first == Bool(true))
        assert(second == Num(32))
      }
    }
  }

  it should "use the fact that left might be nullable for parsing" in {
    val parser = (elem(BoolClass) | epsilon(Bool(true))) ~ elem(NumClass)

    inside(parser(Seq(Num(32)).iterator)) {
      case Parsed(first ~ second, rest) => {
        assert(first == Bool(true))
        assert(second == Num(32))
      }
    }
  }

  it should "fail at the correct point" in {
    val parser = elem(BoolClass) ~ elem(NumClass)

    inside(parser(Seq(Num(1), Num(2)).iterator)) {
      case UnexpectedToken(token, rest) => {
        assert(token == Num(1))
        assert(rest.first == Set(BoolClass))
        assert(rest.toSyntax.first == Set(BoolClass))
      }
    }

    inside(parser(Seq(Bool(true), Bool(false)).iterator)) {
      case UnexpectedToken(token, rest) => {
        assert(token == Bool(false))
        assert(rest.first == Set(NumClass))
        assert(rest.toSyntax.first == Set(NumClass))
      }
    }
  }

  it should "be nullable if both sides are nullable" in {
    val parser = epsilon(Bool(true)) ~ epsilon(Num(13))

    inside(parser.nullable) {
      case Some(first ~ second) => {
        assert(first == Bool(true))
        assert(second == Num(13))
      }
    }
  }

  it should "not be nullable if the first parser is not nullable" in {
    val parser = elem(BoolClass) ~ epsilon(Bool(true))

    assert(parser.nullable.isEmpty)
  }

  it should "not be nullable if the second parser is not nullable" in {
    val parser = epsilon(Bool(true)) ~ elem(BoolClass)

    assert(parser.nullable.isEmpty)
  }

  it should "not be nullable if both sides are not nullable" in {
    val parser = elem(BoolClass) ~ elem(NumClass)

    assert(parser.nullable.isEmpty)
  }

  it should "have correct `first` in case of non-nullable first parser" in {
    val parser = elem(BoolClass) ~ elem(NumClass)

    assert(parser.first == Set(BoolClass))
  }

  it should "have correct `first` in case of nullable first parser" in {
    val parser = (elem(BoolClass) | epsilon(Bool(true))) ~ elem(NumClass)

    assert(parser.first == Set(BoolClass, NumClass))
  }

  it should "not be LL(1) when the first is nullable and both sides have conflicting `first`" in {
    val parser = (elem(BoolClass) | epsilon(Bool(true))) ~ elem(BoolClass)

    assert(!parser.isLL1)
  }

  it should "not be LL(1) when the first has a trailing nullable that conflicts with the second's `first`" in {
    val left = elem(NumClass) ~ (elem(BoolClass) | epsilon(Bool(true)))

    assert(left.isLL1)

    val parser = left ~ elem(BoolClass)

    assert(!parser.isLL1)
  }

  it should "not be LL(1) when first parser is not LL(1)" in {
    val parser = (epsilon(Bool(true)) | epsilon(Bool(false))) ~ Elem(BoolClass)

    assert(!parser.isLL1)
  }

  it should "not be LL(1) when second parser is not LL(1)" in {
    val parser = elem(BoolClass) ~ (epsilon(Bool(true)) | epsilon(Bool(false)))

    assert(!parser.isLL1)
  }

  it should "be LL(1) otherwise" in {
    val parser = (epsilon[Token](Num(2)) | elem(BoolClass)) ~ (elem(NumClass) | epsilon(Bool(true)))

    assert(parser.isLL1)
  }

  // concatenation

  "concatenation" should "parse using the two parsers in sequence" in {
    def f(k: TokenClass): Syntax[Seq[Token]] = elem(k).map(Seq(_))
    val parser = f(BoolClass) ++ f(NumClass)

    inside(parser(Seq(Bool(true), Num(32)).iterator)) {
      case Parsed(res, rest) => {
        assert(res == Seq(Bool(true), Num(32)))
        assert(rest.first.isEmpty)
        assert(rest.toSyntax.first.isEmpty)
      }
    }
  }

  it should "use the fact that left might be nullable for parsing" in {
    val parser = (elem(BoolClass) |
      epsilon(Bool(true))).map(Seq(_)) ++
      elem(NumClass).map(Seq(_))

    inside(parser(Seq(Num(32)).iterator)) {
      case Parsed(res, rest) => {
        assert(res == Seq(Bool(true), Num(32)))
      }
    }
  }

  it should "fail at the correct point" in {
    val parser = elem(BoolClass).map(Seq(_)) ++ elem(NumClass).map(Seq(_))

    inside(parser(Seq(Num(1), Num(2)).iterator)) {
      case UnexpectedToken(token, rest) => {
        assert(token == Num(1))
        assert(rest.first == Set(BoolClass))
        assert(rest.toSyntax.first == Set(BoolClass))
      }
    }

    inside(parser(Seq(Bool(true), Bool(false)).iterator)) {
      case UnexpectedToken(token, rest) => {
        assert(token == Bool(false))
        assert(rest.first == Set(NumClass))
        assert(rest.toSyntax.first == Set(NumClass))
      }
    }
  }

  it should "be nullable if both sides are nullable" in {
    val parser = epsilon(Bool(true)).map(Seq(_)) ++ epsilon(Bool(false)).map(Seq(_))

    assert(parser.nullable == Some(Seq(Bool(true), Bool(false))))
  }

  it should "not be nullable if the first parser is not nullable" in {
    val parser = elem(BoolClass).map(Seq(_)) ++ (elem(BoolClass) | epsilon(Bool(false))).map(Seq(_))

    assert(parser.nullable.isEmpty)
  }

  it should "not be nullable if the second parser is not nullable" in {
    val parser = epsilon[Token](Bool(false)).map(Seq(_)) ++ elem(BoolClass).map(Seq(_))

    assert(parser.nullable.isEmpty)
  }

  it should "not be nullable if both sides are not nullable" in {
    val parser = elem(BoolClass).map(Seq(_)) ++ elem(NumClass).map(Seq(_))

    assert(parser.nullable.isEmpty)
  }

  it should "have correct `first` in case of non-nullable first parser" in {
    val parser = elem(BoolClass).map(Seq(_)) ++ elem(NumClass).map(Seq(_))

    assert(parser.first == Set(BoolClass))
  }

  it should "have correct `first` in case of nullable first parser" in {
    val parser = (elem(BoolClass).map(Seq(_)) | epsilon(Seq())) ++ elem(NumClass).map(Seq(_))

    assert(parser.first == Set(BoolClass, NumClass))
  }

  it should "not be LL(1) when the first is nullable and both sides have conflicting `first`" in {
    val parser = (elem(BoolClass) | epsilon(Bool(true))).map(Seq(_)) ++ elem(BoolClass).map(Seq(_))

    assert(!parser.isLL1)
  }

  it should "not be LL(1) when the first has a trailing nullable that conflicts with the second's `first`" in {
    val left = elem(NumClass).map(Seq(_)) ++ (elem(BoolClass) | epsilon(Bool(true))).map(Seq(_))

    assert(left.isLL1)

    val parser = left ++ elem(BoolClass).map(Seq(_))

    assert(!parser.isLL1)
  }

  it should "not be LL(1) when first parser is not LL(1)" in {
    val parser = (epsilon[Token](Bool(true)) | epsilon(Bool(false))).map(Seq(_)) ++ elem(BoolClass).map(Seq(_))

    assert(!parser.isLL1)
  }

  it should "not be LL(1) when second parser is not LL(1)" in {
    val parser = elem(BoolClass).map(Seq(_)) ++ (epsilon(Bool(true)) | epsilon(Bool(false))).map(Seq[Token](_))

    assert(!parser.isLL1)
  }

  it should "be LL(1) otherwise" in {
    val parser = (epsilon[Token](Num(2)) | elem(BoolClass)).map(Seq(_)) ++
                 (elem(NumClass) | epsilon(Bool(true))).map(Seq(_))

    assert(parser.isLL1)
  }

  // disjunction

  "disjunction" should "accept from the first parser" in {
    val parser = elem(BoolClass) | elem(NumClass)

    inside(parser(Seq(Bool(true)).iterator)) {
      case Parsed(res, rest) => {
        assert(res == Bool(true))
        inside(rest.toSyntax) {
          case Success(Bool(true), _) => ()
        }
      }
    }
  }

  it should "accept from the second parser" in {
    val parser = elem(BoolClass) | elem(NumClass)

    inside(parser(Seq(Num(1)).iterator)) {
      case Parsed(res, rest) => {
        assert(res == Num(1))
        inside(rest.toSyntax) {
          case Success(Num(1), _) => ()
        }
      }
    }
  }

  it should "not be nullable if neither sides are nullable" in {
    val parser = elem(BoolClass) | elem(NumClass)

    assert(parser.nullable.isEmpty)
  }

  it should "be nullable if the first parser is nullable" in {
    val parser = epsilon[Token](Bool(true)) | elem(NumClass)

    assert(parser.nullable == Some(Bool(true)))
  }

  it should "be nullable if the second parser is nullable" in {
    val parser = elem(BoolClass) | epsilon(Bool(true))

    assert(parser.nullable == Some(Bool(true)))
  }

  it should "be nullable if both sides are nullable" in {
    val parser = epsilon(Bool(false)) | epsilon(Bool(true))

    assert(parser.nullable.nonEmpty)
  }

  it should "have correct `first`" in {
    val parser = elem(BoolClass) | elem(NumClass) | epsilon(Bool(true))

    assert(parser.first == Set(BoolClass, NumClass))
  }

  it should "not be LL(1) when both sides are nullable" in {
    val parser = epsilon(Bool(true)) | epsilon(Bool(false))

    assert(!parser.isLL1)
  }

  it should "not be LL(1) when both sides have interecting first sets" in {
    val parser = elem(BoolClass) | (elem(NumClass) | elem(BoolClass))

    assert(!parser.isLL1)
  }

  it should "not be LL(1) when first parser is not LL(1)" in {
    val parser = (epsilon[Token](Bool(true)) | epsilon(Bool(false))) | Elem(BoolClass)

    assert(!parser.isLL1)
  }

  it should "not be LL(1) when second parser is not LL(1)" in {
    val parser = elem(BoolClass) | (epsilon[Token](Bool(true)) | epsilon(Bool(false)))

    assert(!parser.isLL1)
  }

  it should "be LL(1) otherwise" in {
    val parser = elem(BoolClass) | elem(NumClass) | epsilon(Bool(true))

    assert(parser.isLL1)
  }

  // tagged disjunction

  "tagged disjunction" should "correctly tag values" in {
    val parser = elem(BoolClass) || elem(NumClass)

    inside(parser(Seq(Num(1)).iterator)) {
      case Parsed(res, rest) => {
        assert(res == Right(Num(1)))
      }
    }

    inside(parser(Seq(Bool(true)).iterator)) {
      case Parsed(res, rest) => {
        assert(res == Left(Bool(true)))
      }
    }
  }

  it should "accept different branch types" in {
    val parser = elem(BoolClass).map(_ => "X") || elem(NumClass).map(_ => 42)

    inside(parser(Seq(Num(1)).iterator)) {
      case Parsed(res, rest) => {
        assert(res == Right(42))
      }
    }

    inside(parser(Seq(Bool(true)).iterator)) {
      case Parsed(res, rest) => {
        assert(res == Left("X"))
      }
    }
  }

  // many

  "many" should "parse zero repetitions" in {
    val parser = many(elem(NumClass))

    inside(parser(Seq().iterator)) {
      case Parsed(res, rest) => {
        assert(res == Seq())
        assert(rest.first == Set(NumClass))
        assert(rest.toSyntax.first == Set(NumClass))
      }
    }
  }

  it should "parse one repetition" in {
    val parser = many(elem(NumClass))

    inside(parser(Seq(Num(12)).iterator)) {
      case Parsed(res, rest) => {
        assert(res == Seq(Num(12)))
        assert(rest.first == Set(NumClass))
        assert(rest.toSyntax.first == Set(NumClass))
      }
    }
  }

  it should "parse several repetitions" in {
    val parser = many(elem(NumClass))

    inside(parser(Seq(Num(12), Num(34), Num(1)).iterator)) {
      case Parsed(res, rest) => {
        assert(res == Seq(Num(12), Num(34), Num(1)))
        assert(rest.first == Set(NumClass))
        assert(rest.toSyntax.first == Set(NumClass))
      }
    }
  }

  it should "not fix choices" in {
    val parser = many(elem(NumClass) | elem(BoolClass))

    inside(parser(Seq(Num(12), Bool(true), Num(1), Num(12), Bool(false)).iterator)) {
      case Parsed(res, rest) => {
        assert(res == Seq(Num(12), Bool(true), Num(1), Num(12), Bool(false)))
        assert(rest.toSyntax.first == Set(NumClass, BoolClass))
      }
    }
  }

  it should "fail when inner parser fails" in {
    val parser = many(elem(NumClass))

    inside(parser(Seq(Num(12), Bool(true), Num(1)).iterator)) {
      case UnexpectedToken(token, rest) => {
        assert(token == Bool(true))
        assert(rest.first == Set(NumClass))
        assert(rest.nullable == Some(Seq(Num(12))))
        assert(rest.toSyntax.first == Set(NumClass))
        assert(rest.toSyntax.nullable == Some(Seq(Num(12))))
      }
    }
  }

  it should "be nullable" in {
    val parser = many(elem(NumClass))

    assert(parser.nullable.nonEmpty)
  }

  it should "inherit the `first`" in {
    val parser = many(elem(NumClass))

    assert(parser.first == elem(NumClass).first)
  }

  it should "be LL(1) if the inner parser is LL(1) and not nullable" in {
    val parser = many(elem(NumClass))

    assert(parser.isLL1)
  }

  it should "not be LL(1) when the inner parser is nullable" in {
    val parser = many(elem(NumClass) | epsilon(Bool(true)))

    assert(!parser.isLL1)
  }

  it should "not be LL(1) when the inner parser is not LL(1)" in {
    val parser = many(epsilon[Token](Num(1)) | elem(BoolClass) | epsilon[Token](Bool(true)))

    assert(!parser.isLL1)
  }

  // many1

  "many1" should "not parse zero repetitions" in {
    val parser = many1(elem(NumClass))

    inside(parser(Seq().iterator)) {
      case UnexpectedEnd(rest) => {
        assert(rest.first == Set(NumClass))
        assert(rest.toSyntax.first == Set(NumClass))
      }
    }
  }

  it should "parse one repetition" in {
    val parser = many1(elem(NumClass))

    inside(parser(Seq(Num(12)).iterator)) {
      case Parsed(res, rest) => {
        assert(res == Seq(Num(12)))
        assert(rest.first == Set(NumClass))
        assert(rest.toSyntax.first == Set(NumClass))
      }
    }
  }

  it should "parse several repetitions" in {
    val parser = many1(elem(NumClass))

    inside(parser(Seq(Num(12), Num(34), Num(1)).iterator)) {
      case Parsed(res, rest) => {
        assert(res == Seq(Num(12), Num(34), Num(1)))
        assert(rest.first == Set(NumClass))
        assert(rest.toSyntax.first == Set(NumClass))
      }
    }
  }

  it should "not fix choices" in {
    val parser = many1(elem(NumClass) | elem(BoolClass))

    inside(parser(Seq(Num(12), Bool(true), Num(1), Num(12), Bool(false)).iterator)) {
      case Parsed(res, rest) => {
        assert(res == Seq(Num(12), Bool(true), Num(1), Num(12), Bool(false)))
        assert(rest.first == Set(NumClass, BoolClass))
        assert(rest.toSyntax.first == Set(NumClass, BoolClass))
      }
    }
  }

  it should "fail when inner parser fails" in {
    val parser = many1(elem(NumClass))

    inside(parser(Seq(Num(12), Bool(true), Num(1)).iterator)) {
      case UnexpectedToken(token, rest) => {
        assert(token == Bool(true))
        assert(rest.first == Set(NumClass))
        assert(rest.nullable == Some(Seq(Num(12))))
        assert(rest.toSyntax.first == Set(NumClass))
        assert(rest.toSyntax.nullable == Some(Seq(Num(12))))
      }
    }
  }

  it should "not be nullable" in {
    val parser = many1(elem(NumClass))

    assert(parser.nullable.isEmpty)
  }

  it should "inherit the `first`" in {
    val parser = many1(elem(NumClass))

    assert(parser.first == elem(NumClass).first)
  }

  it should "be LL(1) if the inner parser is LL(1) and not nullable" in {
    val parser = many1(elem(NumClass))

    assert(parser.isLL1)
  }

  it should "not be LL(1) when the inner parser is nullable" in {
    val parser = many1(elem(NumClass) | epsilon(Bool(true)))

    assert(!parser.isLL1)
  }

  it should "not be LL(1) when the inner parser is not LL(1)" in {
    val parser = many1(epsilon[Token](Num(1)) | elem(BoolClass) | epsilon[Token](Bool(true)))

    assert(!parser.isLL1)
  }

  // recursive

  "recursive" should "allow building recursive parsers" in {
    lazy val parser: Syntax[Seq[Token]] = recursive {
      elem(BoolClass) +: parser | epsilon(Seq())
    }

    inside(parser(Seq(Bool(true), Bool(false)).iterator)) {
      case Parsed(res, rest) => {
        assert(res == Seq(Bool(true), Bool(false)))
        assert(rest.first == Set(BoolClass))
        assert(rest.toSyntax.first == Set(BoolClass))
      }
    }
  }

  // LL1 conflicts

  import LL1Conflict._

  "LL1 conflicts" should "catch ambiguous first kinds" in {
    val parser = elem(BoolClass) | elem(NumClass) | elem(BoolClass) ~<~ elem(NumClass).unit()

    assert(!parser.isLL1)

    val conflicts = parser.conflicts.toSeq
    assert(conflicts.size == 1)

    inside(conflicts(0)) {
      case FirstConflict(source, ambiguities) => {
        assert(ambiguities == Set(BoolClass))
        assert(source == parser)
      }
    }
  }

  it should "catch multiple nullable branchs" in {
    val parser =
      elem(NumClass)           |
      epsilon(Bool(true))      |
      elem(OperatorClass('+')) |
      epsilon(Bool(true))

    assert(!parser.isLL1)

    val conflicts = parser.conflicts.toSeq
    assert(conflicts.size == 1)

    inside(conflicts(0)) {
      case NullableConflict(source) => {
        assert(source == parser)
      }
    }
  }

  it should "catch ambiguous nullable in sequences" in {
    val parser =
      elem(BoolClass) ~
      opt(elem(NumClass)) ~
      (elem(NumClass).up[Any] | opt(elem(BoolClass)).up[Any]) ~
      elem(BoolClass)

    assert(!parser.isLL1)

    val conflicts = parser.conflicts.toSeq
    assert(conflicts.size == 2)
  }

  it should " catch combinations of problems" in {
    val literal = accept(NumClass) {
      case Num(value) => value
    } | epsilon(0)

    lazy val expr: Syntax[Int] = recursive {
      plusExpr | opt(elem(OperatorClass('+'))).unit() ~>~ literal
    }

    lazy val plusExpr: Syntax[Int] = (opt(elem(OperatorClass('+'))) ~ expr).map {
      case _ ~ rhs => rhs
    }

    val cs = expr.conflicts.toSeq

    assert(cs.size == 3)

    val firstConflicts = cs.collect {
      case c: FirstConflict => c
    }

    // In expr, both branchs can start with "+" or "Num".
    assert(firstConflicts.size == 1)
    assert(firstConflicts(0).ambiguities == Set(OperatorClass('+'), NumClass))

    val nullableConflicts = cs.collect {
      case c: NullableConflict => c
    }

    // Both branches of expr are nullable.
    assert(nullableConflicts.size == 1)

    val followConflicts = cs.collect {
      case c: FollowConflict => c
    }


    // In plusExpr, left hand side can not be followed by "+",
    // but expr can start with "+".
    assert(followConflicts.size == 1)
    assert(followConflicts(0).ambiguities == Set(OperatorClass('+')))
  }


  "Trails" should "return correct results for basic parsers" in {
    assert(elem(NumClass).trails.toSeq == Seq(Seq(NumClass)))
    assert(epsilon(0).trails.toSeq == Seq(Seq()))
    assert(failure[Any].trails.toSeq == Seq())
  }

  it should "work for basic sequencing" in {
    assert((elem(NumClass) ~ elem(BoolClass)).trails.toSeq == Seq(Seq(NumClass, BoolClass)))
  }

  it should "work for basic disjunction" in {
    assert((elem(NumClass) | elem(BoolClass)).trails.toSeq == Seq(Seq(NumClass), Seq(BoolClass)))
  }

  it should "return elements in increasing order" in {
    val one = elem(BoolClass)
    val two = elem(NumClass) ~ elem(NumClass)
    val five = two ~ one ~ two

    val parser = (one ~ opt(five)).up[Any] | five.up[Any] | (two ~ opt(two)).up[Any]

    val trails = parser.trails.toSeq

    assert(trails.size == 5)
    assert(trails(0) == Seq(BoolClass))
    assert(trails(1) == Seq(NumClass, NumClass))
    assert(trails(2) == Seq(NumClass, NumClass, NumClass, NumClass))
    assert(trails(3) == Seq(NumClass, NumClass, BoolClass, NumClass, NumClass))
    assert(trails(4) == Seq(BoolClass, NumClass, NumClass, BoolClass, NumClass, NumClass))
  }

  it should "work for simple recursive parsers" in {
    lazy val parser: Syntax[Any] = recursive(elem(BoolClass).up[Any] | (elem(NumClass) ~ parser).up[Any])

    val trails = parser.trails

    assert(trails.next() == Seq(BoolClass))
    assert(trails.next() == Seq(NumClass, BoolClass))
    assert(trails.next() == Seq(NumClass, NumClass, BoolClass))
  }

  it should "work for intricate, non-LL(1), recursive parsers" in {
    lazy val parser: Syntax[Any] =
      many(elem(OperatorClass('+'))).up[Any]      |
      recursive(parser ~ elem(BoolClass)).up[Any] |
      recursive(parser ~ elem(NumClass) ~ parser).up[Any]

    val trails = parser.trails

    assert(trails.next() == Seq())
    assert(trails.next() == Seq(OperatorClass('+')))
    assert(trails.next() == Seq(BoolClass))
    assert(trails.next() == Seq(NumClass))
    assert(trails.next() == Seq(OperatorClass('+'), OperatorClass('+')))
    assert(trails.next() == Seq(OperatorClass('+'), BoolClass))
    assert(trails.next() == Seq(BoolClass, BoolClass))
    assert(trails.next() == Seq(NumClass, BoolClass))
    assert(trails.next() == Seq(NumClass, OperatorClass('+')))
    assert(trails.next() == Seq(NumClass, BoolClass))
    assert(trails.next() == Seq(NumClass, NumClass))

    // Check that for the next 10000 elements all sequences appear in order.
    var previous = 2
    trails.take(10000).foreach { trail =>
      assert(trail.size >= previous)
      previous = trail.size
    }
  }


  "Prefix" should "return a syntax for the language up to that point" in {
    val point = elem(BoolClass).up[Any]

    lazy val syntax: Syntax[Any] = recursive {
      opt(elem(NumClass) ~ syntax) ~ point
    }.up[Any]

    val trails = syntax.prefix(point).trails

    assert(trails.next() == Seq())
    assert(trails.next() == Seq(NumClass))
    assert(trails.next() == Seq(NumClass, NumClass))
    assert(trails.next() == Seq(NumClass, BoolClass))
  }
}
