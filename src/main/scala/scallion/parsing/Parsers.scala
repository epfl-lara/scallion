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

import scala.collection.immutable.ListSet

/** Contains definitions relating to parsers.
  *
  * @see See trait [[scallion.parsing.Operators]] for useful combinators
  *      to describe infix, prefix and postfix operators.
  *
  * @group parsing
  *
  * @groupprio abstract
  * @groupname abstract Abstract Members
  *
  * @groupprio parser 1
  * @groupname parser Parser
  *
  * @groupprio result 2
  * @groupname result Parse Results
  *
  * @groupprio combinator 3
  * @groupname combinator Combinators
  *
  * @groupprio conflict 4
  * @groupname conflict LL(1) Conflicts
  */
trait Parsers[Token, Kind] {

  import Parser._

  /** Returns the kind associated with `token`.
    *
    * @group abstract
    */
  def getKind(token: Token): Kind

  /** Consumes a stream of tokens and tries to produces a value of type `A`.
    *
    * @group parser
    *
    * @groupprio parsing 5
    * @groupname parsing Parsing
    *
    * @groupprio property 6
    * @groupname property Properties
    */
  sealed trait Parser[+A] {

    /** The value, if any, produced by this parser without consuming more input.
      *
      * @group property
      */
    def nullable: Option[A]

    /** Indicates if there exists a sequence of tokens that the parser can accept.
      *
      * @group property
      */
    def isProductive: Boolean

    /** Returns the set of tokens that are accepted as the next token.
      *
      * @group property
      */
    @inline def first: Set[Kind] = collectFirst(ListSet())

    /** Returns the set of tokens that should not be accepted
      * as the next token by a subsequent parser.
      *
      * @group property
      */
    @inline def shouldNotFollow: Set[Kind] = collectShouldNotFollow(ListSet())

    /** Checks if a `Recursive` parser can be entered without consuming input first.
      *
      * @param id The reference of the `Recursive` parser.
      *
      * @group property
      */
    @inline def calledLeft(id: Recursive[Any]): Boolean = collectCalledLeft(id, ListSet())

    /** Checks if this parser corresponds to a LL(1) grammar.
      *
      * @group property
      */
    @inline def isLL1: Boolean = collectIsLL1(ListSet())

    /** Returns all LL(1) conflicts in the parser.
      *
      * @group property
      */
    @inline def conflicts: Set[LL1Conflict] = collectLL1Conflicts(ListSet())

    // All the functions below have an argument `recs` which
    // contains the set of all `Recursive` parser on which the call
    // was already performed.
    //
    // This is done to handle the potentially cyclic structure of parsers
    // introduced by `Recursive`.

    protected def collectNullable(recs: Set[AnyRef]): Option[A]
    protected def collectFirst(recs: Set[AnyRef]): Set[Kind]
    protected def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind]
    protected def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean
    protected def collectIsProductive(recs: Set[AnyRef]): Boolean
    protected def collectIsLL1(recs: Set[AnyRef]): Boolean
    protected def collectLL1Conflicts(recs: Set[AnyRef]): Set[LL1Conflict]

    /** Feeds a token to the parser and obtain a parser for the rest of input.
      *
      * @group parsing
      */
    def derive(token: Token, kind: Kind): Parser[A]

    /** String representation of the parser. */
    override def toString = repr(0, Map.empty)

    /** Computes a friendlier string representation for the parser. */
    protected def repr(level: Int, recs: Map[AnyRef, String]): String

    /** Applies a function to the parsed values.
      *
      * @group combinator
      */
    def map[B](function: A => B): Parser[B] = this match {
      case Failure => Failure
      case Success(value) => Success(function(value))
      case Transform(other, inner) => Transform(other andThen function, inner)
      case _ => Transform(function, this)
    }

    /** Sequences `this` and `that` parser. The parsed values are concatenated.
      *
      * @group combinator
      */
    def ++[B](that: Parser[Seq[B]])(implicit ev1: Parser[A] <:< Parser[Seq[B]], ev2: A <:< Seq[B]): Parser[Seq[B]] = (this, that) match {
      case (Failure, _) => Failure
      case (_, Failure) => Failure
      case (Success(a), Success(b)) => Success(a ++ b)
      // The next transformation is crucial.
      // It allows to merge together values which accumulate on the left.
      case (_, Concat(left, right)) => (this ++ left) ++ right
      case _ => Concat(this, that)
    }

    /** Sequences `this` and `that` parser. The parsed value from `that` is returned.
      *
      * @group combinator
      */
    def ~>~[B](that: Parser[B]): Parser[B] = this.~(that).map(_._2)

    /** Sequences `this` and `that` parser. The parsed value from `this` is returned.
      *
      * @group combinator
      */
    def ~<~[B](that: Parser[B]): Parser[A] = this.~(that).map(_._1)

    /** Sequences `this` and `that` parser. The parsed value from `that` is appended to that from `this`.
      *
      * @group combinator
      */
    def :+[B](that: Parser[B])(implicit ev1: Parser[A] <:< Parser[Seq[B]], ev2: A <:< Seq[B]): Parser[Seq[B]] =
      this ++ that.map(Vector(_))

    /** Sequences `this` and `that` parser. The parsed value from `that` is prepended to that from `this`.
      *
      * @group combinator
      */
    def +:[B](that: Parser[B])(implicit ev1: Parser[A] <:< Parser[Seq[B]], ev2: A <:< Seq[B]): Parser[Seq[B]] =
      that.map(Vector(_)) ++ this

    /** Sequences `this` and `that` parser. The parsed values are returned as a pair.
      *
      * @group combinator
      */
    def ~[B](that: Parser[B]): Parser[A ~ B] = (this, that) match {
      case (Failure, _) => Failure
      case (_, Failure) => Failure
      case (Success(a), Success(b)) => Success(scallion.parsing.~(a, b))
      case _ => Sequence(this, that)
    }

    /** Disjunction of `this` and `that` parser.
      *
      * @group combinator
      */
    def |[B >: A](that: Parser[B]): Parser[B] = (this, that) match {
      case (Failure, _) => that
      case (_, Failure) => this
      case _ => Disjunction(this, that)
    }

    /** Disjunction of `this` and `that` parser.
      * The value is tagged to indicate the side which produced it.
      *
      * @group combinator
      */
    def ||[B](that: Parser[B]): Parser[Either[A, B]] =
      this.map(Left(_)) | that.map(Right(_))

    /** Makes the parser nullable.
      *
      * @group combinator
      */
    def opt: Parser[Option[A]] = this.map(Some(_)) | epsilon(None)

    /** Consumes a sequence of tokens and parses into a value.
      *
      * @group parsing
      */
    def apply(it: Iterator[Token]): ParseResult[A] = {
      //require(isLL1, "The parser is not LL(1).")

      var parser: Parser[A] = this
      while (it.hasNext) {
        val token = it.next()
        val newParser = parser.derive(token, getKind(token))
        if (!newParser.isProductive) {
          return UnexpectedToken(token, parser)
        }
        parser = newParser
      }
      parser.nullable match {
        case None => UnexpectedEnd(parser)
        case Some(value) => Parsed(value, parser)
      }
    }
  }

  /** Result of running a `Parser`.
    *
    * @group result
    */
  sealed trait ParseResult[+A] {

    /** Parser for the rest of input. */
    val parser: Parser[A]
  }

  /** Indicates that the input has been fully processed, resulting in a `value`.
    *
    * A `parser` for subsequent input is also provided.
    *
    * @group result
    */
  case class Parsed[+A](value: A, parser: Parser[A]) extends ParseResult[A]

  /** Indicates that the provided `token` was not expected at that point.
    *
    * The `parser` that rejected the token is returned.
    *
    * @group result
    */
  case class UnexpectedToken[+A](token: Token, parser: Parser[A]) extends ParseResult[A]

  /** Indicates that end of input was unexpectedly encountered.
    *
    * The `parser` for subsequent input is provided.
    *
    * @group result
    */
  case class UnexpectedEnd[+A](parser: Parser[A]) extends ParseResult[A]

  /** Describes a LL(1) conflict.
    *
    * @group conflict
    */
  sealed trait LL1Conflict

  /** Contains the description of the various LL(1) conflicts.
    *
    * @group conflict
    */
  object LL1Conflict {

    /** Indicates that both branches of a disjunction are nullable. */
    case class NullableConflict(parser: Disjunction[Any]) extends LL1Conflict

    /** Indicates that two branches of a disjunction share the same first token(s). */
    case class FirstConflict(ambiguities: Set[Kind], first: Parser[Any], second: Parser[Any]) extends LL1Conflict

    /** Indicates that the right end side first token set conflicts with the left end side. */
    case class FollowConflict(ambiguities: Set[Kind], left: Parser[Any], second: Parser[Any]) extends LL1Conflict

    /** Indicates that the parser recursively calls itself in a left position. */
    case class LeftRecursiveConflict(parser: Recursive[Any]) extends LL1Conflict
  }

  import LL1Conflict._

  /** Contains primitive parser combinators.
    *
    * @group parser
    */
  object Parser {

    /** Parser that produces `value` without consuming input tokens. */
    case class Success[+A](value: A) extends Parser[A] {
      override val nullable: Option[A] = Some(value)
      override val isProductive: Boolean = true
      override protected def collectNullable(recs: Set[AnyRef]): Option[A] = Some(value)
      override protected def collectFirst(recs: Set[AnyRef]): Set[Kind] = ListSet()
      override protected def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] = ListSet()
      override protected def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean = false
      override protected def collectIsLL1(recs: Set[AnyRef]): Boolean = true
      override protected def collectLL1Conflicts(recs: Set[AnyRef]): Set[LL1Conflict] = ListSet()
      override protected def collectIsProductive(recs: Set[AnyRef]): Boolean = true
      override def derive(token: Token, kind: Kind): Parser[A] = Failure
      override protected def repr(level: Int, recs: Map[AnyRef, String]): String = "epsilon(" + value.toString + ")"
    }

    /** Parser that produces `value` without consuming input tokens. */
    case object Failure extends Parser[Nothing] {
      override val nullable: Option[Nothing] = None
      override val isProductive: Boolean = false
      override protected def collectNullable(recs: Set[AnyRef]): Option[Nothing] = None
      override protected def collectFirst(recs: Set[AnyRef]): Set[Kind] = ListSet()
      override protected def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] = ListSet()
      override protected def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean = false
      override protected def collectIsLL1(recs: Set[AnyRef]): Boolean = true
      override protected def collectLL1Conflicts(recs: Set[AnyRef]): Set[LL1Conflict] = ListSet()
      override protected def collectIsProductive(recs: Set[AnyRef]): Boolean = false
      override def derive(token: Token, kind: Kind): Parser[Nothing] = Failure
      override protected def repr(level: Int, recs: Map[AnyRef, String]): String = "failure"
    }

    /** Parser that consumes tokens of the given `kind`. */
    case class Elem(kind: Kind) extends Parser[Token] {
      override val nullable: Option[Token] = None
      override val isProductive: Boolean = true
      override protected def collectNullable(recs: Set[AnyRef]): Option[Token] = None
      override protected def collectFirst(recs: Set[AnyRef]): Set[Kind] = Set(kind)
      override protected def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] = ListSet()
      override protected def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean = false
      override protected def collectIsLL1(recs: Set[AnyRef]): Boolean = true
      override protected def collectLL1Conflicts(recs: Set[AnyRef]): Set[LL1Conflict] = ListSet()
      override protected def collectIsProductive(recs: Set[AnyRef]): Boolean = true
      override def derive(token: Token, tokenKind: Kind): Parser[Token] = if (tokenKind == kind) Success(token) else Failure
      override protected def repr(level: Int, recs: Map[AnyRef, String]): String = "elem(" + kind + ")"
    }

    /** Parser that applies a `function` on the parsed value of the `inner` parser. */
    case class Transform[A, +B](function: A => B, inner: Parser[A]) extends Parser[B] {
      override lazy val nullable: Option[B] = inner.nullable.map(function)
      override lazy val isProductive: Boolean = inner.isProductive
      override protected def collectNullable(recs: Set[AnyRef]): Option[B] = inner.collectNullable(recs).map(function)
      override protected def collectFirst(recs: Set[AnyRef]): Set[Kind] = inner.collectFirst(recs)
      override protected def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] = inner.collectShouldNotFollow(recs)
      override protected def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean = inner.collectCalledLeft(id, recs)
      override protected def collectIsLL1(recs: Set[AnyRef]): Boolean = inner.collectIsLL1(recs)
      override protected def collectLL1Conflicts(recs: Set[AnyRef]): Set[LL1Conflict] = inner.collectLL1Conflicts(recs)
      override protected def collectIsProductive(recs: Set[AnyRef]): Boolean = inner.collectIsProductive(recs)
      override def derive(token: Token, kind: Kind): Parser[B] = inner.derive(token, kind).map(function)
      override protected def repr(level: Int, recs: Map[AnyRef, String]): String = inner.repr(10, recs) + ".map(<function>)"
    }

    /** Parser that sequences the `left` and `right` parsers. */
    sealed trait SequenceLike[+A, +B] { self: Parser[Any] =>

      /** Parser for the left end side of the sequence. */
      val left: Parser[A]

      /** Parser for the right end side of the sequence. */
      val right: Parser[B]

      override protected def collectFirst(recs: Set[AnyRef]): Set[Kind] = left.nullable match {
        case Some(_) => left.collectFirst(recs) ++ right.collectFirst(recs)
        case None => left.collectFirst(recs)
      }
      override protected def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] = right.nullable match {
        case Some(_) => right.collectShouldNotFollow(recs) ++ left.collectShouldNotFollow(recs)
        case None => right.collectShouldNotFollow(recs)
      }
      override protected def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean = {
        left.collectCalledLeft(id, recs) || (left.nullable.nonEmpty && right.collectCalledLeft(id, recs))
      }
      override protected def collectIsLL1(recs: Set[AnyRef]): Boolean = {
        left.collectIsLL1(recs) && right.collectIsLL1(recs) &&
        (left.shouldNotFollow & right.first).isEmpty
      }
      override protected def collectLL1Conflicts(recs: Set[AnyRef]): Set[LL1Conflict] = {
        val ss = sequents(this)

        val followConflicts: Seq[LL1Conflict] = for {
          (lhs, rhs) <- ss.zip(ss.tail)
          ambiguities = lhs.shouldNotFollow & rhs.first
          if ambiguities.nonEmpty
        } yield FollowConflict(ambiguities, lhs, rhs)

        ss.flatMap(_.collectLL1Conflicts(recs)).toSet union followConflicts.toSet
      }
      override protected def collectIsProductive(recs: Set[AnyRef]): Boolean =
        left.collectIsProductive(recs) && right.collectIsProductive(recs)
    }

    /** Parser that sequences the `left` and `right` parsers and groups the results. */
    case class Sequence[+A, +B](left: Parser[A], right: Parser[B]) extends Parser[A ~ B] with SequenceLike[A, B] {
      override lazy val isProductive: Boolean = left.isProductive && right.isProductive
      override lazy val nullable: Option[A ~ B] = for {
        leftValue <- left.nullable
        rightValue <- right.nullable
      } yield scallion.parsing.~(leftValue, rightValue)
      override protected def collectNullable(recs: Set[AnyRef]): Option[A ~ B] = for {
        leftValue <- left.collectNullable(recs)
        rightValue <- right.collectNullable(recs)
      } yield scallion.parsing.~(leftValue, rightValue)
      override protected def repr(level: Int, recs: Map[AnyRef, String]): String = {
        val l = left.repr(9, recs)
        val r = right.repr(10, recs)

        if (level > 9) {
          "(" + l + " ~ " + r + ")"
        }
        else {
          l + " ~ " + r
        }
      }
      override def derive(token: Token, kind: Kind): Parser[A ~ B] = {
        val derived = left.derive(token, kind)

        if (!derived.isProductive) {
          left.nullable match {
            case Some(leftValue) => Success(leftValue) ~ right.derive(token, kind)
            case None => Failure
          }
        }
        else {
          derived ~ right
        }
      }
    }

    /** Parser that sequences the `left` and `right` parsers and concatenates the results. */
    case class Concat[+A](left: Parser[Seq[A]], right: Parser[Seq[A]]) extends Parser[Seq[A]] with SequenceLike[Seq[A], Seq[A]] {
      override lazy val isProductive: Boolean = left.isProductive && right.isProductive
      override lazy val nullable: Option[Seq[A]] = for {
        leftValue <- left.nullable
        rightValue <- right.nullable
      } yield leftValue ++ rightValue
      override protected def collectNullable(recs: Set[AnyRef]): Option[Seq[A]] = for {
        leftValue <- left.collectNullable(recs)
        rightValue <- right.collectNullable(recs)
      } yield leftValue ++ rightValue
      override protected def repr(level: Int, recs: Map[AnyRef, String]): String = {
        val l = left.repr(7, recs)
        val r = right.repr(8, recs)

        if (level > 7) {
          "(" + l + " ++ " + r + ")"
        }
        else {
          l + " ++ " + r
        }
      }
      override def derive(token: Token, kind: Kind): Parser[Seq[A]] = {
        val derived = left.derive(token, kind)

        if (!derived.isProductive) {
          left.nullable match {
            case Some(leftValue) => Success(leftValue) ++ right.derive(token, kind)
            case None => Failure
          }
        }
        else {
          derived ++ right
        }
      }
    }

    /** Parser that acts either as the disjunction of the `left` and `right` parsers. */
    case class Disjunction[+A](left: Parser[A], right: Parser[A]) extends Parser[A] {
      private lazy val order = if (right.nullable.nonEmpty) (left, right) else (right, left)
      private lazy val firstFirst = order._1.first

      override lazy val nullable: Option[A] = left.nullable orElse right.nullable
      override lazy val isProductive: Boolean = left.isProductive || right.isProductive
      override protected def collectNullable(recs: Set[AnyRef]): Option[A] =
        left.collectNullable(recs) orElse right.collectNullable(recs)
      override protected def collectFirst(recs: Set[AnyRef]): Set[Kind] =
        left.collectFirst(recs) ++ right.collectFirst(recs)
      override protected def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] = {
        val fromLeft: Set[Kind] = if (right.nullable.nonEmpty) left.first else ListSet()
        val fromRight: Set[Kind] = if (left.nullable.nonEmpty) right.first else ListSet()

        fromRight ++ fromLeft ++ left.collectShouldNotFollow(recs) ++ right.collectShouldNotFollow(recs)
      }
      override protected def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean =
        left.collectCalledLeft(id, recs) || right.collectCalledLeft(id, recs)
      override protected def collectIsLL1(recs: Set[AnyRef]): Boolean =
        left.collectIsLL1(recs) && right.collectIsLL1(recs) &&
        (left.nullable.isEmpty || right.nullable.isEmpty) &&
        (left.first & right.first).isEmpty
      override protected def collectLL1Conflicts(recs: Set[AnyRef]): Set[LL1Conflict] = {
        val ds = disjuncts(this)

        val firstConflicts: Seq[LL1Conflict] = for {
          i <- 0 until (ds.size - 1)
          j <- (i + 1) until ds.size
          lhs = ds(i)
          rhs = ds(j)
          ambiguities = lhs.first & rhs.first
          if ambiguities.nonEmpty
        } yield FirstConflict(ambiguities, lhs, rhs)

        val nullableConflicts: Set[LL1Conflict] =
          if (ds.count(_.nullable.nonEmpty) > 1) Set(NullableConflict(this)) else Set()

        ds.flatMap(_.collectLL1Conflicts(recs)).toSet union firstConflicts.toSet union nullableConflicts
      }
      override protected def collectIsProductive(recs: Set[AnyRef]): Boolean =
        left.collectIsProductive(recs) || right.collectIsProductive(recs)
      override def derive(token: Token, kind: Kind): Parser[A] = {
        if (firstFirst.contains(kind)) {
          order._1.derive(token, kind)
        }
        else {
          order._2.derive(token, kind)
        }
      }
      override protected def repr(level: Int, recs: Map[AnyRef, String]): String = {
        val l = left.repr(1, recs)
        val r = right.repr(2, recs)

        if (level > 1) {
          "(" + l + " | " + r + ")"
        }
        else {
          l + " | " + r
        }
      }
    }

    /** Parser that may recursively call itself. */
    case class Recursive[+A](computation: () => Parser[A]) extends Parser[A] {
      lazy val inner: Parser[A] = computation()

      override lazy val nullable: Option[A] = inner.collectNullable(Set(this))
      override lazy val isProductive: Boolean = inner.collectIsProductive(Set(this))

      override protected def collectNullable(recs: Set[AnyRef]): Option[A] =
        if (recs.contains(this)) None else inner.collectNullable(recs + this)
      override protected def collectFirst(recs: Set[AnyRef]): Set[Kind] =
        if (recs.contains(this)) ListSet() else inner.collectFirst(recs + this)
      override protected def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] =
        if (recs.contains(this)) ListSet() else inner.collectShouldNotFollow(recs + this)
      override protected def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean =
        if (recs.contains(this)) false else (this eq id) || inner.collectCalledLeft(id, recs + this)
      override protected def collectIsLL1(recs: Set[AnyRef]): Boolean =
        if (recs.contains(this)) true else !inner.calledLeft(this) && inner.collectIsLL1(recs + this)
      override protected def collectLL1Conflicts(recs: Set[AnyRef]): Set[LL1Conflict] =
        if (recs.contains(this)) ListSet() else {
          val base = inner.collectLL1Conflicts(recs + this)

          if (inner.calledLeft(this)) {
            base + LeftRecursiveConflict(this)
          }
          else {
            base
          }
        }
      override protected def collectIsProductive(recs: Set[AnyRef]): Boolean =
        if (recs.contains(this)) false else inner.collectIsProductive(recs + this)
      override def derive(token: Token, kind: Kind): Parser[A] =
        inner.derive(token, kind)
      override protected def repr(level: Int, recs: Map[AnyRef, String]): String = {
        recs.get(this) match {
          case None => {
            val n = (recs.size + 1).toString
            "recursive<" + n + ">(" + inner.repr(0, recs + (this -> n)) + ")"
          }
          case Some(n) => "<" + n + ">"
        }
      }
    }

    private def disjuncts(parser: Parser[Any]): Seq[Parser[Any]] = parser match {
      case Disjunction(lhs, rhs) => disjuncts(lhs) ++ disjuncts(rhs)
      case _ => Seq(parser)
    }

    private def sequents(parser: Parser[Any]): Seq[Parser[Any]] =
      if (parser.isInstanceOf[SequenceLike[_, _]]) {
        val seqLike = parser.asInstanceOf[SequenceLike[Any, Any]]

        sequents(seqLike.left) ++ sequents(seqLike.right)
      }
      else {
        Seq(parser)
      }
  }

  /** Parser that accepts tokens of the provided `kind`.
    *
    * @group combinator
    */
  def elem(kind: Kind): Parser[Token] = Elem(kind)

  /** Parser that accepts tokens of the provided `kind`.
    * A function directly is applied on the successfully matched token.
    *
    * @group combinator
    */
  def accept[A](kind: Kind)(function: PartialFunction[Token, A]): Parser[A] = elem(kind).map(function)

  /** Indicates that the parser can be recursively invoke itself.
    *
    * @group combinator
    */
  def recursive[A](parser: => Parser[A]): Parser[A] = Recursive(() => parser)

  /** Parser that produces the given `value` without consuming any input.
    *
    * @group combinator
    */
  def epsilon[A](value: A): Parser[A] = Success(value)

  /** Parser that always fails.
    *
    * @group combinator
    */
  def failure[A]: Parser[A] = Failure

  /** Parser that represents 0 or 1 instances of the `parser`.
    *
    * @group combinator
    */
  def opt[A](parser: Parser[A]): Parser[Option[A]] = parser.opt

  /** Parser that represents 0 or more repetitions of the `rep` parser.
    *
    * @group combinator
    */
  def many[A](rep: Parser[A]): Parser[Seq[A]] = {
    lazy val rest: Parser[Seq[A]] = recursive(rep +: rest | epsilon(Vector()))
    rest
  }

  /** Parser that represents 1 or more repetitions of the `rep` parser.
    *
    * @group combinator
    */
  def many1[A](rep: Parser[A]): Parser[Seq[A]] = rep +: many(rep)

  /** Parser that represents 0 or more repetitions of the `rep` parser, separated by `sep`.
    *
    * @group combinator
    */
  def repsep[A](rep: Parser[A], sep: Parser[Any]): Parser[Seq[A]] = rep1sep(rep, sep) | epsilon(Vector())

  /** Parser that represents 1 or more repetitions of the `rep` parser, separated by `sep`.
    *
    * @group combinator
    */
  def rep1sep[A](rep: Parser[A], sep: Parser[Any]): Parser[Seq[A]] = {
    lazy val rest: Parser[Seq[A]] = recursive((sep ~>~ rep) +: rest | epsilon(Vector()))
    rep +: rest
  }

  /** Parser that represents the disjunction of several `parsers`.
    *
    * @group combinator
    */
  def oneOf[A](parsers: Parser[A]*): Parser[A] = {
    var queue = parsers.toVector :+ failure[A]

    while (queue.size > 1) {
      val a = queue(0)
      val b = queue(1)
      queue = queue.drop(2)
      queue :+= a | b
    }

    queue.head
  }
}


/** Contains utilities to write parsers with infix, prefix and postfix operators.
  * Expected to be mixed-in to `Parsers`.
  *
  * @groupprio assoc 1
  * @groupname assoc Associativity
  *
  * @groupprio level 2
  * @groupname level Priority Levels
  *
  * @groupprio combinator 3
  * @groupname combinator Combinators
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

  /** Parser that parses `elem` prefixed by an number of `op`.
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

  /** Parser that parses `elem` postfixed by an number of `op`.
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
