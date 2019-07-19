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

import scala.language.implicitConversions

import scala.collection.immutable.ListSet
import scala.util.Try

import scallion.util.internal.{Producer, ProducerOps}

/** Contains definitions relating to parsers.
  *
  * @see See trait [[scallion.parsing.Operators]] for useful combinators
  *      to describe infix, prefix and postfix operators.
  *
  * @group parsing
  */
trait Parsers[Token, Kind]
    extends visualization.Graphs[Kind]
       with visualization.Grammars[Kind] {

  import Parser._

  type InvParser[A] = Parser[A, A]

  /** Returns the kind associated with `token`.
    *
    * @group abstract
    */
  def getKind(token: Token): Kind

  /** Sequence of token kinds.
    *
    * @group other
    */
  protected type Trail = Seq[Kind]

  /** Contains utilities to build trails.
    *
    * @group other
    */
  private object Trail {

    /** The empty trail. */
    val empty: Trail = Vector()

    /** Returns a trail containing a single `kind`. */
    def single(kind: Kind): Trail = Vector(kind)
  }

  /** Contains utilies to produce trails. */
  private object trailOps extends ProducerOps[Trail] {

    /** Concatenation of trails. */
    override def join(left: Trail, right: Trail): Trail =
      left ++ right

    /** Comparison of trails by size. */
    override def lessEquals(left: Trail, right: Trail): Boolean =
      left.size <= right.size
  }

  private object tokenSeqOps extends ProducerOps[Seq[Token]] {

    /** Concatenation of token sequences. */
    override def join(left: Seq[Token], right: Seq[Token]): Seq[Token] =
      left ++ right

    /** Comparison of trails by size. */
    override def lessEquals(left: Seq[Token], right: Seq[Token]): Boolean =
      left.size <= right.size
  }

  /** Consumes a stream of tokens and tries to produces a value of type `A`.
    *
    * @group parser
    *
    * @groupprio subparser 2
    * @groupname subparser Member Parsers
    *
    * @groupprio parsing 5
    * @groupname parsing Parsing
    *
    * @groupprio complete 6
    * @groupname complete Completions
    *
    * @groupprio property 7
    * @groupname property Properties
    */
  sealed trait Parser[-V, +A] {

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

    /** Returns all of kinds that should not be accepted
      * as the next token by a subsequent parser.
      *
      * The value associated to the kind is a parser that accepts
      * all up until that kind.
      *
      * @group property
      */
    @inline def shouldNotFollow: Map[Kind, Parser[Nothing, Any]] = collectShouldNotFollow(ListSet())

    /** Checks if a `Recursive` parser can be entered without consuming input first.
      *
      * @param rec The `Recursive` parser.
      *
      * @group property
      */
    @inline def calledLeft(rec: Recursive[Nothing, Any]): Boolean = collectCalledLeft(rec, ListSet())

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

    /** Returns all possible sequences of accepted token kinds in increasing size.
      *
      * @group property
      */
    @inline def trails: Iterator[Seq[Kind]] = collectTrails(Map.empty).toIterator

    /** Returns a parser that behaves like `this` parser but rejects all tokens whose kind does
      * not satisfy the given predicate.
      *
      * @param predicate The predicate that kinds must satisfy.
      *
      * @group combinator
      */
    @inline def filter(predicate: Kind => Boolean): Parser[V, A] = collectFilter(predicate, Map.empty)

    /** Returns the set of all kinds that appear somewhere in `this` parser.
      *
      * @group property
      */
    @inline def kinds: Set[Kind] = collectKinds(ListSet())

    def tokensOf(value: V): Iterator[Seq[Token]] =
      collectTokens(value, Map.empty).toIterator

    // All the functions below have an argument `recs` which
    // contains the set of all `Recursive` parser on which the call
    // was already performed.
    //
    // This is done to handle the potentially cyclic structure of parsers
    // introduced by `Recursive`.


    /** Collects the nullable value from this parser.
      *
      * @param recs The identifiers of already visited `Recursive` parsers.
      */
    protected def collectNullable(recs: Set[RecId]): Option[A]

    /** Collects the "first" set from this parser.
      *
      * @param recs The identifiers of already visited `Recursive` parsers.
      */
    protected def collectFirst(recs: Set[RecId]): Set[Kind]

    /** Collects the "should-not-follow" set from this parser.
      *
      * @param recs The identifiers of already visited `Recursive` parsers.
      */
    protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Parser[Nothing, Any]]

    /** Checks if the recusive parser `rec` can be invoked without consuming any input tokens.
      *
      * @param rec  The recursive parser.
      * @param recs The identifiers of already visited `Recursive` parsers.
      */
    protected def collectCalledLeft(rec: Recursive[Nothing, Any], recs: Set[RecId]): Boolean

    /** Checks if this parser is productive.
      *
      * @param recs The identifiers of already visited `Recursive` parsers.
      */
    protected def collectIsProductive(recs: Set[RecId]): Boolean

    /** Checks if this parser is LL(1).
      *
      * @param recs The identifiers of already visited `Recursive` parsers.
      */
    protected def collectIsLL1(recs: Set[RecId]): Boolean

    /** Collects the LL(1) conflicts from this parser.
      *
      * @param recs The identifiers of already visited `Recursive` parsers.
      */
    protected def collectLL1Conflicts(recs: Set[RecId]): Set[LL1Conflict]

    /** Builds a producer of traces from this parser.
      *
      * @param recs The identifiers of already visited `Recursive` parsers.
      */
    protected def collectTrails(recs: Map[RecId, () => Producer[Trail]]): Producer[Trail]

    /** Builds a parser that filter out unwanted kinds.
      *
      * @param predicate Predicate that kinds must satisfy.
      * @param recs      The identifiers of already visited `Recursive` parsers.
      */
    protected def collectFilter(predicate: Kind => Boolean, recs: Map[RecId, Parser[Nothing, Any]]): Parser[V, A]

    /** Collects all kinds appearing in this parser.
      *
      * @param recs The identifiers of already visited `Recursive` parsers.
      */
    protected def collectKinds(recs: Set[RecId]): Set[Kind]


    protected def collectTokens(value: V, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]]

    /** Feeds a token to the parser and obtain a parser for the rest of input.
      *
      * @group parsing
      */
    def derive(token: Token, kind: Kind): Parser[V, A]


    /** String representation of the parser.
      *
      * @group other
      */
    override def toString = repr(0, Map.empty)

    /** Computes a friendlier string representation for the parser. */
    protected def repr(level: Int, recs: Map[RecId, String]): String


    // Combinators.

    /** Applies a function to the parsed values.
      *
      * @see See the [[transform]] combinator for applying inversible functions.
      *
      * @group combinator
      */
    def map[B](function: A => B): Parser[V, B] =
      this match {
        case Failure => Failure
        case Success(value) => Success(function(value))
        case Transform(otherFunction, otherInverse, inner) =>
          Transform(
            otherFunction andThen function,
            otherInverse,
            inner)
        case inner => Transform(function, (v: V) => Seq(v), inner)
      }

    def contramap[W](inverse: W => Seq[V]): Parser[W, A] =
      this match {
        case Failure => Failure
        case Success(value) => Success(value)
        case Transform(otherFunction, otherInverse, inner) =>
          Transform(
            otherFunction,
            (z: W) => Try(inverse(z)).getOrElse(Seq()).flatMap((y: V) => otherInverse(y)),
            inner)
        case inner => Transform((x: A) => x, inverse, inner)
      }

    def bimap[W, B](function: A => B, inverse: W => Seq[V]): Parser[W, B] =
      this match {
        case Failure => Failure
        case Success(value) => Success(function(value))
        case Transform(otherFunction, otherInverse, inner) =>
          Transform(
            otherFunction andThen function,
            (z: W) => Try(inverse(z)).getOrElse(Seq()).flatMap((y: V) => otherInverse(y)),
            inner)
        case inner => Transform(function, inverse, inner)
      }

    /** Sequences `this` and `that` parser. The parsed values are concatenated.
      *
      * @group combinator
      */
    def ++[W, B](that: Parser[Seq[W], Seq[B]])
        (implicit ev1: Parser[V, A] <:< Parser[Seq[W], Seq[B]],
                  ev2: Seq[W] <:< V,
                  ev3: A <:< Seq[B]): Parser[Seq[W], Seq[B]] =
      (this, that) match {
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
    def ~>~[W, B](that: Parser[W, B])(implicit ev: Unit <:< V): Parser[W, B] =
      this.~(that).bimap(_._2, {
      case x => Seq(scallion.parsing.~(ev(()), x))
    })

    /** Sequences `this` and `that` parser. The parsed value from `this` is returned.
      *
      * @group combinator
      */
    def ~<~(that: Parser[Unit, Any]): Parser[V, A] = this.~(that).bimap(_._1, {
      case x => Seq(scallion.parsing.~(x, ()))
    })

    /** Sequences `this` and `that` parser. The parsed value from `that` is appended to that from `this`.
      *
      * @group combinator
      */
    def :+[W, B](that: Parser[W, B])
        (implicit ev1: Parser[V, A] <:< Parser[Seq[W], Seq[B]],
                  ev2: Seq[W] <:< V,
                  ev3: A <:< Seq[B]): Parser[Seq[W], Seq[B]] =
      this ++ that.bimap(Vector(_), {
        case Seq(x) => Seq(x)
        case _ => Seq()
      })

    /** Sequences `this` and `that` parser. The parsed value from `that` is prepended to that from `this`.
      *
      * @group combinator
      */
    def +:[W, B](that: Parser[W, B])
        (implicit ev1: Parser[V, A] <:< Parser[Seq[W], Seq[B]],
                  ev2: Seq[W] <:< V,
                  ev3: A <:< Seq[B]): Parser[Seq[W], Seq[B]] =
      that.bimap(Vector(_), {
        (xs: Seq[W]) => if (xs.size == 1) xs else Seq()
      }) ++ this

    /** Sequences `this` and `that` parser. The parsed values are returned as a pair.
      *
      * @group combinator
      */
    def ~[W, X <: W, B](that: Parser[W, B]): Parser[V ~ X, A ~ B] = (this, that) match {
      case (Failure, _) => Failure
      case (_, Failure) => Failure
      case (Success(a), Success(b)) => Success(scallion.parsing.~(a, b))
      case _ => Sequence(this, that)
    }

    /** Disjunction of `this` and `that` parser.
      *
      * @group combinator
      */
    def |[W <: V, B >: A](that: Parser[W, B]): Parser[W, B] = (this, that) match {
      case (Failure, _) => that
      case (_, Failure) => this
      case _ => Disjunction(this, that)
    }

    /** Disjunction of `this` and `that` parser.
      * The value is tagged to indicate the side which produced it.
      *
      * @group combinator
      */
    def ||[W, B](that: Parser[W, B]): Parser[Either[V, W], Either[A, B]] =
      this.bimap(Left(_), {
        (e: Either[V, W]) => e match {
          case Left(x) => Seq(x)
          case Right(_) => Seq()
        }
      }) | that.bimap(Right(_), {
        (e: Either[V, W]) => e match {
          case Left(_) => Seq()
          case Right(x) => Seq(x)
        }
      })

    /** Makes the parser nullable.
      *
      * @group combinator
      */
    def opt: Parser[Option[V], Option[A]] = this.bimap(Some(_), {
      (o: Option[V]) => o match {
        case Some(x) => Seq(x)
        case None => Seq()
      }
    }) | epsilon(None)

    def unit(defaults: V*): Parser[Unit, A] = this.contramap {
      (_: Unit) => defaults
    }

    def always(defaults: V*): Parser[Unit, Unit] = this.bimap(_ => (), {
      case () => defaults
    })

    def void[W]: Parser[W, A] = this.contramap {
      (_: W) => Seq()
    }

    // Parsing.

    /** Consumes a sequence of tokens and parses into a value.
      * When `this` parser is not LL(1), the result is unspecified.
      *
      * @group parsing
      */
    def apply(it: Iterator[Token]): ParseResult[V, A] = {

      var parser: Parser[V, A] = this
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


    // Completions.

    /** Returns all possible completions of `this` parser,
      * ordered by increasing number of tokens.
      *
      * When `this` parser is not LL(1), the result is unspecified.
      *
      * @param toTokens Computes the possible tokens for a given kind.
      *
      * @group complete
      */
    def completions(toTokens: Kind => Seq[Token]): Iterator[Parser[V, A]] = {

      val kindTokens: Map[Kind, Seq[Token]] =
        kinds.toSeq.map(kind => kind -> toTokens(kind)).toMap

      val unwantedKinds: Set[Kind] =
        kindTokens.filter(_._2.isEmpty).keySet

      val cleanedParser =
        if (unwantedKinds.isEmpty) {
          this
        }
        else {
          this.filter(k => !unwantedKinds.contains(k))
        }

      cleanedParser.trails.flatMap { kinds =>
        val choices = kinds.map(kindTokens)

        def go(elems: Seq[Seq[Token]]): Seq[List[Token]] =
          if (elems.isEmpty) {
            Seq(List())
          }
          else for {
            token <- elems.head
            rest <- go(elems.tail)
          } yield token :: rest

        go(choices).map { tokens =>
          apply(tokens.toIterator).parser
        }
      }
    }

    /** Returns the smallest completion of `this` parser that can
      * be obtained using the partial `toToken` function, if any.
      *
      * When `this` parser is not LL(1), the result is unspecified.
      *
      * @param toToken Computes the preferred token of the given class, if any.
      *
      * @group complete
      */
    def complete(toToken: PartialFunction[Kind, Token]): Parser[V, A] = {
      val it = completions(kind => toToken.lift(kind).toSeq)
      if (it.hasNext) {
        it.next()
      }
      else {
        Failure
      }
    }
  }

  /** Result of running a `Parser`.
    *
    * @group result
    */
  sealed trait ParseResult[-V, +A] {

    /** Parser for the rest of input. */
    val parser: Parser[V, A]

    /** Returns the parsed value, if any. */
    def getValue: Option[A] = this match {
      case Parsed(value, _) => Some(value)
      case _ => None
    }
  }

  /** Indicates that the input has been fully processed, resulting in a `value`.
    *
    * A `parser` for subsequent input is also provided.
    *
    * @group result
    */
  case class Parsed[-V, +A](value: A, parser: Parser[V, A]) extends ParseResult[V, A]

  /** Indicates that the provided `token` was not expected at that point.
    *
    * The `parser` that rejected the token is returned.
    *
    * @group result
    */
  case class UnexpectedToken[-V, +A](token: Token, parser: Parser[V, A]) extends ParseResult[V, A]

  /** Indicates that end of input was unexpectedly encountered.
    *
    * The `parser` for subsequent input is provided.
    *
    * @group result
    */
  case class UnexpectedEnd[-V, +A](parser: Parser[V, A]) extends ParseResult[V, A]

  /** Describes a LL(1) conflict.
    *
    * @group conflict
    */
  sealed trait LL1Conflict {

    /** Source of the conflict. */
    val source: Parser[Nothing, Any]

    /** Parser for a prefix before the conflict occurs. */
    val prefix: Parser[Nothing, Any]

    private[parsing] def addPrefix(parser: Parser[Nothing, Any]): LL1Conflict

    /** Returns trails that witness the conflict. */
    def witnesses: Iterator[Trail]
  }

  /** Contains the description of the various LL(1) conflicts.
    *
    * @group conflict
    */
  object LL1Conflict {

    /** Indicates that both branches of a disjunction are nullable. */
    case class NullableConflict(
        prefix: Parser[Nothing, Any],
        source: Disjunction[Nothing, Any]) extends LL1Conflict {

      override private[parsing] def addPrefix(start: Parser[Nothing, Any]): NullableConflict =
        this.copy(prefix = start ~ prefix)

      override def witnesses: Iterator[Trail] = prefix.trails
    }

    /** Indicates that two branches of a disjunction share the same first token(s). */
    case class FirstConflict(
        prefix: Parser[Nothing, Any],
        ambiguities: Set[Kind],
        source: Disjunction[Nothing, Any]) extends LL1Conflict {

      override private[parsing] def addPrefix(start: Parser[Nothing, Any]): FirstConflict =
        this.copy(prefix = start ~ prefix)

      override def witnesses: Iterator[Trail] = for {
        trail <- prefix.trails
        kind <- ambiguities
      } yield trail :+ kind
    }

    /** Indicates that the right end side first token set conflicts with the left end side. */
    case class FollowConflict(
        prefix: Parser[Nothing, Any],
        ambiguities: Set[Kind],
        source: Parser[Nothing, Any] with SequenceLike[Nothing, Nothing, Any, Any]) extends LL1Conflict {

      override private[parsing] def addPrefix(start: Parser[Nothing, Any]): FollowConflict =
        this.copy(prefix = start ~ prefix)

      override def witnesses: Iterator[Trail] = for {
        trail <- prefix.trails
        kind <- ambiguities
      } yield trail :+ kind
    }

    /** Indicates that the parser recursively calls itself in a left position. */
    case class LeftRecursiveConflict(
        prefix: Parser[Nothing, Any],
        source: Recursive[Nothing, Any]) extends LL1Conflict {

      override private[parsing] def addPrefix(start: Parser[Nothing, Any]): LeftRecursiveConflict =
        this.copy(prefix = start ~ prefix)

      override def witnesses: Iterator[Trail] = prefix.trails
    }
  }

  import LL1Conflict._

  /** Contains primitive basic parsers and parser combinators.
    *
    * @group parser
    */
  object Parser {

    /** Parser that produces `value` without consuming input tokens.
      *
      * @param value The value produced by the parser.
      *
      * @group basic
      */
    case class Success[+A](value: A) extends Parser[Any, A] {

      override val nullable: Option[A] =
        Some(value)

      override val isProductive: Boolean =
        true

      override protected def collectNullable(recs: Set[RecId]): Option[A] =
        Some(value)

      override protected def collectFirst(recs: Set[RecId]): Set[Kind] =
        ListSet()

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Parser[Nothing, Any]] =
        Map.empty

      override protected def collectCalledLeft(rec: Recursive[Nothing, Any], recs: Set[RecId]): Boolean =
        false

      override protected def collectIsLL1(recs: Set[RecId]): Boolean =
        true

      override protected def collectLL1Conflicts(recs: Set[RecId]): Set[LL1Conflict] =
        ListSet()

      override protected def collectIsProductive(recs: Set[RecId]): Boolean =
        true

      override protected def collectTrails(recs: Map[RecId, () => Producer[Trail]]): Producer[Trail] =
        Producer.single(Trail.empty)

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Parser[Nothing, Any]]): Parser[Any, A] =
        this

      override protected def collectKinds(recs: Set[RecId]): Set[Kind] =
        ListSet()

      override protected def collectTokens(
          other: Any, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]] =
        if (value == other) Producer.single(Vector()) else Producer.empty

      override def derive(token: Token, kind: Kind): Parser[Any, A] =
        Failure

      override protected def repr(level: Int, recs: Map[RecId, String]): String =
        "epsilon(" + value.toString + ")"
    }

    /** Parser that produces `value` without consuming input tokens.
      *
      * @group basic
      */
    case object Failure extends Parser[Any, Nothing] {

      override val nullable: Option[Nothing] =
        None

      override val isProductive: Boolean =
        false

      override protected def collectNullable(recs: Set[RecId]): Option[Nothing] =
        None

      override protected def collectFirst(recs: Set[RecId]): Set[Kind] =
        ListSet()

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Parser[Nothing, Any]] =
        Map.empty

      override protected def collectCalledLeft(rec: Recursive[Nothing, Any], recs: Set[RecId]): Boolean =
        false

      override protected def collectIsLL1(recs: Set[RecId]): Boolean =
        true

      override protected def collectLL1Conflicts(recs: Set[RecId]): Set[LL1Conflict] =
        ListSet()

      override protected def collectIsProductive(recs: Set[RecId]): Boolean =
        false

      override protected def collectTrails(recs: Map[RecId, () => Producer[Trail]]): Producer[Trail] =
        Producer.empty

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Parser[Nothing, Any]]): Parser[Any, Nothing] =
        this

      override protected def collectKinds(recs: Set[RecId]): Set[Kind] =
        ListSet()

      override protected def collectTokens(
            value: Any, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]] =
        Producer.empty

      override def derive(token: Token, kind: Kind): Parser[Any, Nothing] =
        this

      override protected def repr(level: Int, recs: Map[RecId, String]): String =
        "failure"
    }

    /** Parser that consumes tokens of the given `kind`.
      *
      * @param kind The kind accepted by the parser.
      *
      * @group basic
      */
    case class Elem(kind: Kind) extends Parser[Token, Token] {

      override val nullable: Option[Token] =
        None

      override val isProductive: Boolean =
        true

      override protected def collectNullable(recs: Set[RecId]): Option[Token] =
        None

      override protected def collectFirst(recs: Set[RecId]): Set[Kind] =
        Set(kind)

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Parser[Nothing, Any]] =
        Map.empty

      override protected def collectCalledLeft(rec: Recursive[Nothing, Any], recs: Set[RecId]): Boolean =
        false

      override protected def collectIsLL1(recs: Set[RecId]): Boolean =
        true

      override protected def collectLL1Conflicts(recs: Set[RecId]): Set[LL1Conflict] =
        ListSet()

      override protected def collectIsProductive(recs: Set[RecId]): Boolean =
        true

      override protected def collectTrails(recs: Map[RecId, () => Producer[Trail]]): Producer[Trail] =
        Producer.single(Trail.single(kind))

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Parser[Nothing, Any]]): Parser[Token, Token] =
        if (predicate(kind)) this else Failure

      override protected def collectKinds(recs: Set[RecId]): Set[Kind] =
        ListSet(kind)

      override protected def collectTokens(
          value: Token, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]) : Producer[Seq[Token]] =
        if (getKind(value) == kind) Producer.single(Vector(value)) else Producer.empty

      override def derive(token: Token, tokenKind: Kind): Parser[Token, Token] =
        if (tokenKind == kind) Success(token) else Failure

      override protected def repr(level: Int, recs: Map[RecId, String]): String =
        "elem(" + kind + ")"
    }

    /** Unary combinator.
      *
      * @group combinator
      */
    sealed trait Unary[-V, +A] { self: Parser[_, _] =>

      /** The inner parser.
        *
        * @group subparser
        */
      def inner: Parser[V, A]
    }

    /** Binary combinator.
      *
      * @group combinator
      */
    sealed trait Binary[-V, -W, +A, +B] { self: Parser[_, _] =>

      /** The left-hand side parser.
        *
        * @group subparser
        */
      def left: Parser[V, A]

      /** The right-hand side parser.
        *
        * @group subparser
        */
      def right: Parser[W, B]
    }

    /** Parser that applies a `function` on the parsed value of the `inner` parser.
      *
      * @param function The function to apply on produced values.
      * @param inner    The inner parser.
      *
      * @group combinator
      */
    case class Transform[V, W, A, B](
        function: A => B,
        inverse: W => Seq[V],
        inner: Parser[V, A]) extends Parser[W, B] with Unary[V, A] {

      override lazy val nullable: Option[B] =
        inner.nullable.map(function)

      override lazy val isProductive: Boolean =
        inner.isProductive

      override protected def collectNullable(recs: Set[RecId]): Option[B] =
        inner.collectNullable(recs).map(function)

      override protected def collectFirst(recs: Set[RecId]): Set[Kind] =
        inner.collectFirst(recs)

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Parser[Nothing, Any]] =
        inner.collectShouldNotFollow(recs)

      override protected def collectCalledLeft(rec: Recursive[Nothing, Any], recs: Set[RecId]): Boolean =
        inner.collectCalledLeft(rec, recs)

      override protected def collectIsLL1(recs: Set[RecId]): Boolean =
        inner.collectIsLL1(recs)

      override protected def collectLL1Conflicts(recs: Set[RecId]): Set[LL1Conflict] =
        inner.collectLL1Conflicts(recs)

      override protected def collectIsProductive(recs: Set[RecId]): Boolean =
        inner.collectIsProductive(recs)

      override protected def collectTrails(recs: Map[RecId, () => Producer[Trail]]): Producer[Trail] =
        inner.collectTrails(recs)

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Parser[Nothing, Any]]): Parser[W, B] =
        inner.collectFilter(predicate, recs).bimap(function, inverse)

      override protected def collectKinds(recs: Set[RecId]): Set[Kind] =
        inner.collectKinds(recs)

      override def derive(token: Token, kind: Kind): Parser[W, B] =
        inner.derive(token, kind).bimap(function, inverse)

      override protected def collectTokens(
        value: W, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]] = {

        val producers = inverse(value).map(inversed => inner.collectTokens(inversed, recs))

        if (producers.isEmpty) {
          Producer.empty
        }
        else {
          producers.reduceLeft(tokenSeqOps.union(_, _))
        }
      }

      override protected def repr(level: Int, recs: Map[RecId, String]): String =
        inner.repr(10, recs) + ".map(<function>)"
    }

    /** Parser that sequences the `left` and `right` parsers.
      *
      * @group combinator
      */
    sealed trait SequenceLike[-V, -W, +A, +B] extends Binary[V, W, A, B] { self: Parser[_, _] =>

      override protected def collectFirst(recs: Set[RecId]): Set[Kind] =
        left.nullable match {
          case Some(_) => left.collectFirst(recs) ++ right.collectFirst(recs)
          case None => left.collectFirst(recs)
        }

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Parser[Nothing, Any]] = {
        val rightSNF =
          right.collectShouldNotFollow(recs).map {
            case (k, v) => k -> left ~ v
          }

        right.nullable match {
          case Some(_) => combineSNF(
            left.collectShouldNotFollow(recs),
            rightSNF
          )
          case None => rightSNF
        }
      }

      override protected def collectCalledLeft(rec: Recursive[Nothing, Any], recs: Set[RecId]): Boolean =
        left.collectCalledLeft(rec, recs) || (left.nullable.nonEmpty && right.collectCalledLeft(rec, recs))

      override protected def collectIsLL1(recs: Set[RecId]): Boolean =
        left.collectIsLL1(recs) && right.collectIsLL1(recs) &&
        (left.shouldNotFollow.keySet & right.first).isEmpty

      override protected def collectLL1Conflicts(recs: Set[RecId]): Set[LL1Conflict] = {

        val leftSNF = left.shouldNotFollow

        val problematicKinds = (leftSNF.keySet & right.first)

        val followConflicts: Set[LL1Conflict] =
          if (problematicKinds.isEmpty) {
            ListSet()
          }
          else {
            problematicKinds.map { kind =>
              FollowConflict(leftSNF(kind), problematicKinds, this)
            }
          }

        val baseConflicts: Set[LL1Conflict] =
          left.collectLL1Conflicts(recs) union
          right.collectLL1Conflicts(recs).map(_.addPrefix(left))

        baseConflicts union followConflicts
      }

      override protected def collectIsProductive(recs: Set[RecId]): Boolean =
        left.collectIsProductive(recs) && right.collectIsProductive(recs)

      override protected def collectTrails(recs: Map[RecId, () => Producer[Trail]]): Producer[Trail] =
        trailOps.product(left.collectTrails(recs), right.collectTrails(recs))

      override protected def collectKinds(recs: Set[RecId]): Set[Kind] =
        left.collectKinds(recs) union right.collectKinds(recs)
    }

    /** Parser that sequences the `left` and `right` parsers and groups the results.
      *
      * @param left  The parser for the prefix.
      * @param right The parser for the suffix.
      *
      * @group combinator
      */
    case class Sequence[-V, -W, +A, +B](left: Parser[V, A], right: Parser[W, B])
        extends Parser[V ~ W, A ~ B] with SequenceLike[V, W, A, B] {

      override lazy val isProductive: Boolean =
        left.isProductive && right.isProductive

      override lazy val nullable: Option[A ~ B] = for {
        leftValue <- left.nullable
        rightValue <- right.nullable
      } yield scallion.parsing.~(leftValue, rightValue)

      override protected def collectNullable(recs: Set[RecId]): Option[A ~ B] = for {
        leftValue <- left.collectNullable(recs)
        rightValue <- right.collectNullable(recs)
      } yield scallion.parsing.~(leftValue, rightValue)

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Parser[Nothing, Any]]): Parser[V ~ W, A ~ B] =
        left.collectFilter(predicate, recs) ~ right.collectFilter(predicate, recs)

      override protected def repr(level: Int, recs: Map[RecId, String]): String = {
        val l = left.repr(9, recs)
        val r = right.repr(10, recs)

        if (level > 9) {
          "(" + l + " ~ " + r + ")"
        }
        else {
          l + " ~ " + r
        }
      }

      override protected def collectTokens(
          value: V ~ W, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]] =

        value match {
          case a ~ b => tokenSeqOps.product(left.collectTokens(a, recs), right.collectTokens(b, recs))
        }

      override def derive(token: Token, kind: Kind): Parser[V ~ W, A ~ B] = {
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

    /** Parser that sequences the `left` and `right` parsers and concatenates the results.
      *
      * @param left  The parser for the prefix.
      * @param right The parser for the suffix.
      *
      * @group combinator
      */
    case class Concat[-V, +A](left: Parser[Seq[V], Seq[A]], right: Parser[Seq[V], Seq[A]])
        extends Parser[Seq[V], Seq[A]] with SequenceLike[Seq[V], Seq[V], Seq[A], Seq[A]] {

      override lazy val isProductive: Boolean =
        left.isProductive && right.isProductive

      override lazy val nullable: Option[Seq[A]] = for {
        leftValue <- left.nullable
        rightValue <- right.nullable
      } yield leftValue ++ rightValue

      override protected def collectNullable(recs: Set[RecId]): Option[Seq[A]] = for {
        leftValue <- left.collectNullable(recs)
        rightValue <- right.collectNullable(recs)
      } yield leftValue ++ rightValue

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Parser[Nothing, Any]]): Parser[Seq[V], Seq[A]] =
        left.collectFilter(predicate, recs) ++ right.collectFilter(predicate, recs)

      override protected def repr(level: Int, recs: Map[RecId, String]): String = {
        val l = left.repr(7, recs)
        val r = right.repr(8, recs)

        if (level > 7) {
          "(" + l + " ++ " + r + ")"
        }
        else {
          l + " ++ " + r
        }
      }

      override def derive(token: Token, kind: Kind): Parser[Seq[V], Seq[A]] = {
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

      override protected def collectTokens(
          value: Seq[V], recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]] = {

        val producers = for {
          i <- 0 to value.size
          (a, b) = value.splitAt(i)
        } yield tokenSeqOps.product(left.collectTokens(a, recs), right.collectTokens(b, recs))

        if (producers.isEmpty) {
          Producer.empty
        }
        else {
          producers.reduceLeft(tokenSeqOps.union(_, _))
        }
      }
    }

    /** Parser that acts either as the disjunction of the `left` and `right` parsers.
      *
      * @param left  The parser for the first alternative.
      * @param right The parser for the second alternative.
      *
      * @group combinator
      */
    case class Disjunction[-V, +A](left: Parser[V, A], right: Parser[V, A])
        extends Parser[V, A] with Binary[V, V, A, A] {

      private lazy val order = if (right.nullable.nonEmpty) (left, right) else (right, left)
      private lazy val firstFirst = order._1.first

      override lazy val nullable: Option[A] =
        left.nullable orElse right.nullable

      override lazy val isProductive: Boolean =
        left.isProductive || right.isProductive

      override protected def collectNullable(recs: Set[RecId]): Option[A] =
        left.collectNullable(recs) orElse right.collectNullable(recs)

      override protected def collectFirst(recs: Set[RecId]): Set[Kind] =
        left.collectFirst(recs) ++ right.collectFirst(recs)

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Parser[Nothing, Any]] = {
        val fromLeft: Map[Kind, Parser[Nothing, Any]] =
          if (right.nullable.nonEmpty) {
            left.first.toSeq.map {
              kind => kind -> Success(())
            }.toMap
          }
          else {
            Map.empty
          }
        val fromRight: Map[Kind, Parser[Nothing, Any]] =
          if (left.nullable.nonEmpty) {
            right.first.toSeq.map {
              kind => kind -> Success(())
            }.toMap
          }
          else {
            Map.empty
          }

        val baseSNF = combineSNF(left.collectShouldNotFollow(recs), right.collectShouldNotFollow(recs))
        val addedSNF = combineSNF(fromLeft, fromRight)

        combineSNF(baseSNF, addedSNF)
      }

      override protected def collectCalledLeft(rec: Recursive[Nothing, Any], recs: Set[RecId]): Boolean =
        left.collectCalledLeft(rec, recs) || right.collectCalledLeft(rec, recs)

      override protected def collectIsLL1(recs: Set[RecId]): Boolean =
        left.collectIsLL1(recs) && right.collectIsLL1(recs) &&
        (left.nullable.isEmpty || right.nullable.isEmpty) &&
        (left.first & right.first).isEmpty

      override protected def collectLL1Conflicts(recs: Set[RecId]): Set[LL1Conflict] = {

        val problematicKinds = (left.first & right.first)

        val firstConflicts: Set[LL1Conflict] =
          if (problematicKinds.isEmpty) {
            ListSet()
          }
          else {
            ListSet(FirstConflict(Success(()), problematicKinds, this))
          }

        val nullableConflicts: Set[LL1Conflict] =
          if (left.nullable.isEmpty || right.nullable.isEmpty) {
            ListSet()
          }
          else {
            ListSet(NullableConflict(Success(()), this))
          }

        val baseConflicts: Set[LL1Conflict] =
          left.collectLL1Conflicts(recs) union right.collectLL1Conflicts(recs)

        baseConflicts union firstConflicts union nullableConflicts
      }

      override protected def collectIsProductive(recs: Set[RecId]): Boolean =
        left.collectIsProductive(recs) || right.collectIsProductive(recs)

      override protected def collectTrails(recs: Map[RecId, () => Producer[Trail]]): Producer[Trail] =
        trailOps.union(left.collectTrails(recs), right.collectTrails(recs))

      override def derive(token: Token, kind: Kind): Parser[V, A] = {
        if (firstFirst.contains(kind)) {
          order._1.derive(token, kind)
        }
        else {
          order._2.derive(token, kind)
        }
      }

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Parser[Nothing, Any]]): Parser[V, A] =
        left.collectFilter(predicate, recs) | right.collectFilter(predicate, recs)

      override protected def collectKinds(recs: Set[RecId]): Set[Kind] =
        left.collectKinds(recs) union right.collectKinds(recs)

      override protected def collectTokens(
          value: V, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]] =
        tokenSeqOps.union(left.collectTokens(value, recs), right.collectTokens(value, recs))

      override protected def repr(level: Int, recs: Map[RecId, String]): String = {
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

    /** Identifier for Recursive. */
    protected type RecId = Int

    /** Companion object of `Recursive`.
      *
      * @group combinator
      */
    object Recursive {
      private var freeNextId: RecId = 0

      /** Generates a fresh identifier. */
      private def nextId(): RecId = synchronized {
        val res = freeNextId
        freeNextId += 1
        res
      }

      /** Extract the inner parser of a `Recursive` parser. */
      def unapply[V, W <: V, A](that: Parser[V, A]): Option[Parser[W, A]] = {
        if (that.isInstanceOf[Recursive[_, _]]) {
          Some(that.asInstanceOf[Recursive[V, A]].inner)
        }
        else {
          None
        }
      }

      /** Creates a new `Recursive` parser.
        *
        * @param parser The inner parser.
        */
      def create[V, A](parser: => Parser[V, A]): Recursive[V, A] = new Recursive[V, A] {
        override protected val id = nextId()
        override lazy val inner: Parser[V, A] = parser
      }
    }

    /** Parser that may recursively call itself.
      *
      * @group combinator
      */
    sealed abstract class Recursive[-V, +A] extends Parser[V, A] with Unary[V, A] {

      /** Unique identifier for this recursive parser. */
      protected val id: RecId

      /** Checks if `this` is equal to `other`.
        *
        * @group other
        */
      override def equals(other: Any): Boolean =
        if (!other.isInstanceOf[Recursive[Nothing, Any]]) {
          false
        }
        else {
          val that = other.asInstanceOf[Recursive[Nothing, Any]]
          this.id == that.id
        }

      /** Returns the hash of this object.
        *
        * @group other
        */
      override def hashCode(): Int = id

      override lazy val nullable: Option[A] =
        inner.collectNullable(Set(this.id))

      override lazy val isProductive: Boolean =
        inner.collectIsProductive(Set(this.id))

      override protected def collectNullable(recs: Set[RecId]): Option[A] =
        if (recs.contains(this.id)) None else inner.collectNullable(recs + this.id)

      override protected def collectFirst(recs: Set[RecId]): Set[Kind] =
        if (recs.contains(this.id)) ListSet() else inner.collectFirst(recs + this.id)

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Parser[Nothing, Any]] =
        if (recs.contains(this.id)) Map.empty else inner.collectShouldNotFollow(recs + this.id)

      override protected def collectCalledLeft(rec: Recursive[Nothing, Any], recs: Set[RecId]): Boolean =
        if (recs.contains(this.id)) false else (this.id == rec.id) || inner.collectCalledLeft(rec, recs + this.id)

      override protected def collectIsLL1(recs: Set[RecId]): Boolean =
        if (recs.contains(this.id)) true else !inner.calledLeft(this) && inner.collectIsLL1(recs + this.id)

      override protected def collectLL1Conflicts(recs: Set[RecId]): Set[LL1Conflict] =
        if (recs.contains(this.id)) ListSet() else {
          val base = inner.collectLL1Conflicts(recs + this.id)

          if (inner.calledLeft(this)) {
            base + LeftRecursiveConflict(Success(()), this)
          }
          else {
            base
          }
        }

      override protected def collectIsProductive(recs: Set[RecId]): Boolean =
        if (recs.contains(this.id)) false else inner.collectIsProductive(recs + this.id)

      override def derive(token: Token, kind: Kind): Parser[V, A] =
        inner.derive(token, kind)

      override protected def collectTrails(recs: Map[RecId, () => Producer[Trail]]): Producer[Trail] =
        recs.get(this.id) match {
          case None => {
            lazy val pair: (Producer[Trail], () => Producer[Trail]) =
              Producer.duplicate(Producer.lazily {
                inner.collectTrails(recs + (this.id -> pair._2))
              })
            pair._1
          }
          case Some(createProducer) => createProducer()
        }

      override protected def collectTokens(
          value: V, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]] =

        recs.get((this.id, value)) match {
          case None => {
            lazy val pair: (Producer[Seq[Token]], () => Producer[Seq[Token]]) =
              Producer.duplicate(Producer.lazily {
                inner.collectTokens(value, recs + ((this.id, value) -> pair._2))
              })
            pair._1
          }
          case Some(createProducer) => createProducer()
        }

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Parser[Nothing, Any]]): Parser[V, A] = {
        recs.get(this.id) match {
          case None => {
            lazy val rec: Parser[V, A] = recursive(inner.collectFilter(predicate, recs + (this.id -> rec)))
            rec
          }
          case Some(rec) => rec.asInstanceOf[Parser[V, A]]
        }
      }

      override protected def collectKinds(recs: Set[RecId]): Set[Kind] =
        if (recs.contains(this.id)) ListSet() else inner.collectKinds(recs + this.id)

      override protected def repr(level: Int, recs: Map[RecId, String]): String = {
        recs.get(this.id) match {
          case None => {
            val n = (recs.size + 1).toString
            "recursive<" + n + ">(" + inner.repr(0, recs + (this.id -> n)) + ")"
          }
          case Some(n) => "<" + n + ">"
        }
      }
    }

    /** Combines two maps by applying a function
      * in case of conflicting entries.
      */
    private def combine[K, V](merge: (V, V) => V)(left: Map[K, V], right: Map[K, V]): Map[K, V] =
      right.foldLeft(left) {
        case (acc, (key, value)) => acc + (key -> left.get(key).map(merge(_, value)).getOrElse(value))
      }

    /** Combines two Should-Not-Follow results by taking
      * the disjunction of parser in case of conflicting entries.
      */
    private def combineSNF(
        left: Map[Kind, Parser[Nothing, Any]],
        right: Map[Kind, Parser[Nothing, Any]]): Map[Kind, Parser[Nothing, Any]] =
      combine((p1: Parser[Nothing, Any], p2: Parser[Nothing, Any]) => p1 | p2)(left, right)
  }


  // API for combinators and basic parsers.

  /** Parser that accepts tokens of the provided `kind`.
    *
    * @group basic
    */
  def elem(kind: Kind): Parser[Token, Token] = Elem(kind)

  /** Parser that accepts tokens of the provided `kind`.
    * A function directly is applied on the successfully matched token.
    *
    * @group basic
    */
  def accept[A](kind: Kind)(function: PartialFunction[Token, A]): Parser[Token, A] =
    elem(kind).map(function)

  /** Indicates that the parser can be recursively invoke itself.
    *
    * @group combinator
    */
  def recursive[V, A](parser: => Parser[V, A]): Parser[V, A] = Recursive.create(parser)

  /** Parser that produces the given `value` without consuming any input.
    *
    * @group basic
    */
  def epsilon[A](value: A): Parser[Any, A] = Success(value)

  /** Parser that always fails.
    *
    * @group basic
    */
  def failure[V, A]: Parser[V, A] = Failure

  /** Parser that represents 0 or 1 instances of the `parser`.
    *
    * @group combinator
    */
  def opt[V, A](parser: Parser[V, A]): Parser[Option[V], Option[A]] = parser.opt

  /** Parser that represents 0 or more repetitions of the `rep` parser.
    *
    * @group combinator
    */
  def many[V, A](rep: Parser[V, A]): Parser[Seq[V], Seq[A]] = {
    lazy val rest: Parser[Seq[V], Seq[A]] = recursive(rep +: rest | epsilon(Vector()))
    rest
  }

  /** Parser that represents 1 or more repetitions of the `rep` parser.
    *
    * @group combinator
    */
  def many1[V, A](rep: Parser[V, A]): Parser[Seq[V], Seq[A]] = rep +: many(rep)

  /** Parser that represents 0 or more repetitions of the `rep` parser, separated by `sep`.
    *
    * @group combinator
    */
  def repsep[V, A](rep: Parser[V, A], sep: Parser[Unit, Any]): Parser[Seq[V], Seq[A]] =
    rep1sep(rep, sep) | epsilon(Vector())

  /** Parser that represents 1 or more repetitions of the `rep` parser, separated by `sep`.
    *
    * @group combinator
    */
  def rep1sep[V, A](rep: Parser[V, A], sep: Parser[Unit, Any]): Parser[Seq[V], Seq[A]] = {
    lazy val rest: Parser[Seq[V], Seq[A]] = recursive((sep ~>~ rep) +: rest | epsilon(Vector()))
    rep +: rest
  }

  /** Parser that represents the disjunction of several `parsers`.
    *
    * @group combinator
    */
  def oneOf[V, A](parsers: Parser[V, A]*): Parser[V, A] = {
    var queue = parsers.toVector :+ failure[V, A]

    while (queue.size > 1) {
      val a = queue(0)
      val b = queue(1)
      queue = queue.drop(2)
      queue :+= a | b
    }

    queue.head
  }
}
