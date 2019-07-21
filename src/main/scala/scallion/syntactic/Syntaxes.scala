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

import scala.language.implicitConversions

import scala.collection.immutable.ListSet
import scala.util.Try

import scallion.util.internal.{Producer, ProducerOps}

/** Contains definitions relating to syntaxes.
  *
  * @see See trait [[scallion.syntactic.Operators]] for useful combinators
  *      to describe infix, prefix and postfix operators.
  *
  * @group syntax
  */
trait Syntaxes[Token, Kind]
    extends visualization.Graphs[Kind]
       with visualization.Grammars[Kind] {

  import Syntax._

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

  /** Represents a syntax.
    *
    * Acts as both a parser and a pretty printer
    * for the described language.
    *
    * @group syntax
    *
    * @groupprio subsyntax 2
    * @groupname subsyntax Member Syntaxes
    *
    * @groupprio derivation 6
    * @groupname derivation Derivations
    *
    * @groupprio complete 7
    * @groupname complete Completions
    *
    * @groupprio property 8
    * @groupname property Properties
    */
  sealed trait Syntax[-V, +A] {

    /** The value, if any, produced by this syntax without consuming more input.
      *
      * @group property
      */
    def nullable: Option[A]

    /** Indicates if there exists a sequence of tokens that the syntax can accept.
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
      * as the next token by a subsequent syntax.
      *
      * The value associated to the kind is a syntax that accepts
      * all up until that kind.
      *
      * @group property
      */
    @inline def shouldNotFollow: Map[Kind, Syntax[Nothing, Any]] = collectShouldNotFollow(ListSet())

    /** Checks if a `Recursive` syntax can be entered without consuming input first.
      *
      * @param rec The `Recursive` syntax.
      *
      * @group property
      */
    @inline def calledLeft(rec: Recursive[Nothing, Any]): Boolean = collectCalledLeft(rec, ListSet())

    /** Checks if this syntax corresponds to a LL(1) grammar.
      *
      * @group property
      */
    @inline def isLL1: Boolean = collectIsLL1(ListSet())

    /** Returns all LL(1) conflicts in the syntax.
      *
      * @group property
      */
    @inline def conflicts: Set[LL1Conflict] = collectLL1Conflicts(ListSet())

    /** Returns all possible sequences of accepted token kinds in increasing size.
      *
      * @group property
      */
    @inline def trails: Iterator[Seq[Kind]] = collectTrails(Map.empty).toIterator

    /** Returns a syntax that behaves like `this` syntax but rejects all tokens whose kind does
      * not satisfy the given predicate.
      *
      * @param predicate The predicate that kinds must satisfy.
      *
      * @group combinator
      */
    @inline def filter(predicate: Kind => Boolean): Syntax[V, A] = collectFilter(predicate, Map.empty)

    /** Returns the set of all kinds that appear somewhere in `this` syntax.
      *
      * @group property
      */
    @inline def kinds: Set[Kind] = collectKinds(ListSet())

    /** @group printing */
    def tokensOf(value: V): Iterator[Seq[Token]] =
      collectTokens(value, Map.empty).toIterator

    // All the functions below have an argument `recs` which
    // contains the set of all `Recursive` syntax on which the call
    // was already performed.
    //
    // This is done to handle the potentially cyclic structure of syntaxes
    // introduced by `Recursive`.


    /** Collects the nullable value from this syntax.
      *
      * @param recs The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectNullable(recs: Set[RecId]): Option[A]

    /** Collects the "first" set from this syntax.
      *
      * @param recs The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectFirst(recs: Set[RecId]): Set[Kind]

    /** Collects the "should-not-follow" set from this syntax.
      *
      * @param recs The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Syntax[Nothing, Any]]

    /** Checks if the recusive syntax `rec` can be invoked without consuming any input tokens.
      *
      * @param rec  The recursive syntax.
      * @param recs The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectCalledLeft(rec: Recursive[Nothing, Any], recs: Set[RecId]): Boolean

    /** Checks if this syntax is productive.
      *
      * @param recs The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectIsProductive(recs: Set[RecId]): Boolean

    /** Checks if this syntax is LL(1).
      *
      * @param recs The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectIsLL1(recs: Set[RecId]): Boolean

    /** Collects the LL(1) conflicts from this syntax.
      *
      * @param recs The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectLL1Conflicts(recs: Set[RecId]): Set[LL1Conflict]

    /** Builds a producer of traces from `this` syntax.
      *
      * @param recs The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectTrails(recs: Map[RecId, () => Producer[Trail]]): Producer[Trail]

    /** Builds a syntax that filter out unwanted kinds.
      *
      * @param predicate Predicate that kinds must satisfy.
      * @param recs      The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectFilter(predicate: Kind => Boolean, recs: Map[RecId, Syntax[Nothing, Any]]): Syntax[V, A]

    /** Collects all kinds appearing in this syntax.
      *
      * @param recs The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectKinds(recs: Set[RecId]): Set[Kind]


    protected def collectTokens(value: V, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]]

    /** Feeds a token and its kind to the syntax and obtain a syntax for the rest of input.
      *
      * @group derivation
      */
    protected def derive(token: Token, kind: Kind): Syntax[V, A]

    /** Feeds a token to the syntax and obtain a syntax for the rest of input.
      *
      * @group derivation
      */
    @inline def derive(token: Token): Syntax[V, A] = derive(token, getKind(token))


    /** String representation of the syntax.
      *
      * @group other
      */
    override def toString = repr(0, Map.empty)

    /** Computes a friendlier string representation for the syntax. */
    protected def repr(level: Int, recs: Map[RecId, String]): String


    // Combinators.

    /** Applies a function to the parsed values.
      *
      * @group combinator
      */
    def map[B](function: A => B): Syntax[V, B] =
      this match {
        case Failure => Failure
        case Success(value, predicate) => Success(function(value), predicate)
        case Transform(otherFunction, otherInverse, inner) =>
          Transform(
            otherFunction andThen function,
            otherInverse,
            inner)
        case inner => Transform(function, (v: V) => Seq(v), inner)
      }

    /** Specifies the inverse of the applied function.
      *
      * @group combinator
      */
    def contramap[W](inverse: W => Seq[V]): Syntax[W, A] =
      this match {
        case Failure => Failure
        case Success(value, predicate) => Success(value, (y: W) => inverse(y).map(predicate).sum)
        case Transform(otherFunction, otherInverse, inner) =>
          Transform(
            otherFunction,
            (z: W) => Try(inverse(z)).getOrElse(Seq()).flatMap((y: V) => otherInverse(y)),
            inner)
        case inner => Transform((x: A) => x, inverse, inner)
      }

    /** Applies a function to the parsed values and specifies its inverse.
      *
      * @group combinator
      */
    def bimap[W, B](function: A => B, inverse: W => Seq[V]): Syntax[W, B] =
      this match {
        case Failure => Failure
        case Success(value, predicate) => Success(function(value), (y: W) => inverse(y).map(predicate).sum)
        case Transform(otherFunction, otherInverse, inner) =>
          Transform(
            otherFunction andThen function,
            (z: W) => Try(inverse(z)).getOrElse(Seq()).flatMap((y: V) => otherInverse(y)),
            inner)
        case inner => Transform(function, inverse, inner)
      }

    /** Sequences `this` and `that` syntax. The parsed values are concatenated.
      *
      * @group combinator
      */
    def ++[W, B](that: Syntax[Seq[W], Seq[B]])
        (implicit ev1: Syntax[V, A] <:< Syntax[Seq[W], Seq[B]],
                  ev2: Seq[W] <:< V,
                  ev3: A <:< Seq[B]): Syntax[Seq[W], Seq[B]] =
      (this, that) match {
        case (Failure, _) => Failure
        case (_, Failure) => Failure
        case (Success(a, pa), Success(b, pb)) => Success(a ++ b, (xs: Seq[W]) => {
          val (as, bs) = xs.splitAt(a.size)
          pa(as) * pb(bs)
        })
        // The next transformation is crucial.
        // It allows to merge together values which accumulate on the left.
        case (_, Concat(left, right)) => (this ++ left) ++ right
        case _ => Concat(this, that)
      }

    /** Sequences `this` and `that` syntax. The parsed value from `that` is returned.
      *
      * @group combinator
      */
    def ~>~[W, B](that: Syntax[W, B])(implicit ev: Unit <:< V): Syntax[W, B] =
      this.~(that).bimap(_._2, {
      case x => Seq(scallion.syntactic.~(ev(()), x))
    })

    /** Sequences `this` and `that` syntax. The parsed value from `this` is returned.
      *
      * @group combinator
      */
    def ~<~(that: Syntax[Unit, Any]): Syntax[V, A] = this.~(that).bimap(_._1, {
      case x => Seq(scallion.syntactic.~(x, ()))
    })

    /** Sequences `this` and `that` syntax. The parsed value from `that` is appended to that from `this`.
      *
      * @group combinator
      */
    def :+[W, B](that: Syntax[W, B])
        (implicit ev1: Syntax[V, A] <:< Syntax[Seq[W], Seq[B]],
                  ev2: Seq[W] <:< V,
                  ev3: A <:< Seq[B]): Syntax[Seq[W], Seq[B]] =
      this ++ that.bimap(Vector(_), {
        case Seq(x) => Seq(x)
        case _ => Seq()
      })

    /** Sequences `this` and `that` syntax. The parsed value from `that` is prepended to that from `this`.
      *
      * @group combinator
      */
    def +:[W, B](that: Syntax[W, B])
        (implicit ev1: Syntax[V, A] <:< Syntax[Seq[W], Seq[B]],
                  ev2: Seq[W] <:< V,
                  ev3: A <:< Seq[B]): Syntax[Seq[W], Seq[B]] =
      that.bimap(Vector(_), {
        (xs: Seq[W]) => if (xs.size == 1) xs else Seq()
      }) ++ this

    /** Sequences `this` and `that` syntax. The parsed values are returned as a pair.
      *
      * @group combinator
      */
    def ~[W, X <: W, B](that: Syntax[W, B]): Syntax[V ~ X, A ~ B] = (this, that) match {
      case (Failure, _) => Failure
      case (_, Failure) => Failure
      case (Success(a, pa), Success(b, pb)) => Success(scallion.syntactic.~(a, b), {
        case va ~ vb => pa(va) * pb(vb)
      })
      case _ => Sequence(this, that)
    }

    /** Disjunction of `this` and `that` syntax.
      *
      * @group combinator
      */
    def |[W <: V, B >: A](that: Syntax[W, B]): Syntax[W, B] = (this, that) match {
      case (Failure, _) => that
      case (_, Failure) => this
      case _ => Disjunction(this, that)
    }

    /** Disjunction of `this` and `that` syntax.
      * The value is tagged to indicate the side which produced it.
      *
      * @group combinator
      */
    def ||[W, B](that: Syntax[W, B]): Syntax[Either[V, W], Either[A, B]] =
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

    /** Makes the syntax nullable.
      *
      * @group combinator
      */
    def opt: Syntax[Option[V], Option[A]] = this.bimap(Some(_), {
      (o: Option[V]) => o match {
        case Some(x) => Seq(x)
        case None => Seq()
      }
    }) | epsilon(None)

    /** Informs the syntax of the values that can be produced.
      *
      * @group combinator
      */
    def unit(defaults: V*): Syntax[Unit, A] = this.contramap {
      (_: Unit) => defaults
    }

    /** Informs the syntax of the values that can be produced.
      *
      * @group combinator
      */
    def always(defaults: V*): Syntax[Unit, Unit] = this.bimap(_ => (), {
      case () => defaults
    })

    /** Prevents token generation.
      *
      * @group combinator
      */
    def void[W]: Syntax[W, A] = this.contramap {
      (_: W) => Seq()
    }

    // Parsing.

    /** Consumes a sequence of tokens and parses into a value.
      * When `this` syntax is not LL(1), the result is unspecified.
      *
      * @group parsing
      */
    def apply(it: Iterator[Token]): ParseResult[V, A] = {

      var syntax: Syntax[V, A] = this
      while (it.hasNext) {
        val token = it.next()
        val newSyntax = syntax.derive(token)
        if (!newSyntax.isProductive) {
          return UnexpectedToken(token, syntax)
        }
        syntax = newSyntax
      }
      syntax.nullable match {
        case None => UnexpectedEnd(syntax)
        case Some(value) => Parsed(value, syntax)
      }
    }


    // Completions.

    /** Returns all possible completions of `this` syntax,
      * ordered by increasing number of tokens.
      *
      * When `this` syntax is not LL(1), the result is unspecified.
      *
      * @param toTokens Computes the possible tokens for a given kind.
      *
      * @group complete
      */
    def completions(toTokens: Kind => Seq[Token]): Iterator[Syntax[V, A]] = {

      val kindTokens: Map[Kind, Seq[Token]] =
        kinds.toSeq.map(kind => kind -> toTokens(kind)).toMap

      val unwantedKinds: Set[Kind] =
        kindTokens.filter(_._2.isEmpty).keySet

      val cleanedSyntax =
        if (unwantedKinds.isEmpty) {
          this
        }
        else {
          this.filter(k => !unwantedKinds.contains(k))
        }

      cleanedSyntax.trails.flatMap { kinds =>
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
          apply(tokens.toIterator).syntax
        }
      }
    }

    /** Returns the smallest completion of `this` syntax that can
      * be obtained using the partial `toToken` function, if any.
      *
      * When `this` syntax is not LL(1), the result is unspecified.
      *
      * @param toToken Computes the preferred token of the given class, if any.
      *
      * @group complete
      */
    def complete(toToken: PartialFunction[Kind, Token]): Syntax[V, A] = {
      val it = completions(kind => toToken.lift(kind).toSeq)
      if (it.hasNext) {
        it.next()
      }
      else {
        Failure
      }
    }
  }

  /** Result of running a `Syntax`.
    *
    * @group result
    */
  sealed trait ParseResult[-V, +A] {

    /** Syntax for the rest of input. */
    val syntax: Syntax[V, A]

    /** Returns the parsed value, if any. */
    def getValue: Option[A] = this match {
      case Parsed(value, _) => Some(value)
      case _ => None
    }
  }

  /** Indicates that the input has been fully processed, resulting in a `value`.
    *
    * A `syntax` for subsequent input is also provided.
    *
    * @group result
    */
  case class Parsed[-V, +A](value: A, syntax: Syntax[V, A]) extends ParseResult[V, A]

  /** Indicates that the provided `token` was not expected at that point.
    *
    * The `syntax` that rejected the token is returned.
    *
    * @group result
    */
  case class UnexpectedToken[-V, +A](token: Token, syntax: Syntax[V, A]) extends ParseResult[V, A]

  /** Indicates that end of input was unexpectedly encountered.
    *
    * The `syntax` for subsequent input is provided.
    *
    * @group result
    */
  case class UnexpectedEnd[-V, +A](syntax: Syntax[V, A]) extends ParseResult[V, A]

  /** Describes a LL(1) conflict.
    *
    * @group conflict
    */
  sealed trait LL1Conflict {

    /** Source of the conflict. */
    val source: Syntax[Nothing, Any]

    /** Syntax for a prefix before the conflict occurs. */
    val prefix: Syntax[Nothing, Any]

    private[syntactic] def addPrefix(syntax: Syntax[Nothing, Any]): LL1Conflict

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
        prefix: Syntax[Nothing, Any],
        source: Disjunction[Nothing, Any]) extends LL1Conflict {

      override private[syntactic] def addPrefix(start: Syntax[Nothing, Any]): NullableConflict =
        this.copy(prefix = start ~ prefix)

      override def witnesses: Iterator[Trail] = prefix.trails
    }

    /** Indicates that two branches of a disjunction share the same first token(s). */
    case class FirstConflict(
        prefix: Syntax[Nothing, Any],
        ambiguities: Set[Kind],
        source: Disjunction[Nothing, Any]) extends LL1Conflict {

      override private[syntactic] def addPrefix(start: Syntax[Nothing, Any]): FirstConflict =
        this.copy(prefix = start ~ prefix)

      override def witnesses: Iterator[Trail] = for {
        trail <- prefix.trails
        kind <- ambiguities
      } yield trail :+ kind
    }

    /** Indicates that the right end side first token set conflicts with the left end side. */
    case class FollowConflict(
        prefix: Syntax[Nothing, Any],
        ambiguities: Set[Kind],
        source: Syntax[Nothing, Any] with SequenceLike[Nothing, Nothing, Any, Any]) extends LL1Conflict {

      override private[syntactic] def addPrefix(start: Syntax[Nothing, Any]): FollowConflict =
        this.copy(prefix = start ~ prefix)

      override def witnesses: Iterator[Trail] = for {
        trail <- prefix.trails
        kind <- ambiguities
      } yield trail :+ kind
    }

    /** Indicates that the syntax recursively calls itself in a left position. */
    case class LeftRecursiveConflict(
        prefix: Syntax[Nothing, Any],
        source: Recursive[Nothing, Any]) extends LL1Conflict {

      override private[syntactic] def addPrefix(start: Syntax[Nothing, Any]): LeftRecursiveConflict =
        this.copy(prefix = start ~ prefix)

      override def witnesses: Iterator[Trail] = prefix.trails
    }
  }

  import LL1Conflict._

  /** Contains primitive basic syntaxes and syntax combinators.
    *
    * @group syntax
    */
  object Syntax {

    /** Syntax that produces `value` without consuming input tokens.
      *
      * @param value     The value produced by the syntax.
      * @param predicate The predicate that checks for equality with `value`.
      *
      * @group basic
      */
    case class Success[-V, +A](value: A, predicate: V => Int) extends Syntax[V, A] {

      override val nullable: Option[A] =
        Some(value)

      override val isProductive: Boolean =
        true

      override protected def collectNullable(recs: Set[RecId]): Option[A] =
        Some(value)

      override protected def collectFirst(recs: Set[RecId]): Set[Kind] =
        ListSet()

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Syntax[Nothing, Any]] =
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
          recs: Map[RecId, Syntax[Nothing, Any]]): Syntax[V, A] =
        this

      override protected def collectKinds(recs: Set[RecId]): Set[Kind] =
        ListSet()

      override protected def collectTokens(
          other: V, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]] =
        Producer.fromIterator(Iterator.fill(predicate(other))(Vector()))

      override protected def derive(token: Token, kind: Kind): Syntax[Any, A] =
        Failure

      override protected def repr(level: Int, recs: Map[RecId, String]): String =
        "epsilon(" + value.toString + ")"
    }

    /** Syntax that produces `value` without consuming input tokens.
      *
      * @group basic
      */
    case object Failure extends Syntax[Any, Nothing] {

      override val nullable: Option[Nothing] =
        None

      override val isProductive: Boolean =
        false

      override protected def collectNullable(recs: Set[RecId]): Option[Nothing] =
        None

      override protected def collectFirst(recs: Set[RecId]): Set[Kind] =
        ListSet()

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Syntax[Nothing, Any]] =
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
          recs: Map[RecId, Syntax[Nothing, Any]]): Syntax[Any, Nothing] =
        this

      override protected def collectKinds(recs: Set[RecId]): Set[Kind] =
        ListSet()

      override protected def collectTokens(
            value: Any, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]] =
        Producer.empty

      override protected def derive(token: Token, kind: Kind): Syntax[Any, Nothing] =
        this

      override protected def repr(level: Int, recs: Map[RecId, String]): String =
        "failure"
    }

    /** Syntax that consumes tokens of the given `kind`.
      *
      * @param kind The kind accepted by the syntax.
      *
      * @group basic
      */
    case class Elem(kind: Kind) extends Syntax[Token, Token] {

      override val nullable: Option[Token] =
        None

      override val isProductive: Boolean =
        true

      override protected def collectNullable(recs: Set[RecId]): Option[Token] =
        None

      override protected def collectFirst(recs: Set[RecId]): Set[Kind] =
        Set(kind)

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Syntax[Nothing, Any]] =
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
          recs: Map[RecId, Syntax[Nothing, Any]]): Syntax[Token, Token] =
        if (predicate(kind)) this else Failure

      override protected def collectKinds(recs: Set[RecId]): Set[Kind] =
        ListSet(kind)

      override protected def collectTokens(
          value: Token, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]) : Producer[Seq[Token]] =
        if (getKind(value) == kind) Producer.single(Vector(value)) else Producer.empty

      override protected def derive(token: Token, tokenKind: Kind): Syntax[Token, Token] =
        if (tokenKind == kind) epsilon(token) else Failure

      override protected def repr(level: Int, recs: Map[RecId, String]): String =
        "elem(" + kind + ")"
    }

    /** Unary combinator.
      *
      * @group combinator
      */
    sealed trait Unary[-V, +A] { self: Syntax[_, _] =>

      /** The inner syntax.
        *
        * @group subsyntax
        */
      def inner: Syntax[V, A]
    }

    /** Binary combinator.
      *
      * @group combinator
      */
    sealed trait Binary[-V, -W, +A, +B] { self: Syntax[_, _] =>

      /** The left-hand side syntax.
        *
        * @group subsyntax
        */
      def left: Syntax[V, A]

      /** The right-hand side syntax.
        *
        * @group subsyntax
        */
      def right: Syntax[W, B]
    }

    /** Syntax that applies a `function` on the parsed value of the `inner` syntax.
      *
      * @param function The function to apply on produced values.
      * @param inner    The inner syntax.
      *
      * @group combinator
      */
    case class Transform[V, W, A, B](
        function: A => B,
        inverse: W => Seq[V],
        inner: Syntax[V, A]) extends Syntax[W, B] with Unary[V, A] {

      override lazy val nullable: Option[B] =
        inner.nullable.map(function)

      override lazy val isProductive: Boolean =
        inner.isProductive

      override protected def collectNullable(recs: Set[RecId]): Option[B] =
        inner.collectNullable(recs).map(function)

      override protected def collectFirst(recs: Set[RecId]): Set[Kind] =
        inner.collectFirst(recs)

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Syntax[Nothing, Any]] =
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
          recs: Map[RecId, Syntax[Nothing, Any]]): Syntax[W, B] =
        inner.collectFilter(predicate, recs).bimap(function, inverse)

      override protected def collectKinds(recs: Set[RecId]): Set[Kind] =
        inner.collectKinds(recs)

      override protected def derive(token: Token, kind: Kind): Syntax[W, B] =
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

    /** Syntax that sequences the `left` and `right` syntaxes.
      *
      * @group combinator
      */
    sealed trait SequenceLike[-V, -W, +A, +B] extends Binary[V, W, A, B] { self: Syntax[_, _] =>

      override protected def collectFirst(recs: Set[RecId]): Set[Kind] =
        left.nullable match {
          case Some(_) => left.collectFirst(recs) ++ right.collectFirst(recs)
          case None => left.collectFirst(recs)
        }

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Syntax[Nothing, Any]] = {
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

    /** Syntax that sequences the `left` and `right` syntaxes and groups the results.
      *
      * @param left  The syntax for the prefix.
      * @param right The syntax for the suffix.
      *
      * @group combinator
      */
    case class Sequence[-V, -W, +A, +B](left: Syntax[V, A], right: Syntax[W, B])
        extends Syntax[V ~ W, A ~ B] with SequenceLike[V, W, A, B] {

      override lazy val isProductive: Boolean =
        left.isProductive && right.isProductive

      override lazy val nullable: Option[A ~ B] = for {
        leftValue <- left.nullable
        rightValue <- right.nullable
      } yield scallion.syntactic.~(leftValue, rightValue)

      override protected def collectNullable(recs: Set[RecId]): Option[A ~ B] = for {
        leftValue <- left.collectNullable(recs)
        rightValue <- right.collectNullable(recs)
      } yield scallion.syntactic.~(leftValue, rightValue)

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Syntax[Nothing, Any]]): Syntax[V ~ W, A ~ B] =
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

      override protected def derive(token: Token, kind: Kind): Syntax[V ~ W, A ~ B] = {
        val derived = left.derive(token, kind)

        if (!derived.isProductive) {
          left.nullable match {
            case Some(leftValue) => epsilon(leftValue) ~ right.derive(token, kind)
            case None => Failure
          }
        }
        else {
          derived ~ right
        }
      }
    }

    /** Syntax that sequences the `left` and `right` syntaxes and concatenates the results.
      *
      * @param left  The syntax for the prefix.
      * @param right The syntax for the suffix.
      *
      * @group combinator
      */
    case class Concat[-V, +A](left: Syntax[Seq[V], Seq[A]], right: Syntax[Seq[V], Seq[A]])
        extends Syntax[Seq[V], Seq[A]] with SequenceLike[Seq[V], Seq[V], Seq[A], Seq[A]] {

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
          recs: Map[RecId, Syntax[Nothing, Any]]): Syntax[Seq[V], Seq[A]] =
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

      override protected def derive(token: Token, kind: Kind): Syntax[Seq[V], Seq[A]] = {
        val derived = left.derive(token, kind)

        if (!derived.isProductive) {
          left.nullable match {
            case Some(leftValue) => epsilon(leftValue) ++ right.derive(token, kind)
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

    /** Syntax that acts either as the disjunction of the `left` and `right` syntaxes.
      *
      * @param left  The syntax for the first alternative.
      * @param right The syntax for the second alternative.
      *
      * @group combinator
      */
    case class Disjunction[-V, +A](left: Syntax[V, A], right: Syntax[V, A])
        extends Syntax[V, A] with Binary[V, V, A, A] {

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

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Syntax[Nothing, Any]] = {
        val fromLeft: Map[Kind, Syntax[Nothing, Any]] =
          if (right.nullable.nonEmpty) {
            left.first.toSeq.map {
              kind => kind -> epsilon(())
            }.toMap
          }
          else {
            Map.empty
          }
        val fromRight: Map[Kind, Syntax[Nothing, Any]] =
          if (left.nullable.nonEmpty) {
            right.first.toSeq.map {
              kind => kind -> epsilon(())
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
            ListSet(FirstConflict(epsilon(()), problematicKinds, this))
          }

        val nullableConflicts: Set[LL1Conflict] =
          if (left.nullable.isEmpty || right.nullable.isEmpty) {
            ListSet()
          }
          else {
            ListSet(NullableConflict(epsilon(()), this))
          }

        val baseConflicts: Set[LL1Conflict] =
          left.collectLL1Conflicts(recs) union right.collectLL1Conflicts(recs)

        baseConflicts union firstConflicts union nullableConflicts
      }

      override protected def collectIsProductive(recs: Set[RecId]): Boolean =
        left.collectIsProductive(recs) || right.collectIsProductive(recs)

      override protected def collectTrails(recs: Map[RecId, () => Producer[Trail]]): Producer[Trail] =
        trailOps.union(left.collectTrails(recs), right.collectTrails(recs))

      override protected def derive(token: Token, kind: Kind): Syntax[V, A] = {
        if (firstFirst.contains(kind)) {
          order._1.derive(token, kind)
        }
        else {
          order._2.derive(token, kind)
        }
      }

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Syntax[Nothing, Any]]): Syntax[V, A] =
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

      /** Extract the inner syntax of a `Recursive` syntax. */
      def unapply[V, W <: V, A](that: Syntax[V, A]): Option[Syntax[W, A]] = {
        if (that.isInstanceOf[Recursive[_, _]]) {
          Some(that.asInstanceOf[Recursive[V, A]].inner)
        }
        else {
          None
        }
      }

      /** Creates a new `Recursive` syntax.
        *
        * @param syntax The inner syntax.
        */
      def create[V, A](syntax: => Syntax[V, A]): Recursive[V, A] = new Recursive[V, A] {
        override protected val id = nextId()
        override lazy val inner: Syntax[V, A] = syntax
      }
    }

    /** Syntax that may recursively call itself.
      *
      * @group combinator
      */
    sealed abstract class Recursive[-V, +A] extends Syntax[V, A] with Unary[V, A] {

      /** Unique identifier for this recursive syntax. */
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

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Syntax[Nothing, Any]] =
        if (recs.contains(this.id)) Map.empty else inner.collectShouldNotFollow(recs + this.id)

      override protected def collectCalledLeft(rec: Recursive[Nothing, Any], recs: Set[RecId]): Boolean =
        if (recs.contains(this.id)) false else (this.id == rec.id) || inner.collectCalledLeft(rec, recs + this.id)

      override protected def collectIsLL1(recs: Set[RecId]): Boolean =
        if (recs.contains(this.id)) true else !inner.calledLeft(this) && inner.collectIsLL1(recs + this.id)

      override protected def collectLL1Conflicts(recs: Set[RecId]): Set[LL1Conflict] =
        if (recs.contains(this.id)) ListSet() else {
          val base = inner.collectLL1Conflicts(recs + this.id)

          if (inner.calledLeft(this)) {
            base + LeftRecursiveConflict(epsilon(()), this)
          }
          else {
            base
          }
        }

      override protected def collectIsProductive(recs: Set[RecId]): Boolean =
        if (recs.contains(this.id)) false else inner.collectIsProductive(recs + this.id)

      override protected def derive(token: Token, kind: Kind): Syntax[V, A] =
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
          recs: Map[RecId, Syntax[Nothing, Any]]): Syntax[V, A] = {
        recs.get(this.id) match {
          case None => {
            lazy val rec: Syntax[V, A] = recursive(inner.collectFilter(predicate, recs + (this.id -> rec)))
            rec
          }
          case Some(rec) => rec.asInstanceOf[Syntax[V, A]]
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
      * the disjunction of syntax in case of conflicting entries.
      */
    private def combineSNF(
        left: Map[Kind, Syntax[Nothing, Any]],
        right: Map[Kind, Syntax[Nothing, Any]]): Map[Kind, Syntax[Nothing, Any]] =
      combine((p1: Syntax[Nothing, Any], p2: Syntax[Nothing, Any]) => p1 | p2)(left, right)
  }


  // API for combinators and basic syntaxes.

  /** Syntax that accepts tokens of the provided `kind`.
    *
    * @group basic
    */
  def elem(kind: Kind): Syntax[Token, Token] = Elem(kind)

  /** Syntax that accepts tokens of the provided `kind`.
    * A function directly is applied on the successfully matched token.
    *
    * @group basic
    */
  def accept[A](kind: Kind)(function: PartialFunction[Token, A]): Syntax[Token, A] =
    elem(kind).map(function)

  /** Indicates that the syntax can refer to itself within its body.
    *
    * @group combinator
    */
  def recursive[V, A](syntax: => Syntax[V, A]): Syntax[V, A] = Recursive.create(syntax)

  /** Syntax that produces the given `value` without consuming any input.
    *
    * @group basic
    */
  def epsilon[A](value: A): Syntax[Any, A] = Success(value, (x: Any) => if (value == x) 1 else 0)

  /** Syntax that always fails.
    *
    * @group basic
    */
  def failure[V, A]: Syntax[V, A] = Failure

  /** Syntax that represents 0 or 1 instances of the `syntax`.
    *
    * @group combinator
    */
  def opt[V, A](syntax: Syntax[V, A]): Syntax[Option[V], Option[A]] = syntax.opt

  /** Syntax that represents 0 or more repetitions of the `rep` syntax.
    *
    * @group combinator
    */
  def many[V, A](rep: Syntax[V, A]): Syntax[Seq[V], Seq[A]] = {
    lazy val rest: Syntax[Seq[V], Seq[A]] = recursive(rep +: rest | epsilon(Vector()))
    rest
  }

  /** Syntax that represents 1 or more repetitions of the `rep` syntax.
    *
    * @group combinator
    */
  def many1[V, A](rep: Syntax[V, A]): Syntax[Seq[V], Seq[A]] = rep +: many(rep)

  /** Syntax that represents 0 or more repetitions of the `rep` syntax, separated by `sep`.
    *
    * @group combinator
    */
  def repsep[V, A](rep: Syntax[V, A], sep: Syntax[Unit, Any]): Syntax[Seq[V], Seq[A]] =
    rep1sep(rep, sep) | epsilon(Vector())

  /** Syntax that represents 1 or more repetitions of the `rep` syntax, separated by `sep`.
    *
    * @group combinator
    */
  def rep1sep[V, A](rep: Syntax[V, A], sep: Syntax[Unit, Any]): Syntax[Seq[V], Seq[A]] = {
    lazy val rest: Syntax[Seq[V], Seq[A]] = recursive((sep ~>~ rep) +: rest | epsilon(Vector()))
    rep +: rest
  }

  /** Syntax that represents the disjunction of several `syntaxes`.
    *
    * @group combinator
    */
  def oneOf[V, A](syntaxes: Syntax[V, A]*): Syntax[V, A] = {
    var queue = syntaxes.toVector :+ failure[V, A]

    while (queue.size > 1) {
      val a = queue(0)
      val b = queue(1)
      queue = queue.drop(2)
      queue :+= a | b
    }

    queue.head
  }
}
