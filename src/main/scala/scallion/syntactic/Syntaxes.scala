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

import scallion.util.internal.{Producer, ProducerOps, PTPS}

/** Contains definitions relating to syntaxes.
  *
  * @see See trait [[scallion.syntactic.Operators]] for useful combinators
  *      to describe infix, prefix and postfix operators.
  *
  * @group syntax
  */
trait Syntaxes[Token, Kind]
    extends visualization.Graphs[Token, Kind]
       with visualization.Grammars[Token, Kind] {

  import Syntax._

  /** Returns the kind associated with `token`.
    *
    * @group abstract
    */
  def getKind(token: Token): Kind


  private val kindSeqOps = new ProducerOps[Seq[Kind]](PTPS.seqPTPS[Kind])

  private val tokenSeqOps = new ProducerOps[Seq[Token]](PTPS.seqPTPS[Token])

  /** Represents a syntax.
    *
    * Acts as both a parser and a pretty printer.
    *
    * @tparam A the type of values that can be produced or printed.
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
  sealed trait Syntax[A] {

    /** The value, if any, corresponding to the empty sequence of tokens in `this` syntax.
      *
      * @group property
      */
    def nullable: Option[A]

    /** Indicates if there exists a finite sequence of tokens that `this` syntax describes.
      *
      * @group property
      */
    def isProductive: Boolean

    /** Returns the set of token kinds that are accepted as the first token by `this` syntax.
      *
      * @group property
      */
    @inline def first: Set[Kind] = collectFirst(ListSet())

    /** Returns all of kinds that should not be accepted
      * as the first token by a subsequent syntax.
      *
      * The value associated to the kind describes the syntax up to the point
      * where the constraint was generated.
      *
      * @group property
      */
    @inline def shouldNotFollow: Map[Kind, Syntax[_]] = collectShouldNotFollow(ListSet())

    /** Checks if a `Recursive` syntax can be entered without
      * being prefixed by a non-empty sequence of tokens in `this` syntax.
      *
      * @param rec The `Recursive` syntax.
      *
      * @group property
      */
    @inline def calledLeft(rec: Recursive[_]): Boolean = collectCalledLeft(rec, ListSet())

    /** Checks if `this` syntax is LL(1).
      *
      * @group property
      */
    @inline def isLL1: Boolean = collectIsLL1(ListSet())

    /** Returns all LL(1) conflicts in `this` syntax.
      *
      * @group property
      */
    @inline def conflicts: Set[LL1Conflict] = collectLL1Conflicts(ListSet())

    /** Returns all possible sequences of token kinds accepted by `this` syntax,
      * ordered by increasing size.
      *
      * @group property
      */
    @inline def trails: Iterator[Seq[Kind]] = collectTrails(Map.empty).toIterator

    /** Strips `this` syntax of all token kinds that do not satisfy a `predicate`.
      *
      * @param predicate The predicate that kinds must satisfy.
      *
      * @group combinator
      */
    @inline def filter(predicate: Kind => Boolean): Syntax[A] = collectFilter(predicate, Map.empty)

    /** Returns the set of all kinds that appear somewhere in `this` syntax.
      *
      * @group property
      */
    @inline def kinds: Set[Kind] = collectKinds(ListSet())

    /** Returns all representations of `value` in `this` syntax,
      * ordered by increasing size.
      *
      * @group printing
      */
    def unapply(value: A): Iterator[Seq[Token]] =
      collectTokens(value, Map.empty).toIterator

    // All the functions below have an argument `recs` which
    // contains the set of all `Recursive` syntax on which the call
    // was already performed.
    //
    // This is done to handle the potentially cyclic structure of syntaxes
    // introduced by `Recursive`.


    /** Collects the nullable value from `this` syntax.
      *
      * @param recs The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectNullable(recs: Set[RecId]): Option[A]

    /** Collects the "first" set from `this` syntax.
      *
      * @param recs The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectFirst(recs: Set[RecId]): Set[Kind]

    /** Collects the "should-not-follow" set from `this` syntax.
      *
      * @param recs The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Syntax[_]]

    /** Checks if the recusive syntax `rec` can be invoked without consuming any input tokens.
      *
      * @param rec  The recursive syntax.
      * @param recs The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectCalledLeft(rec: Recursive[_], recs: Set[RecId]): Boolean

    /** Checks if `this` syntax is productive.
      *
      * @param recs The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectIsProductive(recs: Set[RecId]): Boolean

    /** Checks if `this` syntax is LL(1).
      *
      * @param recs The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectIsLL1(recs: Set[RecId]): Boolean

    /** Collects the LL(1) conflicts from `this` syntax.
      *
      * @param recs The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectLL1Conflicts(recs: Set[RecId]): Set[LL1Conflict]

    /** Builds a producer of trails from `this` syntax.
      *
      * @param recs The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectTrails(recs: Map[RecId, () => Producer[Seq[Kind]]]): Producer[Seq[Kind]]

    /** Builds a syntax that filter out unwanted kinds from `this` syntax.
      *
      * @param predicate Predicate that kinds must satisfy.
      * @param recs      The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectFilter(predicate: Kind => Boolean, recs: Map[RecId, Syntax[_]]): Syntax[A]

    /** Collects all token kinds appearing in `this` syntax.
      *
      * @param recs The identifiers of already visited `Recursive` syntaxes.
      */
    protected def collectKinds(recs: Set[RecId]): Set[Kind]


    /** Collects a producer that iterates over all representations of `value`.
      *
      * @param value The value being printed.
      * @param recs  The producer view associated to an already visited recursive syntax and value.
      */
    protected def collectTokens(value: A, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]]

    /** Feeds a token and its kind to `this` syntax and obtains a syntax for the rest of input.
      *
      * @group derivation
      */
    protected def derive(token: Token, kind: Kind): Syntax[A]

    /** Feeds a token to this `syntax` and obtains a syntax for the rest of input.
      *
      * @group derivation
      */
    @inline def derive(token: Token): Syntax[A] = derive(token, getKind(token))


    /** String representation of `this` syntax.
      *
      * @group other
      */
    override def toString = repr(0, Map.empty)

    /** Computes a friendly string representation for `this` syntax. */
    protected def repr(level: Int, recs: Map[RecId, String]): String


    // Combinators.

    /** Applies a `function` to the parsed values and the `inverse` function to the printed values.
      *
      * @param function The function to be applied on parsed values.
      * @param inverse  The function to be applied on printed values.
      *
      * @group combinator
      */
    def map[B](function: A => B, inverse: B => Seq[A] = (b: B) => Seq()): Syntax[B] =
      this match {
        case Failure() => Failure()
        case Success(value, predicate) => Success(function(value), (y: B) => inverse(y).map(predicate).sum)
        case Transform(otherFunction, otherInverse, inner) =>
          Transform(
            otherFunction andThen function,
            (z: B) => Try(inverse(z)).getOrElse(Seq()).flatMap((y: A) => otherInverse(y)),
            inner)
        case inner => Transform(function, inverse, inner)
      }

    /** Sequences `this` and `that` syntax. The parsed values are concatenated.
      *
      * @group combinator
      */
    def ++[B](that: Syntax[Seq[B]])
        (implicit ev: Syntax[A] =:= Syntax[Seq[B]]): Syntax[Seq[B]] =
      (ev(this), that) match {
        case (Failure(), _) => Failure()
        case (_, Failure()) => Failure()
        case (Success(a, pa), Success(b, pb)) => Success(a ++ b, (xs: Seq[B]) => {
          val (as, bs) = xs.splitAt(a.size)
          pa(as) * pb(bs)
        })
        // The next transformation is crucial.
        // It allows to merge together values which accumulate on the left.
        case (_, Concat(left, right)) => (this ++ left) ++ right
        case _ => Concat(this, that)
      }

    /** Sequences `this` and `that` syntax. The parsed values from `that` is returned.
      *
      * @group combinator
      */
    def ~>~[W, B](that: Syntax[B])(implicit ev: Syntax[A] =:= Syntax[Unit]): Syntax[B] =
      ev(this).~(that).map(_._2, {
      case x => Seq(scallion.syntactic.~((), x))
    })

    /** Sequences `this` and `that` syntax. The parsed value from `this` is returned.
      *
      * @group combinator
      */
    def ~<~(that: Syntax[Unit]): Syntax[A] = this.~(that).map(_._1, {
      case x => Seq(scallion.syntactic.~(x, ()))
    })

    /** Sequences `this` and `that` syntax. The parsed value from `that` is appended to that from `this`.
      *
      * @group combinator
      */
    def :+[W, B](that: Syntax[B])
        (implicit ev: Syntax[A] =:= Syntax[Seq[B]]): Syntax[Seq[B]] =
      ev(this) ++ that.map(Vector[B](_), {
        case Seq(x) => Seq(x)
        case _ => Seq()
      })

    /** Sequences `this` and `that` syntax. The parsed value from `that` is prepended to that from `this`.
      *
      * @group combinator
      */
    def +:[B](that: Syntax[B])
        (implicit ev: Syntax[A] =:= Syntax[Seq[B]]): Syntax[Seq[B]] =
      that.map(Vector(_) : Seq[B], {
        (xs: Seq[B]) => if (xs.size == 1) xs else Seq()
      }) ++ ev(this)

    /** Sequences `this` and `that` syntax. The parsed values are returned as a pair.
      *
      * @group combinator
      */
    def ~[B](that: Syntax[B]): Syntax[A ~ B] = (this, that) match {
      case (Failure(), _) => Failure()
      case (_, Failure()) => Failure()
      case (Success(a, pa), Success(b, pb)) => Success(scallion.syntactic.~(a, b), {
        case va ~ vb => pa(va) * pb(vb)
      })
      case _ => Sequence(this, that)
    }

    /** Disjunction of `this` and `that` syntax.
      *
      * @group combinator
      */
    def |(that: Syntax[A]): Syntax[A] = (this, that) match {
      case (Failure(), _) => that
      case (_, Failure()) => this
      case _ => Disjunction(this, that)
    }

    /** Disjunction of `this` and `that` syntax.
      * The value is tagged to indicate the side it comes from.
      *
      * @group combinator
      */
    def ||[W, B](that: Syntax[B]): Syntax[Either[A, B]] =
      this.map[Either[A, B]](Left(_), {
        case Left(x) => Seq(x)
        case Right(_) => Seq()
      }) | that.map[Either[A, B]](Right(_), {
        case Left(_) => Seq()
        case Right(x) => Seq(x)
      })

    /** Makes `this` syntax nullable.
      *
      * @group combinator
      */
    def opt: Syntax[Option[A]] = this.map[Option[A]](Some(_), {
      case Some(x) => Seq(x)
      case None => Seq()
    }) | epsilon(None)

    /** Indicates that `this` syntax describes only a finite number of
      * equivalent `values`.
      *
      * Parsed values are replaced by `()`, while printed values
      * are replaced by the various `values`.
      *
      * @group combinator
      */
    def unit(values: A*): Syntax[Unit] = this.map(_ => (), {
      case () => values
    })


    /** Upcasts `this` syntax to `Syntax[Any]`.
      *
      * Disables pretty printing for `this` syntax.
      */
    def void: Syntax[Any] = this.map((x: A) => x, (y: Any) => Seq())

    /** Upcasts `this` syntax.
      *
      * The resulting `syntax` parses and pretty prints equivalently to `this` syntax.
      */
    def up[B >: A](implicit ev: Manifest[A]): Syntax[B] = this.map((x: A) => x, (y: B) => ev.unapply(y) match {
      case None => Seq()
      case Some(x) => Seq(x)
    })

    // Parsing.

    /** Consumes a sequence of tokens and parses it into a value.
      * When `this` syntax is not LL(1), the result is unspecified.
      *
      * @see See [[ParseResult]] for the possible return values.
      *
      * @group parsing
      */
    def apply(it: Iterator[Token]): ParseResult[A] = {

      var syntax: Syntax[A] = this
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
    def completions(toTokens: Kind => Seq[Token]): Iterator[Syntax[A]] = {

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
    def complete(toToken: PartialFunction[Kind, Token]): Syntax[A] = {
      val it = completions(kind => toToken.lift(kind).toSeq)
      if (it.hasNext) {
        it.next()
      }
      else {
        Failure()
      }
    }
  }

  /** Result of parsing a `Syntax`.
    *
    * @group result
    */
  sealed trait ParseResult[A] {

    /** Syntax for the rest of input. */
    val syntax: Syntax[A]

    /** Returns the parsed value, if any. */
    def getValue: Option[A] = this match {
      case Parsed(value, _) => Some(value)
      case _ => None
    }
  }

  /** Indicates that the input has been fully parsed, resulting in a `value`.
    *
    * A `syntax` for subsequent input is also provided.
    *
    * @param value  The value produced.
    * @param syntax Syntax for more input.
    *
    * @group result
    */
  case class Parsed[A](value: A, syntax: Syntax[A]) extends ParseResult[A]

  /** Indicates that the provided `token` was not expected at that point.
    *
    * The `syntax` at the point of error is returned.
    *
    * @param token  The token at fault.
    * @param syntax Syntax at the point of error.
    *
    * @group result
    */
  case class UnexpectedToken[A](token: Token, syntax: Syntax[A]) extends ParseResult[A]

  /** Indicates that end of input was unexpectedly encountered.
    *
    * The `syntax` for subsequent input is provided.
    *
    * @param syntax Syntax at the end of input.
    *
    * @group result
    */
  case class UnexpectedEnd[A](syntax: Syntax[A]) extends ParseResult[A]

  /** Describes a LL(1) conflict.
    *
    * @group conflict
    */
  sealed trait LL1Conflict {

    /** Source of the conflict. */
    val source: Syntax[_]

    /** Syntax for a prefix before the conflict occurs. */
    val prefix: Syntax[_]

    private[syntactic] def addPrefix(syntax: Syntax[_]): LL1Conflict

    /** Returns trails that witness the conflict. */
    def witnesses: Iterator[Seq[Kind]]
  }

  /** Contains the description of the various LL(1) conflicts.
    *
    * @group conflict
    */
  object LL1Conflict {

    /** Indicates that both branches of a disjunction are nullable. */
    case class NullableConflict(
        prefix: Syntax[_],
        source: Disjunction[_]) extends LL1Conflict {

      override private[syntactic] def addPrefix(start: Syntax[_]): NullableConflict =
        this.copy(prefix = start ~ prefix)

      override def witnesses: Iterator[Seq[Kind]] = prefix.trails
    }

    /** Indicates that two branches of a disjunction share the same first token(s). */
    case class FirstConflict(
        prefix: Syntax[_],
        ambiguities: Set[Kind],
        source: Disjunction[_]) extends LL1Conflict {

      override private[syntactic] def addPrefix(start: Syntax[_]): FirstConflict =
        this.copy(prefix = start ~ prefix)

      override def witnesses: Iterator[Seq[Kind]] = for {
        trail <- prefix.trails
        kind <- ambiguities
      } yield trail :+ kind
    }

    /** Indicates that the right end side first token set conflicts with the left end side. */
    case class FollowConflict(
        prefix: Syntax[_],
        ambiguities: Set[Kind],
        source: Syntax[_] with SequenceLike[_, _]) extends LL1Conflict {

      override private[syntactic] def addPrefix(start: Syntax[_]): FollowConflict =
        this.copy(prefix = start ~ prefix)

      override def witnesses: Iterator[Seq[Kind]] = for {
        trail <- prefix.trails
        kind <- ambiguities
      } yield trail :+ kind
    }

    /** Indicates that the syntax recursively calls itself in a left position. */
    case class LeftRecursiveConflict(
        prefix: Syntax[_],
        source: Recursive[_]) extends LL1Conflict {

      override private[syntactic] def addPrefix(start: Syntax[_]): LeftRecursiveConflict =
        this.copy(prefix = start ~ prefix)

      override def witnesses: Iterator[Seq[Kind]] = prefix.trails
    }
  }

  import LL1Conflict._

  /** Contains primitive basic syntaxes and syntax combinators.
    *
    * @group syntax
    */
  object Syntax {

    /** Syntax for the empty string.
      *
      * @param value   The value produced.
      * @param matches The function that counts how many times its parameter matches the `value`.
      *
      * @group basic
      */
    case class Success[A](value: A, matches: A => Int) extends Syntax[A] {

      override val nullable: Option[A] =
        Some(value)

      override val isProductive: Boolean =
        true

      override protected def collectNullable(recs: Set[RecId]): Option[A] =
        Some(value)

      override protected def collectFirst(recs: Set[RecId]): Set[Kind] =
        ListSet()

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Syntax[_]] =
        Map.empty

      override protected def collectCalledLeft(rec: Recursive[_], recs: Set[RecId]): Boolean =
        false

      override protected def collectIsLL1(recs: Set[RecId]): Boolean =
        true

      override protected def collectLL1Conflicts(recs: Set[RecId]): Set[LL1Conflict] =
        ListSet()

      override protected def collectIsProductive(recs: Set[RecId]): Boolean =
        true

      override protected def collectTrails(recs: Map[RecId, () => Producer[Seq[Kind]]]): Producer[Seq[Kind]] =
        Producer.single(Vector())

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Syntax[_]]): Syntax[A] =
        this

      override protected def collectKinds(recs: Set[RecId]): Set[Kind] =
        ListSet()

      override protected def collectTokens(
          other: A, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]] =
        Producer.fromIterator(Iterator.fill(matches(other))(Vector()))

      override protected def derive(token: Token, kind: Kind): Syntax[A] =
        Failure()

      override protected def repr(level: Int, recs: Map[RecId, String]): String =
        "epsilon(" + value.toString + ")"
    }

    /** Empty syntax.
      *
      * @group basic
      */
    case class Failure[A]() extends Syntax[A] {

      override val nullable: Option[A] =
        None

      override val isProductive: Boolean =
        false

      override protected def collectNullable(recs: Set[RecId]): Option[A] =
        None

      override protected def collectFirst(recs: Set[RecId]): Set[Kind] =
        ListSet()

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Syntax[_]] =
        Map.empty

      override protected def collectCalledLeft(rec: Recursive[_], recs: Set[RecId]): Boolean =
        false

      override protected def collectIsLL1(recs: Set[RecId]): Boolean =
        true

      override protected def collectLL1Conflicts(recs: Set[RecId]): Set[LL1Conflict] =
        ListSet()

      override protected def collectIsProductive(recs: Set[RecId]): Boolean =
        false

      override protected def collectTrails(recs: Map[RecId, () => Producer[Seq[Kind]]]): Producer[Seq[Kind]] =
        Producer.empty

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Syntax[_]]): Syntax[A] =
        this

      override protected def collectKinds(recs: Set[RecId]): Set[Kind] =
        ListSet()

      override protected def collectTokens(
            value: A, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]] =
        Producer.empty

      override protected def derive(token: Token, kind: Kind): Syntax[A] =
        this

      override protected def repr(level: Int, recs: Map[RecId, String]): String =
        "failure"
    }

    /** Syntax that describes a single token of the given `kind`.
      *
      * @param kind The kind accepted by the syntax.
      *
      * @group basic
      */
    case class Elem(kind: Kind) extends Syntax[Token] {

      override val nullable: Option[Token] =
        None

      override val isProductive: Boolean =
        true

      override protected def collectNullable(recs: Set[RecId]): Option[Token] =
        None

      override protected def collectFirst(recs: Set[RecId]): Set[Kind] =
        Set(kind)

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Syntax[_]] =
        Map.empty

      override protected def collectCalledLeft(rec: Recursive[_], recs: Set[RecId]): Boolean =
        false

      override protected def collectIsLL1(recs: Set[RecId]): Boolean =
        true

      override protected def collectLL1Conflicts(recs: Set[RecId]): Set[LL1Conflict] =
        ListSet()

      override protected def collectIsProductive(recs: Set[RecId]): Boolean =
        true

      override protected def collectTrails(recs: Map[RecId, () => Producer[Seq[Kind]]]): Producer[Seq[Kind]] =
        Producer.single(Vector(kind))

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Syntax[_]]): Syntax[Token] =
        if (predicate(kind)) this else Failure()

      override protected def collectKinds(recs: Set[RecId]): Set[Kind] =
        ListSet(kind)

      override protected def collectTokens(
          value: Token, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]) : Producer[Seq[Token]] =
        if (getKind(value) == kind) Producer.single(Vector(value)) else Producer.empty

      override protected def derive(token: Token, tokenKind: Kind): Syntax[Token] =
        if (tokenKind == kind) epsilon(token) else Failure()

      override protected def repr(level: Int, recs: Map[RecId, String]): String =
        "elem(" + kind + ")"
    }

    /** Unary combinator.
      *
      * @group combinator
      */
    sealed trait Unary[A] { self: Syntax[_] =>

      /** The inner syntax.
        *
        * @group subsyntax
        */
      def inner: Syntax[A]
    }

    /** Binary combinator.
      *
      * @group combinator
      */
    sealed trait Binary[A, B] { self: Syntax[_] =>

      /** The left-hand side syntax.
        *
        * @group subsyntax
        */
      def left: Syntax[A]

      /** The right-hand side syntax.
        *
        * @group subsyntax
        */
      def right: Syntax[B]
    }

    /** Syntax that applies a `function` on the parsed values of the `inner` syntax
      * and an `inverse` function on the printed values given to the `inner` syntax.
      *
      * @param function The function to apply on produced values.
      * @param inverse  The function to apply on printed values.
      * @param inner    The inner syntax.
      *
      * @group combinator
      */
    case class Transform[A, B](
        function: A => B,
        inverse: B => Seq[A],
        inner: Syntax[A]) extends Syntax[B] with Unary[A] {

      override lazy val nullable: Option[B] =
        inner.nullable.map(function)

      override lazy val isProductive: Boolean =
        inner.isProductive

      override protected def collectNullable(recs: Set[RecId]): Option[B] =
        inner.collectNullable(recs).map(function)

      override protected def collectFirst(recs: Set[RecId]): Set[Kind] =
        inner.collectFirst(recs)

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Syntax[_]] =
        inner.collectShouldNotFollow(recs)

      override protected def collectCalledLeft(rec: Recursive[_], recs: Set[RecId]): Boolean =
        inner.collectCalledLeft(rec, recs)

      override protected def collectIsLL1(recs: Set[RecId]): Boolean =
        inner.collectIsLL1(recs)

      override protected def collectLL1Conflicts(recs: Set[RecId]): Set[LL1Conflict] =
        inner.collectLL1Conflicts(recs)

      override protected def collectIsProductive(recs: Set[RecId]): Boolean =
        inner.collectIsProductive(recs)

      override protected def collectTrails(recs: Map[RecId, () => Producer[Seq[Kind]]]): Producer[Seq[Kind]] =
        inner.collectTrails(recs)

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Syntax[_]]): Syntax[B] =
        inner.collectFilter(predicate, recs).map(function, inverse)

      override protected def collectKinds(recs: Set[RecId]): Set[Kind] =
        inner.collectKinds(recs)

      override protected def derive(token: Token, kind: Kind): Syntax[B] =
        inner.derive(token, kind).map(function, inverse)

      override protected def collectTokens(
        value: B, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]] = {

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
    sealed trait SequenceLike[A, B] extends Binary[A, B] { self: Syntax[_] =>

      override protected def collectFirst(recs: Set[RecId]): Set[Kind] =
        left.nullable match {
          case Some(_) => left.collectFirst(recs) ++ right.collectFirst(recs)
          case None => left.collectFirst(recs)
        }

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Syntax[_]] = {
        val rightSNF: Map[Kind, Syntax[_]] =
          right.collectShouldNotFollow(recs).map {
            case (k, v) => k -> left ~ v
          }.toMap

        right.nullable match {
          case Some(_) => combineSNF(
            left.collectShouldNotFollow(recs),
            rightSNF
          )
          case None => rightSNF
        }
      }

      override protected def collectCalledLeft(rec: Recursive[_], recs: Set[RecId]): Boolean =
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

      override protected def collectTrails(recs: Map[RecId, () => Producer[Seq[Kind]]]): Producer[Seq[Kind]] =
        kindSeqOps.product(left.collectTrails(recs), right.collectTrails(recs))

      override protected def collectKinds(recs: Set[RecId]): Set[Kind] =
        left.collectKinds(recs) union right.collectKinds(recs)
    }

    /** Syntax that sequences the `left` and `right` syntaxes and pairs the results.
      *
      * @param left  The syntax for the prefix.
      * @param right The syntax for the suffix.
      *
      * @group combinator
      */
    case class Sequence[A, B](left: Syntax[A], right: Syntax[B])
        extends Syntax[A ~ B] with SequenceLike[A, B] {

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
          recs: Map[RecId, Syntax[_]]): Syntax[A ~ B] =
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
          value: A ~ B, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]] =

        value match {
          case a ~ b => tokenSeqOps.product(left.collectTokens(a, recs), right.collectTokens(b, recs))
        }

      override protected def derive(token: Token, kind: Kind): Syntax[A ~ B] = {
        val derived = left.derive(token, kind)

        if (!derived.isProductive) {
          left.nullable match {
            case Some(leftValue) => epsilon(leftValue) ~ right.derive(token, kind)
            case None => Failure()
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
    case class Concat[A](left: Syntax[Seq[A]], right: Syntax[Seq[A]])
        extends Syntax[Seq[A]] with SequenceLike[Seq[A], Seq[A]] {

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
          recs: Map[RecId, Syntax[_]]): Syntax[Seq[A]] =
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

      override protected def derive(token: Token, kind: Kind): Syntax[Seq[A]] = {
        val derived = left.derive(token, kind)

        if (!derived.isProductive) {
          left.nullable match {
            case Some(leftValue) => epsilon(leftValue) ++ right.derive(token, kind)
            case None => Failure()
          }
        }
        else {
          derived ++ right
        }
      }

      override protected def collectTokens(
          value: Seq[A], recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]] = {

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

    /** Syntax that acts as either the `left` or the `right` syntaxes.
      *
      * @param left  The syntax for the first alternative.
      * @param right The syntax for the second alternative.
      *
      * @group combinator
      */
    case class Disjunction[A](left: Syntax[A], right: Syntax[A])
        extends Syntax[A] with Binary[A, A] {

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

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Syntax[_]] = {
        val fromLeft: Map[Kind, Syntax[_]] =
          if (right.nullable.nonEmpty) {
            left.first.toSeq.map {
              kind => kind -> epsilon(())
            }.toMap
          }
          else {
            Map.empty
          }
        val fromRight: Map[Kind, Syntax[_]] =
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

      override protected def collectCalledLeft(rec: Recursive[_], recs: Set[RecId]): Boolean =
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

      override protected def collectTrails(recs: Map[RecId, () => Producer[Seq[Kind]]]): Producer[Seq[Kind]] =
        kindSeqOps.union(left.collectTrails(recs), right.collectTrails(recs))

      override protected def derive(token: Token, kind: Kind): Syntax[A] = {
        if (firstFirst.contains(kind)) {
          order._1.derive(token, kind)
        }
        else {
          order._2.derive(token, kind)
        }
      }

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Syntax[_]]): Syntax[A] =
        left.collectFilter(predicate, recs) | right.collectFilter(predicate, recs)

      override protected def collectKinds(recs: Set[RecId]): Set[Kind] =
        left.collectKinds(recs) union right.collectKinds(recs)

      override protected def collectTokens(
          value: A, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]] =
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
      def unapply[A](that: Syntax[A]): Option[Syntax[A]] = {
        if (that.isInstanceOf[Recursive[_]]) {
          Some(that.asInstanceOf[Recursive[A]].inner)
        }
        else {
          None
        }
      }

      /** Creates a new `Recursive` syntax.
        *
        * @param syntax The inner syntax.
        */
      def create[A](syntax: => Syntax[A]): Recursive[A] = new Recursive[A] {
        override protected val id = nextId()
        override lazy val inner: Syntax[A] = syntax
      }
    }

    /** Syntax that may recursively mention itself.
      *
      * @group combinator
      */
    sealed abstract class Recursive[A] extends Syntax[A] with Unary[A] {

      /** Unique identifier for this recursive syntax. */
      protected val id: RecId

      /** Checks if `this` is equal to `other`.
        *
        * @group other
        */
      override def equals(other: Any): Boolean =
        if (!other.isInstanceOf[Recursive[_]]) {
          false
        }
        else {
          val that = other.asInstanceOf[Recursive[_]]
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

      override protected def collectShouldNotFollow(recs: Set[RecId]): Map[Kind, Syntax[_]] =
        if (recs.contains(this.id)) Map.empty else inner.collectShouldNotFollow(recs + this.id)

      override protected def collectCalledLeft(rec: Recursive[_], recs: Set[RecId]): Boolean =
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

      override protected def derive(token: Token, kind: Kind): Syntax[A] =
        inner.derive(token, kind)

      override protected def collectTrails(recs: Map[RecId, () => Producer[Seq[Kind]]]): Producer[Seq[Kind]] =
        recs.get(this.id) match {
          case None => {
            lazy val pair: (Producer[Seq[Kind]], () => Producer[Seq[Kind]]) =
              Producer.duplicate(Producer.lazily {
                inner.collectTrails(recs + (this.id -> pair._2))
              })
            pair._1
          }
          case Some(createProducer) => createProducer()
        }

      override protected def collectTokens(
          value: A, recs: Map[(RecId, Any), () => Producer[Seq[Token]]]): Producer[Seq[Token]] =

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
          recs: Map[RecId, Syntax[_]]): Syntax[A] = {
        recs.get(this.id) match {
          case None => {
            lazy val rec: Syntax[A] = recursive(inner.collectFilter(predicate, recs + (this.id -> rec)))
            rec
          }
          case Some(rec) => rec.asInstanceOf[Syntax[A]]
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
    private def combine[K, V1, V2 <: V1, V3 <: V1]
        (merge: (V2, V3) => V1)
        (left: Map[K, V2], right: Map[K, V3]): Map[K, V1] =
      right.foldLeft(left: Map[K, V1]) {
        case (acc, (key, value)) => acc + (key -> (left.get(key) match {
          case None => value
          case Some(other) => merge(other, value)
        }))
      }

    /** Combines two Should-Not-Follow results by taking
      * the disjunction of syntax in case of conflicting entries.
      */
    private def combineSNF(
        left: Map[Kind, Syntax[_]],
        right: Map[Kind, Syntax[_]]): Map[Kind, Syntax[_]] =
      combine((p1: Syntax[_], p2: Syntax[_]) => p1.void | p2.void)(left, right)
  }


  // API for combinators and basic syntaxes.

  /** Syntax that describes a single token of the provided `kind`.
    *
    * @group basic
    */
  def elem(kind: Kind): Syntax[Token] = Elem(kind)

  /** Syntax that describes a single token of the provided `kind`,
    * and that directly applies a function on the successfully parsed token.
    *
    * @group basic
    */
  def accept[A](kind: Kind)(
      function: PartialFunction[Token, A],
      inverse: A => Seq[Token] = (x: A) => Seq()): Syntax[A] =
    elem(kind).map(function, inverse)

  /** Indicates that the syntax can refer to itself within its body.
    *
    * @group combinator
    */
  def recursive[A](syntax: => Syntax[A]): Syntax[A] = Recursive.create(syntax)

  /** Syntax that produces the given `value` to the empty sequence of tokens.
    *
    * @group basic
    */
  def epsilon[A](value: A): Syntax[A] = Success(value, (x: A) => if (value == x) 1 else 0)

  /** Empty syntax.
    *
    * @group basic
    */
  def failure[A]: Syntax[A] = Failure()

  /** Syntax that represents 0 or 1 instances of the `syntax`.
    *
    * @group combinator
    */
  def opt[A](syntax: Syntax[A]): Syntax[Option[A]] = syntax.opt

  /** Syntax that represents 0 or more repetitions of the `rep` syntax.
    *
    * @group combinator
    */
  def many[A](rep: Syntax[A]): Syntax[Seq[A]] = {
    lazy val rest: Syntax[Seq[A]] = recursive(rep +: rest | epsilon(Vector()))
    rest
  }

  /** Syntax that represents 1 or more repetitions of the `rep` syntax.
    *
    * @group combinator
    */
  def many1[A](rep: Syntax[A]): Syntax[Seq[A]] = rep +: many(rep)

  /** Syntax that represents 0 or more repetitions of the `rep` syntax, separated by `sep`.
    *
    * @group combinator
    */
  def repsep[A](rep: Syntax[A], sep: Syntax[Unit]): Syntax[Seq[A]] =
    rep1sep(rep, sep) | epsilon(Vector())

  /** Syntax that represents 1 or more repetitions of the `rep` syntax, separated by `sep`.
    *
    * @group combinator
    */
  def rep1sep[A](rep: Syntax[A], sep: Syntax[Unit]): Syntax[Seq[A]] = {
    lazy val rest: Syntax[Seq[A]] = recursive((sep ~>~ rep) +: rest | epsilon(Vector()))
    rep +: rest
  }

  /** Syntax that represents the disjunction of several `syntaxes`.
    *
    * @group combinator
    */
  def oneOf[A](syntaxes: Syntax[A]*): Syntax[A] = {
    var queue = syntaxes.toVector :+ failure[A]

    while (queue.size > 1) {
      val a = queue(0)
      val b = queue(1)
      queue = queue.drop(2)
      queue :+= a | b
    }

    queue.head
  }
}
