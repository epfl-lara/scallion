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

import java.util.{ IdentityHashMap => IHM }

import scala.collection.immutable.ListSet
import scala.collection.JavaConverters._
import scala.collection.mutable.{Set => MutSet}
import scala.util.Try

import scallion.util.internal._

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

    /** Indicates if the empty sequence is described by `this` syntax.
      *
      * @group property
      */
    @inline def isNullable: Boolean = nullable.nonEmpty

    /** Indicates if there exists a finite sequence of tokens that `this` syntax describes.
      *
      * @group property
      */
    def isProductive: Boolean

    /** Returns the set of token kinds that are accepted as the first token by `this` syntax.
      *
      * @group property
      */
    def first: Set[Kind]

    /** Returns the set of token kinds that are accepted right after an accepted sequence.
      *
      * @group property
      */
    def followLast: Set[Kind]

    /** Returns the set of token kinds that are accepted right after an accepted sequence,
      * marked with the source of the entry.
      *
      * @group property
      */
    protected def followLastEntries: Set[FollowLastEntry]

    /** Checks if `this` syntax is LL(1).
      *
      * @group property
      */
    def isLL1: Boolean

    /** Returns all LL(1) conflicts in `this` syntax.
      *
      * @group property
      */
    def conflicts: Set[LL1Conflict]

    /** Returns the set of all kinds that appear somewhere in `this` syntax.
      *
      * @group property
      */
    def kinds: Set[Kind]

    /** Returns a syntax for the language up to the given point.
      *
      * Returned syntax might not be LL(1), even when `this` is.
      *
      * @param syntax The syntax to find in `this`.
      */
    private[scallion] def prefix(syntax: Syntax[_]): Syntax[Unit] = {
      val recs = new IHM[Recursive[_], Recursive[Unit]]()
      computePrefix(syntax, recs)
    }

    /** Returns all possible sequences of token kinds accepted by `this` syntax,
      * ordered by increasing size.
      *
      * @group property
      */
    @inline def trails: Iterator[Seq[Kind]] =
      collectTrails(Map.empty).toIterator

    /** Strips `this` syntax of all token kinds that do not satisfy a `predicate`.
      *
      * @param predicate The predicate that kinds must satisfy.
      *
      * @group combinator
      */
    @inline private[scallion] def filter(predicate: Kind => Boolean): Syntax[A] =
      collectFilter(predicate, Map.empty)

    /** Returns all representations of `value` in `this` syntax,
      * ordered by increasing size.
      *
      * @group printing
      */
    def unapply(value: A): Iterator[Seq[Token]] =
      collectTokens(value, Map.empty).toIterator

    /** Computes the nullable value of a syntax and all Recursive syntax below it
      * using a propagator network.
      */
    protected def computeNullable(cells: IHM[Recursive[_], Cell[_]], callback: A => Unit): Unit

    /** Computes the productivity of a syntax and all Recursive syntax below it
      * using a propagator network.
      */
    protected def computeIsProductive(cells: IHM[Recursive[_], Cell[Unit]], callback: Unit => Unit): Unit

    /** Computes the first set of a syntax and all Recursive syntax below it
      * using a propagator network.
      */
    protected def computeFirst(cells: IHM[Recursive[_], Cell[Set[Kind]]], callback: Set[Kind] => Unit): Unit

    /** Computes the follow-last of a syntax and all Recursive syntax below it
      * using a propagator network.
      */
    protected def computeFollowLast(cells: IHM[Recursive[_], Cell[Set[Kind]]], callback: Set[Kind] => Unit): Unit

    /** Computes the follow-last set of a syntax and all Recursive syntax below it
      * using a propagator network. Each set is marked with its origin.
      */
    protected def computeFollowLastEntries(cells: IHM[Recursive[_], Cell[Set[FollowLastEntry]]],
                                           callback: Set[FollowLastEntry] => Unit): Unit

    /** Computes the LL(1)-ness of a syntax and all Recursive syntax below it
      * using a propagator network.
      */
    protected def computeIsLL1(cells: IHM[Recursive[_], Cell[Unit]], callback: Unit => Unit): Unit

    /** Computes the LL(1) conflicts of a syntax and all Recursive syntax below it
      * using a propagator network.
      */
    protected def computeConflicts(cells: IHM[Recursive[_], Cell[Set[LL1Conflict]]],
                                   callback: Set[LL1Conflict] => Unit): Unit

    /** Computes the kinds of a syntax and all Recursive syntax below it
      * using a propagator network.
      */
    protected def computeKinds(cells: IHM[Recursive[_], Cell[Set[Kind]]], callback: Set[Kind] => Unit): Unit

    /** Computes the prefix of a syntax within `this`. */
    protected def computePrefix(syntax: Syntax[_], recs: IHM[Recursive[_], Recursive[Unit]]): Syntax[Unit] =
      if (this == syntax) {
        Success((), _ => 1)
      }
      else {
        computePrefixHelper(syntax, recs)
      }

    /** Computes the prefix of a syntax within `this`. Assumes that the check for equality
      * between `this` and `syntax` has already been performed.
      */
    protected def computePrefixHelper(syntax: Syntax[_], recs: IHM[Recursive[_], Recursive[Unit]]): Syntax[Unit]

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
        case Failure() =>
          Failure()
        case Success(value, predicate) =>
          Success(function(value), (y: B) => Try(inverse(y)).getOrElse(Seq()).map(predicate).sum)
        case Transform(otherFunction, otherInverse, inner) =>
          Transform(
            otherFunction andThen function,
            (z: B) => Try(inverse(z)).getOrElse(Seq()).flatMap((y: A) => otherInverse(y)),
            inner)
        case inner =>
          Transform(function, (y: B) => Try(inverse(y)).getOrElse(Seq()), inner)
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

    /** Sequences `this` and `that` syntax. The parsed value from `this` is returned.
      *
      * @group combinator
      */
    def ~(that: Skip): Syntax[A] = this ~<~ that.syntax

    /** Indicates that the value from `this` syntax should be ignored
      * when building up sequences using `~`.
      *
      * @group combinator
      */
    def skip(implicit ev: Syntax[A] =:= Syntax[Unit]): Skip = Skip(ev(this))

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
      *
      * @group combinator
      */
    def void: Syntax[Any] = this.map((x: A) => x, (y: Any) => Seq())

    /** Upcasts `this` syntax.
      *
      * The resulting `syntax` parses and pretty prints equivalently to `this` syntax.
      *
      * @group combinator
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

      var syntax: Syntax[A] = if (!this.isProductive) Failure() else this
      // Note that the initial value is set to Failure() instead of this in case the
      // syntax is not productive to avoid problems arising due to left-recursivity.
      // left-recursivity makes derive loop.
      // If the syntax is LL(1), the only way a syntax can be left-recursive is
      // if it is not productive.

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
    val source: Disjunction[_]

    /** Returns sequences of token kinds that lead to an ambiguity due to this conflict.
      *
      * @param syntax The syntax from which to view the conflict.
      */
    def witnessedFrom(syntax: Syntax[_]): Iterator[Seq[Kind]] =
      syntax.prefix(source).trails
  }

  /** Contains the description of the various LL(1) conflicts.
    *
    * @group conflict
    */
  object LL1Conflict {

    /** Indicates that both branches of a disjunction are nullable.
      *
      * @param source The source of the conflict.
      */
    case class NullableConflict(source: Disjunction[_]) extends LL1Conflict

    /** Indicates that two branches of a disjunction share some same first token kinds.
      *
      * @param source      The source of the conflict.
      * @param ambiguities The conflicting kinds.
      */
    case class FirstConflict(source: Disjunction[_],
                             ambiguities: Set[Kind]) extends LL1Conflict

    /** Indicates that an ambiguity arises due to a disjunction appearing somewhere in
      * the left-hand side of a sequence, that conflicts with the right-hand side of
      * that sequence.
      *
      * @param source      The source of the conflict.
      * @param root        The sequence in which the conflict occured.
      * @param ambiguities The conflicting kinds.
      */
    case class FollowConflict(source: Disjunction[_],
                              root: Syntax[_] with SequenceLike[_, _],
                              ambiguities: Set[Kind]) extends LL1Conflict
  }

  import LL1Conflict._

  /** Follow-last set tagged with its source. */
  private[scallion] case class FollowLastEntry(source: Disjunction[_], kinds: Set[Kind])

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

      override val first: Set[Kind] =
        Set()

      override def followLast: Set[Kind] =
        Set()

      override protected def followLastEntries: Set[FollowLastEntry] =
        Set()

      override val isLL1: Boolean =
        true

      override def conflicts: Set[LL1Conflict] =
        Set()

      override def kinds: Set[Kind] =
        Set()

      override protected def computeNullable(cells: IHM[Recursive[_], Cell[_]], callback: A => Unit): Unit =
        callback(value)

      override protected def computeIsProductive(cells: IHM[Recursive[_], Cell[Unit]], callback: Unit => Unit): Unit =
        callback(())

      override protected def computeFirst(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                          callback: Set[Kind] => Unit): Unit =
        ()

      override protected def computeKinds(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                          callback: Set[Kind] => Unit): Unit =
        ()

      override protected def computeFollowLast(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                               callback: Set[Kind] => Unit): Unit =
        ()

      override protected def computeFollowLastEntries(cells: IHM[Recursive[_], Cell[Set[FollowLastEntry]]],
                                                      callback: Set[FollowLastEntry] => Unit): Unit =
        ()

      override protected def computeIsLL1(cells: IHM[Recursive[_], Cell[Unit]], callback: Unit => Unit): Unit =
        ()

      override protected def computeConflicts(cells: IHM[Recursive[_], Cell[Set[LL1Conflict]]],
                                              callback: Set[LL1Conflict] => Unit): Unit =
        ()

      override protected def computePrefixHelper(syntax: Syntax[_],
                                                 recs: IHM[Recursive[_], Recursive[Unit]]): Syntax[Unit] =
        Failure()

      override protected def collectTrails(recs: Map[RecId, () => Producer[Seq[Kind]]]): Producer[Seq[Kind]] =
        Producer.single(Vector())

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Syntax[_]]): Syntax[A] =
        this

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

      override val first: Set[Kind] =
        Set()

      override def followLast: Set[Kind] =
        Set()

      override protected def followLastEntries: Set[FollowLastEntry] =
        Set()

      override val isLL1: Boolean =
        true

      override def conflicts: Set[LL1Conflict] =
        Set()

      override def kinds: Set[Kind] =
        Set()

      override protected def computeNullable(cells: IHM[Recursive[_], Cell[_]], callback: A => Unit): Unit =
        ()

      override protected def computeIsProductive(cells: IHM[Recursive[_], Cell[Unit]], callback: Unit => Unit): Unit =
        ()

      override protected def computeFirst(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                          callback: Set[Kind] => Unit): Unit =
        ()

      override protected def computeKinds(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                          callback: Set[Kind] => Unit): Unit =
        ()

      override protected def computeFollowLast(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                               callback: Set[Kind] => Unit): Unit =
        ()

      override protected def computeFollowLastEntries(cells: IHM[Recursive[_], Cell[Set[FollowLastEntry]]],
                                                      callback: Set[FollowLastEntry] => Unit): Unit =
        ()

      override protected def computeIsLL1(cells: IHM[Recursive[_], Cell[Unit]], callback: Unit => Unit): Unit =
        ()

      override protected def computeConflicts(cells: IHM[Recursive[_], Cell[Set[LL1Conflict]]],
                                              callback: Set[LL1Conflict] => Unit): Unit =
        ()

      override protected def computePrefixHelper(syntax: Syntax[_],
                                                 recs: IHM[Recursive[_], Recursive[Unit]]): Syntax[Unit] =
        Failure()

      override protected def collectTrails(recs: Map[RecId, () => Producer[Seq[Kind]]]): Producer[Seq[Kind]] =
        Producer.empty

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Syntax[_]]): Syntax[A] =
        this

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

      override val first: Set[Kind] =
        Set(kind)

      override def followLast: Set[Kind] =
        Set()

      override protected def followLastEntries: Set[FollowLastEntry] =
        Set()

      override def isLL1: Boolean =
        true

      override def conflicts: Set[LL1Conflict] =
        Set()

      override def kinds: Set[Kind] =
        Set(kind)

      override protected def computeNullable(cells: IHM[Recursive[_], Cell[_]], callback: Token => Unit): Unit =
        ()

      override protected def computeIsProductive(cells: IHM[Recursive[_], Cell[Unit]], callback: Unit => Unit): Unit =
        callback(())

      override protected def computeFirst(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                          callback: Set[Kind] => Unit): Unit =
        callback(Set(kind))

      override protected def computeFollowLast(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                               callback: Set[Kind] => Unit): Unit =
        ()

      override protected def computeFollowLastEntries(cells: IHM[Recursive[_], Cell[Set[FollowLastEntry]]],
                                                      callback: Set[FollowLastEntry] => Unit): Unit =
        ()

      override protected def computeIsLL1(cells: IHM[Recursive[_], Cell[Unit]], callback: Unit => Unit): Unit =
        ()

      override protected def computeConflicts(cells: IHM[Recursive[_], Cell[Set[LL1Conflict]]],
                                              callback: Set[LL1Conflict] => Unit): Unit =
        ()

      override protected def computeKinds(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                          callback: Set[Kind] => Unit): Unit =
        callback(Set(kind))

      override protected def computePrefixHelper(syntax: Syntax[_],
                                                 recs: IHM[Recursive[_], Recursive[Unit]]): Syntax[Unit] =
        Failure()

      override protected def collectTrails(recs: Map[RecId, () => Producer[Seq[Kind]]]): Producer[Seq[Kind]] =
        Producer.single(Vector(kind))

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Syntax[_]]): Syntax[Token] =
        if (predicate(kind)) this else Failure()

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

      override lazy val first: Set[Kind] =
        inner.first

      override def followLast: Set[Kind] =
        inner.followLast

      override protected def followLastEntries: Set[FollowLastEntry] =
        inner.followLastEntries

      override def isLL1: Boolean =
        inner.isLL1

      override def conflicts: Set[LL1Conflict] =
        inner.conflicts

      override def kinds: Set[Kind] =
        inner.kinds

      override protected def computeNullable(cells: IHM[Recursive[_], Cell[_]], callback: B => Unit): Unit = {
        val transformed = new TransformOnce(callback, function)
        inner.computeNullable(cells, transformed)
      }

      override protected def computeIsProductive(cells: IHM[Recursive[_], Cell[Unit]], callback: Unit => Unit): Unit =
        inner.computeIsProductive(cells, callback)

      override protected def computeFirst(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                          callback: Set[Kind] => Unit): Unit =
        inner.computeFirst(cells, callback)

      override protected def computeFollowLast(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                               callback: Set[Kind] => Unit): Unit =
        inner.computeFollowLast(cells, callback)

      override protected def computeFollowLastEntries(cells: IHM[Recursive[_], Cell[Set[FollowLastEntry]]],
                                                      callback: Set[FollowLastEntry] => Unit): Unit =
        inner.computeFollowLastEntries(cells, callback)

      override protected def computeIsLL1(cells: IHM[Recursive[_], Cell[Unit]], callback: Unit => Unit): Unit =
        inner.computeIsLL1(cells, callback)

      override protected def computeConflicts(cells: IHM[Recursive[_], Cell[Set[LL1Conflict]]],
                                              callback: Set[LL1Conflict] => Unit): Unit =
        inner.computeConflicts(cells, callback)

      override protected def computeKinds(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                          callback: Set[Kind] => Unit): Unit =
        inner.computeKinds(cells, callback)

      override protected def computePrefixHelper(syntax: Syntax[_],
                                                 recs: IHM[Recursive[_], Recursive[Unit]]): Syntax[Unit] =
        inner.computePrefix(syntax, recs)

      override protected def collectTrails(recs: Map[RecId, () => Producer[Seq[Kind]]]): Producer[Seq[Kind]] =
        inner.collectTrails(recs)

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Syntax[_]]): Syntax[B] =
        inner.collectFilter(predicate, recs).map(function, inverse)

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

      override lazy val isProductive: Boolean =
        left.isProductive && right.isProductive

      override lazy val first: Set[Kind] =
        if (!right.isProductive) {
          Set()
        }
        else if (!left.isNullable) {
          left.first
        }
        else {
          left.first union right.first
        }

      override def followLast: Set[Kind] =
        if (!left.isProductive) {
          Set()
        }
        else if (!right.isNullable) {
          right.followLast
        }
        else {
          right.followLast union left.followLast
        }

      override protected def followLastEntries: Set[FollowLastEntry] =
        if (!left.isProductive) {
          Set()
        }
        else if (!right.isNullable) {
          right.followLastEntries
        }
        else {
          right.followLastEntries union left.followLastEntries
        }

      override def isLL1: Boolean =
        (left.followLast intersect right.first).isEmpty &&
        left.isLL1 &&
        right.isLL1

      override def conflicts: Set[LL1Conflict] = {
        val firstRight = right.first

        val followConflicts: Set[LL1Conflict] = left.followLastEntries.flatMap {
          case FollowLastEntry(source, entry) => {
            val inter = entry intersect firstRight

            if (inter.isEmpty) {
              None
            }
            else {
              Some(FollowConflict(source, this, inter))
            }
          }
        }

        left.conflicts union
        right.conflicts union
        followConflicts
      }

      override def kinds: Set[Kind] =
        left.kinds union right.kinds

      override protected def computeIsProductive(cells: IHM[Recursive[_], Cell[Unit]], callback: Unit => Unit): Unit = {
        val merged = new MergeOnce((_: Unit, _: Unit) => callback(()))
        left.computeIsProductive(cells, merged.left)
        right.computeIsProductive(cells, merged.right)
      }

      override protected def computeFirst(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                          callback: Set[Kind] => Unit): Unit =
        if (right.isProductive) {
          left.computeFirst(cells, callback)

          if (left.isNullable) {
            right.computeFirst(cells, callback)
          }
        }

      override protected def computeFollowLast(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                               callback: Set[Kind] => Unit): Unit =
        if (left.isProductive) {
          right.computeFollowLast(cells, callback)

          if (right.isNullable) {
            left.computeFollowLast(cells, callback)
          }
        }

      override protected def computeFollowLastEntries(cells: IHM[Recursive[_], Cell[Set[FollowLastEntry]]],
                                                      callback: Set[FollowLastEntry] => Unit): Unit =
        if (left.isProductive) {
          right.computeFollowLastEntries(cells, callback)

          if (right.isNullable) {
            left.computeFollowLastEntries(cells, callback)
          }
        }

      override protected def computeIsLL1(cells: IHM[Recursive[_], Cell[Unit]], callback: Unit => Unit): Unit = {
        if ((left.followLast intersect right.first).nonEmpty) {
          callback(())
        }

        left.computeIsLL1(cells, callback)
        right.computeIsLL1(cells, callback)
      }

      override protected def computeConflicts(cells: IHM[Recursive[_], Cell[Set[LL1Conflict]]],
                                              callback: Set[LL1Conflict] => Unit): Unit = {
        val firstRight = right.first

        val followConflicts: Set[LL1Conflict] = left.followLastEntries.flatMap {
          case FollowLastEntry(source, entry) => {
            val inter = entry intersect firstRight

            if (inter.isEmpty) {
              None
            }
            else {
              Some(FollowConflict(source, this, inter))
            }
          }
        }

        if (followConflicts.nonEmpty) {
          callback(followConflicts)
        }

        left.computeConflicts(cells, callback)
        right.computeConflicts(cells, callback)
      }

      override protected def computeKinds(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                          callback: Set[Kind] => Unit): Unit = {
        left.computeKinds(cells, callback)
        right.computeKinds(cells, callback)
      }

      override protected def computePrefixHelper(syntax: Syntax[_],
                                                 recs: IHM[Recursive[_], Recursive[Unit]]): Syntax[Unit] =
        left.computePrefix(syntax, recs) | left.unit() ~>~ right.computePrefix(syntax, recs)

      override protected def collectTrails(recs: Map[RecId, () => Producer[Seq[Kind]]]): Producer[Seq[Kind]] =
        kindSeqOps.product(left.collectTrails(recs), right.collectTrails(recs))
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

      override lazy val nullable: Option[A ~ B] = for {
        leftValue <- left.nullable
        rightValue <- right.nullable
      } yield scallion.syntactic.~(leftValue, rightValue)

      override protected def computeNullable(cells: IHM[Recursive[_], Cell[_]], callback: (A ~ B) => Unit): Unit = {
        val merged = new MergeOnce((a: A, b: B) => callback(scallion.syntactic.~(a, b)))
        left.computeNullable(cells, merged.left)
        right.computeNullable(cells, merged.right)
      }

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

      override lazy val nullable: Option[Seq[A]] = for {
        leftValue <- left.nullable
        rightValue <- right.nullable
      } yield leftValue ++ rightValue

      override protected def computeNullable(cells: IHM[Recursive[_], Cell[_]], callback: Seq[A] => Unit): Unit = {
        val merged = new MergeOnce((xs: Seq[A], ys: Seq[A]) => callback(xs ++ ys))
        left.computeNullable(cells, merged.left)
        right.computeNullable(cells, merged.right)
      }

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

      override lazy val nullable: Option[A] =
        left.nullable orElse right.nullable

      override lazy val isProductive: Boolean =
        left.isProductive || right.isProductive

      override lazy val first: Set[Kind] =
        left.first union right.first

      override def followLast: Set[Kind] = {
        val fromLeft = if (right.isNullable) left.first else Set.empty[Kind]
        val fromRight = if (left.isNullable) right.first else Set.empty[Kind]

        right.followLast union left.followLast union fromLeft union fromRight
      }

      override protected def followLastEntries: Set[FollowLastEntry] = {
        val fromLeft = if (right.isNullable) Set(FollowLastEntry(this, left.first)) else Set.empty[FollowLastEntry]
        val fromRight = if (left.isNullable) Set(FollowLastEntry(this, right.first)) else Set.empty[FollowLastEntry]

        right.followLastEntries union left.followLastEntries union fromLeft union fromRight
      }

      override def isLL1: Boolean =
        (left.first intersect right.first).isEmpty &&
        !(left.isNullable && right.isNullable) &&
        left.isLL1 &&
        right.isLL1

      override def conflicts: Set[LL1Conflict] = {
        val problematic = left.first intersect right.first

        left.conflicts union
        right.conflicts union
        (if (left.isNullable && right.isNullable) Set(NullableConflict(this)) else Set()) union
        (if (problematic.nonEmpty) Set(FirstConflict(this, problematic)) else Set())
      }

      override def kinds: Set[Kind] =
        left.kinds union right.kinds

      override protected def computeNullable(cells: IHM[Recursive[_], Cell[_]], callback: A => Unit): Unit = {
        left.computeNullable(cells, callback)
        right.computeNullable(cells, callback)
      }

      override protected def computeIsProductive(cells: IHM[Recursive[_], Cell[Unit]], callback: Unit => Unit): Unit = {
        left.computeIsProductive(cells, callback)
        right.computeIsProductive(cells, callback)
      }

      override protected def computeFirst(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                          callback: Set[Kind] => Unit): Unit = {
        left.computeFirst(cells, callback)
        right.computeFirst(cells, callback)
      }

      override protected def computeFollowLast(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                               callback: Set[Kind] => Unit): Unit = {
        right.computeFollowLast(cells, callback)
        left.computeFollowLast(cells, callback)

        if (right.isNullable) {
          callback(left.first)
        }
        if (left.isNullable) {
          callback(right.first)
        }
      }

      override protected def computeFollowLastEntries(cells: IHM[Recursive[_], Cell[Set[FollowLastEntry]]],
                                                      callback: Set[FollowLastEntry] => Unit): Unit = {
        right.computeFollowLastEntries(cells, callback)
        left.computeFollowLastEntries(cells, callback)

        if (right.isNullable) {
          callback(Set(FollowLastEntry(this, left.first)))
        }
        if (left.isNullable) {
          callback(Set(FollowLastEntry(this, right.first)))
        }
      }

      override protected def computeIsLL1(cells: IHM[Recursive[_], Cell[Unit]], callback: Unit => Unit): Unit = {
        if ((left.first intersect right.first).nonEmpty || (left.isNullable && right.isNullable)) {
          callback(())
        }

        left.computeIsLL1(cells, callback)
        right.computeIsLL1(cells, callback)
      }

      override protected def computeConflicts(cells: IHM[Recursive[_], Cell[Set[LL1Conflict]]],
                                              callback: Set[LL1Conflict] => Unit): Unit = {
        val problematic = left.first intersect right.first

        if (problematic.nonEmpty) {
          callback(Set(FirstConflict(this, problematic)))
        }

        if (left.isNullable && right.isNullable) {
          callback(Set(NullableConflict(this)))
        }

        left.computeConflicts(cells, callback)
        right.computeConflicts(cells, callback)
      }

      override protected def computeKinds(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                          callback: Set[Kind] => Unit): Unit = {
        left.computeKinds(cells, callback)
        right.computeKinds(cells, callback)
      }

      override protected def computePrefixHelper(syntax: Syntax[_],
                                                 recs: IHM[Recursive[_], Recursive[Unit]]): Syntax[Unit] =
        left.computePrefix(syntax, recs) | right.computePrefix(syntax, recs)

      override protected def collectTrails(recs: Map[RecId, () => Producer[Seq[Kind]]]): Producer[Seq[Kind]] =
        kindSeqOps.union(left.collectTrails(recs), right.collectTrails(recs))

      override protected def derive(token: Token, kind: Kind): Syntax[A] = {
        if (left.first.contains(kind)) {
          left.derive(token, kind)
        }
        else {
          right.derive(token, kind)
        }
      }

      override protected def collectFilter(
          predicate: Kind => Boolean,
          recs: Map[RecId, Syntax[_]]): Syntax[A] =
        left.collectFilter(predicate, recs) | right.collectFilter(predicate, recs)

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

      private var nullableCacheValid: Boolean = false
      private var nullableCacheValue: Option[A] = None
      override def nullable: Option[A] = {
        if (!nullableCacheValid) {
          val cell = new CellOnce[A]({ optValue =>
            nullableCacheValue = optValue
            nullableCacheValid = true
          })
          val cells = new IHM[Recursive[_], Cell[_]]()
          cells.put(this, cell)
          inner.computeNullable(cells, cell)
          for (other <- cells.values().asScala) {
            other.complete()
          }
        }
        nullableCacheValue
      }

      override protected def computeNullable(cells: IHM[Recursive[_], Cell[_]], callback: A => Unit): Unit = {
        if (nullableCacheValid) {
          nullableCacheValue.foreach(value => callback(value))
        }
        else if (cells.containsKey(this)) {
          val cell = cells.get(this).asInstanceOf[Cell[A]]
          cell.register(callback)
        }
        else {
          val cell = new CellOnce[A]({ optValue =>
            nullableCacheValue = optValue
            nullableCacheValid = true
          })
          cell.register(callback)
          cells.put(this, cell)
          inner.computeNullable(cells, cell)
        }
      }

      private var isProductiveCacheValid: Boolean = false
      private var isProductiveCacheValue: Boolean = false
      override def isProductive: Boolean = {
        if (!isProductiveCacheValid) {
          val cell = new CellOnce[Unit]({ optValue =>
            isProductiveCacheValue = optValue.nonEmpty
            isProductiveCacheValid = true
          })
          val cells = new IHM[Recursive[_], Cell[Unit]]()
          cells.put(this, cell)
          inner.computeIsProductive(cells, cell)
          for (other <- cells.values().asScala) {
            other.complete()
          }
        }
        isProductiveCacheValue
      }

      override protected def computeIsProductive(cells: IHM[Recursive[_], Cell[Unit]], callback: Unit => Unit): Unit = {
        if (isProductiveCacheValid) {
          if (isProductiveCacheValue) {
            callback(())
          }
        }
        else if (cells.containsKey(this)) {
          val cell = cells.get(this)
          cell.register(callback)
        }
        else {
          val cell = new CellOnce[Unit]({ optValue =>
            isProductiveCacheValue = optValue.nonEmpty
            isProductiveCacheValid = true
          })
          cell.register(callback)
          cells.put(this, cell)
          inner.computeIsProductive(cells, cell)
        }
      }

      private var firstCacheValid: Boolean = false
      private var firstCacheValue: Set[Kind] = Set()
      override def first: Set[Kind] = {
        if (!firstCacheValid) {
          val cell = new CellUpgradableSet[Kind]({ result =>
            firstCacheValue = result
            firstCacheValid = true
          })
          val cells = new IHM[Recursive[_], Cell[Set[Kind]]]()
          cells.put(this, cell)
          inner.computeFirst(cells, cell)
          for (other <- cells.values().asScala) {
            other.complete()
          }
        }
        firstCacheValue
      }

      override protected def computeFirst(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                          callback: Set[Kind] => Unit): Unit = {
        if (firstCacheValid) {
          if (firstCacheValue.nonEmpty) {
            callback(firstCacheValue)
          }
        }
        else if (cells.containsKey(this)) {
          val cell = cells.get(this)
          cell.register(callback)
        }
        else {
          val cell = new CellUpgradableSet[Kind]({ result =>
            firstCacheValue = result
            firstCacheValid = true
          })
          cell.register(callback)
          cells.put(this, cell)
          inner.computeFirst(cells, cell)
        }
      }

      private var followLastCacheValid: Boolean = false
      private var followLastCacheValue: Set[Kind] = Set()
      override def followLast: Set[Kind] = {
        if (!followLastCacheValid) {
          val cell = new CellUpgradableSet[Kind]({ result =>
            followLastCacheValue = result
            followLastCacheValid = true
          })
          val cells = new IHM[Recursive[_], Cell[Set[Kind]]]()
          cells.put(this, cell)
          inner.computeFollowLast(cells, cell)
          for (other <- cells.values().asScala) {
            other.complete()
          }
        }
        followLastCacheValue
      }

      override protected def computeFollowLast(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                               callback: Set[Kind] => Unit): Unit = {
        if (followLastCacheValid) {
          if (followLastCacheValue.nonEmpty) {
            callback(followLastCacheValue)
          }
        }
        else if (cells.containsKey(this)) {
          val cell = cells.get(this)
          cell.register(callback)
        }
        else {
          val cell = new CellUpgradableSet[Kind]({ result =>
            followLastCacheValue = result
            followLastCacheValid = true
          })
          cell.register(callback)
          cells.put(this, cell)
          inner.computeFollowLast(cells, cell)
        }
      }

      private var followLastEntriesCacheValid: Boolean = false
      private var followLastEntriesCacheValue: Set[FollowLastEntry] = Set()
      override protected def followLastEntries: Set[FollowLastEntry] = {
        if (!followLastEntriesCacheValid) {
          val cell = new CellUpgradableSet[FollowLastEntry]({ result =>
            followLastEntriesCacheValue = result
            followLastEntriesCacheValid = true
          })
          val cells = new IHM[Recursive[_], Cell[Set[FollowLastEntry]]]()
          cells.put(this, cell)
          inner.computeFollowLastEntries(cells, cell)
          for (other <- cells.values().asScala) {
            other.complete()
          }
        }
        followLastEntriesCacheValue
      }

      override protected def computeFollowLastEntries(cells: IHM[Recursive[_], Cell[Set[FollowLastEntry]]],
                                                      callback: Set[FollowLastEntry] => Unit): Unit = {
        if (followLastEntriesCacheValid) {
          if (followLastEntriesCacheValue.nonEmpty) {
            callback(followLastEntriesCacheValue)
          }
        }
        else if (cells.containsKey(this)) {
          val cell = cells.get(this)
          cell.register(callback)
        }
        else {
          val cell = new CellUpgradableSet[FollowLastEntry]({ result =>
            followLastEntriesCacheValue = result
            followLastEntriesCacheValid = true
          })
          cell.register(callback)
          cells.put(this, cell)
          inner.computeFollowLastEntries(cells, cell)
        }
      }


      private var isLL1CacheValid: Boolean = false
      private var isLL1CacheValue: Boolean = true
      override def isLL1: Boolean = {
        if (!isLL1CacheValid) {
          val cell = new CellOnce[Unit]({ optValue =>
            isLL1CacheValue = optValue.isEmpty
            isLL1CacheValid = true
          })
          val cells = new IHM[Recursive[_], Cell[Unit]]()
          cells.put(this, cell)
          inner.computeIsLL1(cells, cell)
          for (other <- cells.values().asScala) {
            other.complete()
          }
        }
        isLL1CacheValue
      }

      override protected def computeIsLL1(cells: IHM[Recursive[_], Cell[Unit]], callback: Unit => Unit): Unit = {
        if (isLL1CacheValid) {
          if (!isLL1CacheValue) {
            callback(())
          }
        }
        else if (cells.containsKey(this)) {
          val cell = cells.get(this)
          cell.register(callback)
        }
        else {
          val cell = new CellOnce[Unit]({ optValue =>
            isLL1CacheValue = optValue.isEmpty
            isLL1CacheValid = true
          })
          cell.register(callback)
          cells.put(this, cell)
          inner.computeIsLL1(cells, cell)
        }
      }

      private var conflictsCacheValid: Boolean = false
      private var conflictsCacheValue: Set[LL1Conflict] = Set()
      override def conflicts: Set[LL1Conflict] = {
        if (!conflictsCacheValid) {
          val cell = new CellUpgradableSet[LL1Conflict]({ result =>
            conflictsCacheValue = result
            conflictsCacheValid = true
          })
          val cells = new IHM[Recursive[_], Cell[Set[LL1Conflict]]]()
          cells.put(this, cell)
          inner.computeConflicts(cells, cell)
          for (other <- cells.values().asScala) {
            other.complete()
          }
        }
        conflictsCacheValue
      }

      override protected def computeConflicts(cells: IHM[Recursive[_], Cell[Set[LL1Conflict]]],
                                              callback: Set[LL1Conflict] => Unit): Unit = {
        if (conflictsCacheValid) {
          if (conflictsCacheValue.nonEmpty) {
            callback(conflictsCacheValue)
          }
        }
        else if (cells.containsKey(this)) {
          val cell = cells.get(this)
          cell.register(callback)
        }
        else {
          val cell = new CellUpgradableSet[LL1Conflict]({ result =>
            conflictsCacheValue = result
            conflictsCacheValid = true
          })
          cell.register(callback)
          cells.put(this, cell)
          inner.computeConflicts(cells, cell)
        }
      }

      private var kindsCacheValid: Boolean = false
      private var kindsCacheValue: Set[Kind] = Set()
      override def kinds: Set[Kind] = {
        if (!kindsCacheValid) {
          val cell = new CellUpgradableSet[Kind]({ result =>
            kindsCacheValue = result
            kindsCacheValid = true
          })
          val cells = new IHM[Recursive[_], Cell[Set[Kind]]]()
          cells.put(this, cell)
          inner.computeKinds(cells, cell)
          for (other <- cells.values().asScala) {
            other.complete()
          }
        }
        kindsCacheValue
      }

      override protected def computeKinds(cells: IHM[Recursive[_], Cell[Set[Kind]]],
                                          callback: Set[Kind] => Unit): Unit = {
        if (kindsCacheValid) {
          if (kindsCacheValue.nonEmpty) {
            callback(kindsCacheValue)
          }
        }
        else if (cells.containsKey(this)) {
          val cell = cells.get(this)
          cell.register(callback)
        }
        else {
          val cell = new CellUpgradableSet[Kind]({ result =>
            kindsCacheValue = result
            kindsCacheValid = true
          })
          cell.register(callback)
          cells.put(this, cell)
          inner.computeKinds(cells, cell)
        }
      }

      override protected def computePrefixHelper(syntax: Syntax[_],
                                                 recs: IHM[Recursive[_], Recursive[Unit]]): Syntax[Unit] = {

        if (recs.containsKey(this)) {
          recs.get(this)
        }
        else {
          val rec: Recursive[Unit] = Recursive.create {
            inner.computePrefix(syntax, recs)
          }
          recs.put(this, rec)
          rec
        }
      }

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

  /** Wrapper around a `Syntax` indicating that values from
    * the inner `syntax` should be ignored when building up sequences
    * using `~`.
    *
    * @group other
    */
  case class Skip(syntax: Syntax[Unit]) {

    /** Sequences `this` and `that` syntax.
      * The parsed value from `that` is returned.
      *
      * @group combinator
      */
    def ~[A](that: Syntax[A]): Syntax[A] = this.syntax ~>~ that

    /** Sequences `this` and `that` skipped syntax.
      * Results in a skipped syntax.
      *
      * @group combinator
      */
    def ~(that: Skip): Skip = Skip(this.syntax ~>~ that.syntax)
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
