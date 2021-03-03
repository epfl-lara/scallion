/* Copyright 2020 EPFL, Lausanne
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

import scala.annotation.implicitNotFound
import scala.language.higherKinds
import scala.util.Try
import scala.collection.mutable.Map

/** Contains the definition of syntaxes. */
trait Syntaxes {

  /** Type of tokens.
    *
    * @group abstract
    */
  type Token

  /** Type of kinds.
    *
    * @group abstract
    */
  type Kind

  /** Returns the kind associated with `token`.
    *
    * @group abstract
    */
  def getKind(token: Token): Kind

  /** Unique identifier for Recursive syntaxes.
    *
    * @group other
    */
  type RecId = Int

  /** Type of markings.
    *
    * @group other
    */
  type Mark = String

  /** Low priority implicits.
    * Contains an instance for [[Uninteresting]] for every type.
    *
    * @group implicit
    */
  trait UnsafeImplicits {

    /** Implicit instance for `Uninteresting[A]` for any `A`. */
    implicit def anyUninteresting[A]: Uninteresting[A] =
      new Uninteresting[A] {
        override def unit(syntax: Syntax[A]): Syntax[Unit] =
          syntax.map(_ => ())
      }
  }

  /** The [[Uninteresting]] instance for `Unit`.
    *
    * @group implicit
    */
  private object UnitUninteresting extends Uninteresting[Unit] {
    override def unit(syntax: Syntax[Unit]): Syntax[Unit] =
      syntax
  }

  /** Contains an instance for [[Uninteresting]] for every type.
    * The [[Uninteresting]] instance for `Unit` is distinct.
    *
    * @group implicit
    */
  object Implicits extends UnsafeImplicits {

    /** Implicit instance for `Uninteresting[Unit]`. */
    implicit val unitUninteresting: Uninteresting[Unit] =
      UnitUninteresting
  }

  /** Contains the instance for [[Uninteresting]] of `Unit`.
    *
    * @group implicit
    */
  object SafeImplicits {

    /** Implicit instance for `Uninteresting[Unit]`. */
    implicit val unitUninteresting: Uninteresting[Unit] =
      UnitUninteresting
  }

  import SafeImplicits._
  import Syntax._

  /** Represents a syntax.
    *
    * Acts as both a parser and a pretty printer.
    *
    * @tparam A the type of values that can be produced or printed.
    *
    * @group syntax
    */
  sealed trait Syntax[A] {

    private[scallion] val trace = Traces.get

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
        // case Success(value, predicate) =>
        //   Success(function(value), (y: B) => Try(inverse(y)).getOrElse(Seq()).map(predicate).sum)
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
        case (Success(a), Success(b)) => Success(a ++ b)
        case _ => ev(this).~(that).map({
          case xs ~ ys => xs ++ ys
        }, {
          (xys: Seq[B]) => {
            for (i <- 0 to xys.size) yield {
              val (xs, ys) = xys.splitAt(i)
              xs ~ ys
            }
          }
        })
      }

    /** Sequences `this` and `that` syntax. The parsed value from `this` is returned.
      *
      * @group combinator
      */
    def ~(that: Skip): Syntax[A] = this ~<~ that.syntax

    /** @usecase def skip: Skip
      *
      * Indicates that the value from `this` syntax should be ignored
      * when building up sequences using `~`.
      *
      * {{{
      * // Assume are not interesed in values produced by open, separator or close.
      * // We call .skip on them while building up a sequence to skip them in the
      * // resulting sequence of values.
      * (open.skip ~ key ~ separator.skip ~ value ~ close.skip).map {
      *   // Then, we can just apply a transformation over the two values
      *   // that have not been skipped.
      *   case k ~ v => (k, v)
      * }
      * }}}
      *
      * @group combinator
      */
    def skip(implicit ev: Uninteresting[A]): Skip =
      Skip(ev.unit(this))

    /** @usecase def ~>~[B](that: Syntax[B]): Syntax[B]
      *
      * Sequences `this` and `that` syntax. The parsed value from `that` is returned.
      *
      * @group combinator
      */
    def ~>~[B](that: Syntax[B])(implicit ev: Uninteresting[A]): Syntax[B] =
      ev.unit(this).~(that).map(_._2, {
        case x => Seq(scallion.~((), x))
      })

    /** @usecase def ~<~[B](that: Syntax[B]): Syntax[A]
      *
      * Sequences `this` and `that` syntax. The parsed value from `this` is returned.
      *
      * @group combinator
      */
    def ~<~[B](that: Syntax[B])(implicit ev: Uninteresting[B]): Syntax[A] =
      this.~(ev.unit(that)).map(_._1, {
        case x => Seq(scallion.~(x, ()))
      })

    /** Sequences `this` and `that` syntax.
      * The parsed value from `that` is appended to that from `this`.
      *
      * @group combinator
      */
    def :+[B](that: Syntax[B])
        (implicit ev: Syntax[A] =:= Syntax[Seq[B]]): Syntax[Seq[B]] =
      ev(this).~(that).map({
        case xs ~ x => xs :+ x
      }, {
        case xs if xs.size >= 1 => Seq(xs.init ~ xs.last)
        case _ => Seq()
      })

    /** Sequences `this` and `that` syntax.
      * The parsed value from `that` is prepended to that from `this`.
      *
      * @group combinator
      */
    def +:[B](that: Syntax[B])
        (implicit ev: Syntax[A] =:= Syntax[Seq[B]]): Syntax[Seq[B]] =
      that.~(ev(this)).map({
        case x ~ xs => x +: xs
      }, {
        case xs if xs.size >= 1 => Seq(xs.head ~ xs.tail)
        case _ => Seq()
      })

    /** Sequences `this` and `that` syntax. The parsed values are returned as a pair.
      *
      * @group combinator
      */
    def ~[B](that: Syntax[B]): Syntax[A ~ B] = (this, that) match {
      case (Failure(), _) => Failure()
      case (_, Failure()) => Failure()
      case (Success(a), Success(b)) => Success(scallion.~(a, b))
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
    def ||[B](that: Syntax[B]): Syntax[Either[A, B]] =
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
      * equivalent values.
      *
      * Parsed values are replaced by `()`, while printed values
      * are replaced by the various given values.
      *
      * @group combinator
      */
    def unit(value: A, values: A*): Syntax[Unit] = this.map(_ => (), {
      case () => value +: values
    })

    /** Upcasts `this` syntax.
      *
      * The resulting `syntax` parses and pretty prints equivalently to `this` syntax.
      *
      * @group combinator
      */
    def up[B >: A](implicit ev: Manifest[A]): Syntax[B] =
      this.map((x: A) => x, (y: B) => ev.unapply(y) match {
        case None => Seq()
        case Some(x) => Seq(x)
      })

    /** Marks `this` syntax.
      *
      * @group combinator
      */
    def mark(mark: Mark): Syntax[A] = Marked(mark, this)


    private[scallion] def prefixOf(needle: Syntax[_]): Syntax[_] = prefixOf(needle, Map.empty)

    protected def prefixOf(needle: Syntax[_], recs: Map[RecId, Recursive[_]]): Syntax[_] = {

      if (this eq needle) {
        epsilon(())
      }
      else {
        prefixOfHelper(needle, recs)
      }
    }

    protected def prefixOfHelper(needle: Syntax[_], recs: Map[RecId, Recursive[_]]): Syntax[_]
  }

  /** Contains primitive basic syntaxes and syntax combinators.
    *
    * @group syntax
    */
  object Syntax {

    /** Syntax for the empty string.
      *
      * @param value   The value produced.
      *
      * @group basic
      */
    case class Success[A](value: A) extends Syntax[A] {
      override protected def prefixOfHelper(needle: Syntax[_], recs: Map[RecId, Recursive[_]]): Syntax[_] = failure
    }

    /** Empty syntax.
      *
      * @group basic
      */
    case class Failure[A]() extends Syntax[A] {
      override protected def prefixOfHelper(needle: Syntax[_], recs: Map[RecId, Recursive[_]]): Syntax[_] = failure
    }

    /** Syntax that describes a single token of the given `kind`.
      *
      * @param kind The kind accepted by the syntax.
      *
      * @group basic
      */
    case class Elem(kind: Kind) extends Syntax[Token] {
      override protected def prefixOfHelper(needle: Syntax[_], recs: Map[RecId, Recursive[_]]): Syntax[_] = failure
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
      require(inner != null)

      override protected def prefixOfHelper(needle: Syntax[_], recs: Map[RecId, Recursive[_]]): Syntax[_] =
        inner.prefixOf(needle, recs)
    }

    case class Marked[A](mark: Mark, inner: Syntax[A]) extends Syntax[A] with Unary[A] {
      require(inner != null)

      override protected def prefixOfHelper(needle: Syntax[_], recs: Map[RecId, Recursive[_]]): Syntax[_] =
        inner.prefixOf(needle, recs)
    }

    /** Syntax that sequences the `left` and `right` syntaxes and pairs the results.
      *
      * @param left  The syntax for the prefix.
      * @param right The syntax for the suffix.
      *
      * @group combinator
      */
    case class Sequence[A, B](left: Syntax[A], right: Syntax[B])
        extends Syntax[A ~ B] with Binary[A, B] {
      require(left != null && right != null)

      override protected def prefixOfHelper(needle: Syntax[_], recs: Map[RecId, Recursive[_]]): Syntax[_] =
        left.prefixOf(needle, recs).asInstanceOf[Syntax[Any]] |
          (left ~ right.prefixOf(needle, recs)).asInstanceOf[Syntax[Any]]
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
      require(left != null && right != null)

      override protected def prefixOfHelper(needle: Syntax[_], recs: Map[RecId, Recursive[_]]): Syntax[_] =
        left.prefixOf(needle, recs).asInstanceOf[Syntax[Any]] |
          right.prefixOf(needle, recs).asInstanceOf[Syntax[Any]]
    }

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

      /** Extract the id and inner syntax of a `Recursive` syntax. */
      def unapply[A](that: Syntax[A]): Option[(RecId, Syntax[A])] = {
        if (that.isInstanceOf[Recursive[_]]) {
          val other = that.asInstanceOf[Recursive[A]]
          Some((other.id, other.inner))
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
        override val id = nextId()
        override lazy val inner: Syntax[A] = {
          Traces.push(trace)
          val res = syntax
          Traces.pop()
          res
        }
      }
    }

    /** Syntax that may recursively mention itself.
      *
      * @group combinator
      */
    sealed abstract class Recursive[A] extends Syntax[A] with Unary[A] {

      /** Unique identifier for this recursive syntax. */
      val id: RecId

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

      override protected def prefixOfHelper(needle: Syntax[_], recs: Map[RecId, Recursive[_]]): Syntax[_] =
        recs.get(id) match {
          case Some(rec) => rec
          case None => {
            lazy val inside: Syntax[_] = inner.prefixOf(needle, recs)
            val rec = Recursive.create(inside)
            recs += id -> rec
            inside  // Forcing inside.
            rec
          }
        }
    }
  }

  // "Skip" syntactic sugar

  /** Wrapper around a `Syntax` indicating that values from
    * the inner `syntax` should be ignored when building up sequences
    * using `~`.
    *
    * @param syntax The wrapped syntax.
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

  /** Typeclass to denote that values of a given type are
    * uninteresting and can be safely ignored while describing a syntax.
    *
    * @group other
    */
  @implicitNotFound(msg =
    "${A} is considered interesting, and can not be ignored. " +
    "Make sure to import either import SafeImplicits._ to make " +
    "Unit uninteresting, " +
    "or import Implicits._ to consider " +
    "all types as potentially uninteresting.")
  trait Uninteresting[A] {

    /** Converts a syntax that produces values of type `A`,
      * to a syntax that only produces the unit value.
      */
    def unit(syntax: Syntax[A]): Syntax[Unit]
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
  def epsilon[A](value: A): Syntax[A] = Success(value)

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

  private def optMark[A](syntax: Syntax[A], mark: Option[Mark]): Syntax[A] =
    mark match {
      case None => syntax
      case Some(mark) => syntax.mark(mark)
    }

  /** Syntax that represents 0 or more repetitions of the `rep` syntax.
    *
    * @group combinator
    */
  def many[A](rep: Syntax[A], mark: Option[Mark] = None): Syntax[Seq[A]] = {
    lazy val rest: Syntax[Seq[A]] = recursive {
      optMark(rep +: rest | epsilon(List()), mark)
    }
    rest
  }

  /** Syntax that represents 1 or more repetitions of the `rep` syntax.
    *
    * @group combinator
    */
  def many1[A](rep: Syntax[A], mark: Option[Mark] = None): Syntax[Seq[A]] = {
    rep +: many(rep, mark)
  }

  /** @usecase def repsep[A, B](rep: Syntax[A], sep: Syntax[B]): Syntax[Seq[A]]
    *
    * Syntax that represents 0 or more repetitions of the `rep` syntax, separated by `sep`.
    *
    * @group combinator
    */
  def repsep[A, B](rep: Syntax[A], sep: Syntax[B], mark: Option[Mark] = None)
      (implicit ev: Uninteresting[B]): Syntax[Seq[A]] = {
    optMark(rep1sep(rep, sep, mark)(ev) | epsilon(Vector()), mark)
  }

  /** @usecase def rep1sep[A, B](rep: Syntax[A], sep: Syntax[B]): Syntax[Seq[A]]
    *
    * Syntax that represents 1 or more repetitions of the `rep` syntax, separated by `sep`.
    *
    * @group combinator
    */
  def rep1sep[A, B](rep: Syntax[A], sep: Syntax[B], mark: Option[Mark] = None)
      (implicit ev: Uninteresting[B]): Syntax[Seq[A]] = {
    lazy val rest: Syntax[Seq[A]] = recursive {
      (sep ~>~ optMark(rep +: rest, mark)) | epsilon(Vector())
    }

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

private object Traces {

  private val traces = new ThreadLocal[List[(Int, List[StackTraceElement])]] {
    override protected def initialValue: List[(Int, List[StackTraceElement])] = List()
  }
  def get: List[StackTraceElement] = {
    val currents = traces.get()
    val tss = currents.take(1).map(_._2).flatten
    val n = currents.headOption.map(_._1).getOrElse(0)
    val ts = Thread.currentThread.getStackTrace.toList
    val k = ts.size
    ts.take(k - n).drop(2).takeWhile(!_.isNativeMethod()) ++ tss
  }
  def push(extra: List[StackTraceElement]): Unit = {
    val currents = traces.get()
    traces.set((Thread.currentThread.getStackTrace.length, extra) :: currents)
  }
  def pop(): Unit = {
    val currents = traces.get()
    traces.set(currents.tail)
  }
}