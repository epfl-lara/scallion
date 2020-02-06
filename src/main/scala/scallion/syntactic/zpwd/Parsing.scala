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

package scallion.syntactic
package zpwd

import scala.language.implicitConversions

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.collection.mutable.{ HashMap, ListBuffer }

import java.util.WeakHashMap

import scallion.util.internal._

/** This trait implements LL(1) parsing with derivatives. */
trait Parsing { self: Syntaxes =>

  /** Factory of LL(1) parsers. */
  object ZPWD {

    /** Cache of transformation from syntax to LL(1) parser. */
    private val syntaxToTreeCache: WeakHashMap[Syntax[_], Tree[_]] = new WeakHashMap()

    /** Builds a LL(1) parser from a syntax description.
      *
      * @param syntax     The description of the syntax.
      * @group parsing
      */
    def apply[A](syntax: Syntax[A]): Parser[A] = {

      // Handles caching.
      if (syntaxToTreeCache.containsKey(syntax)) {
        val tree: Tree[A] = syntaxToTreeCache.get(syntax).asInstanceOf[Tree[A]]
        return State(List(FocusedPair(tree, Context.Empty[A]())))
      }

      // Cache miss... Real work begins.

      val recCells: HashMap[RecId, Any] = new HashMap()

      def buildCell[A](syntax: Syntax[A]): SyntaxCell[A] = syntax match {
        case Syntax.Success(value, _) => SyntaxCell.Success(value, syntax)
        case Syntax.Failure() => SyntaxCell.Failure(syntax)
        case Syntax.Elem(kind) => SyntaxCell.Elem(kind, syntax)
        case Syntax.Disjunction(left, right) =>
          SyntaxCell.Disjunction(buildCell(left), buildCell(right), syntax)
        case Syntax.Sequence(left, right) =>
          SyntaxCell.Sequence(buildCell(left), buildCell(right), syntax)
        case Syntax.Transform(function, inverse, inner) =>
          SyntaxCell.Transform(buildCell(inner), function, inverse, syntax)
        case Syntax.Recursive(id, inner) => recCells.get(id) match {
          case None => {
            val rec = SyntaxCell.Recursive(buildCell(inner), id, syntax)
            recCells += id -> rec
            rec
          }
          case Some(rec) => rec.asInstanceOf[SyntaxCell.Recursive[A]]
        }
      }

      val syntaxCell = buildCell(syntax)

      syntaxCell.init()

      val recTrees: HashMap[RecId, Any] = new HashMap()

      def buildTree[A](syntaxCell: SyntaxCell[A]): Tree[A] = {
        val tree: Tree[A] = syntaxCell match {
          case SyntaxCell.Success(value, _) =>
            new Tree.Success(value) {
              override val nullable: Option[A] = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[A] = syntaxCell.syntax
            }
          case SyntaxCell.Failure(_) =>
            new Tree.Failure[A]() {
              override val nullable: Option[A] = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[A] = syntaxCell.syntax
            }
          case SyntaxCell.Elem(kind, _) =>
            new Tree.Elem(kind) {
              override val nullable: Option[Token] = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[Token] = syntaxCell.syntax
            }
          case SyntaxCell.Disjunction(left, right, _) =>
            new Tree.Disjunction[A](buildTree(left), buildTree(right)) {
              override val nullable: Option[A] = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[A] = syntaxCell.syntax
            }
          case SyntaxCell.Sequence(left: SyntaxCell[tA], right: SyntaxCell[tB], _) =>
            new Tree.Sequence[tA, tB](buildTree(left), buildTree(right)) {
              override val nullable: Option[tA ~ tB] = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[tA ~ tB] = syntaxCell.syntax
            }
          case SyntaxCell.Transform(inner: SyntaxCell[tA], function, inverse, _) =>
            new Tree.Transform[tA, A](buildTree(inner), function, inverse) {
              override val nullable: Option[A] = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[A] = syntaxCell.syntax
            }
          case SyntaxCell.Recursive(recInner, recId, _) => recTrees.get(recId) match {
            case None => {
              val rec = new Tree.Recursive[A] {
                override val id = recId
                override lazy val inner: Tree[A] = syntaxToTreeCache.get(recInner.syntax).asInstanceOf[Tree[A]]
                override val nullable: Option[A] = syntaxCell.nullableCell.get
                override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
                override val syntax: Syntax[A] = syntaxCell.syntax
              }

              recTrees += recId -> rec

              buildTree(recInner)

              rec
            }
            case Some(rec) => rec.asInstanceOf[Tree[A]]
          }
        }

        syntaxToTreeCache.put(syntaxCell.syntax, tree)

        tree
      }

      val tree: Tree[A] = buildTree(syntaxCell)

      State(List(FocusedPair(tree, Context.Empty[A]())))
    }

    private sealed trait SyntaxCell[A] {
      def init(): Unit

      val syntax: Syntax[A]
      val productiveCell: Cell[Unit, Unit, Boolean] = new BooleanCell
      val nullableCell: Cell[A, A, Option[A]] = new OptionCell[A]
      val firstCell: Cell[Set[Kind], Set[Kind], Set[Kind]] = new SetCell[Kind]
    }

    private object SyntaxCell {
      case class Success[A](value: A, syntax: Syntax[A]) extends SyntaxCell[A] {
        override def init(): Unit = {
          productiveCell(())
          nullableCell(value)
        }
      }
      case class Failure[A](syntax: Syntax[A]) extends SyntaxCell[A] {
        override def init(): Unit = ()
      }
      case class Elem(kind: Kind, syntax: Syntax[Token]) extends SyntaxCell[Token] {
        override def init(): Unit = {
          productiveCell(())
          firstCell(Set(kind))
        }
      }
      case class Disjunction[A](left: SyntaxCell[A], right: SyntaxCell[A], syntax: Syntax[A])
          extends SyntaxCell[A] {
        override def init(): Unit = {

          left.init()
          right.init()

          // Productivity
          left.productiveCell.register(productiveCell)
          right.productiveCell.register(productiveCell)

          // Nullability
          left.nullableCell.register(nullableCell)
          right.nullableCell.register(nullableCell)

          // First
          left.firstCell.register(firstCell)
          right.firstCell.register(firstCell)
        }
      }
      case class Sequence[A, B](left: SyntaxCell[A], right: SyntaxCell[B], syntax: Syntax[A ~ B])
          extends SyntaxCell[A ~ B] {

        override def init(): Unit = {

          left.init()
          right.init()

          // Productivity
          val mergeProductive: Cell[Either[Unit, Unit], Unit, Option[Unit]] =
            new MergeOnceCell[Unit, Unit, Unit]((_: Unit, _: Unit) => ())

          left.productiveCell.register(mergeProductive.contramap(Left(_)))
          right.productiveCell.register(mergeProductive.contramap(Right(_)))
          mergeProductive.register(productiveCell)

          // Nullability
          val mergeNullable: Cell[Either[A, B], (A ~ B), Option[A ~ B]] =
            new MergeOnceCell[A, B, A ~ B]((leftValue: A, rightValue: B) => leftValue ~ rightValue)

          left.nullableCell.register(mergeNullable.contramap(Left(_)))
          right.nullableCell.register(mergeNullable.contramap(Right(_)))
          mergeNullable.register(nullableCell)

          // First
          val firstLeft: Cell[Option[Set[Kind]], Set[Kind], Any] =
            new GatedCell[Set[Kind]]

          left.firstCell.register(firstLeft.contramap(Some(_)))
          right.productiveCell.register(firstLeft.contramap((_: Unit) => None))
          firstLeft.register(firstCell)

          val firstRight: Cell[Option[Set[Kind]], Set[Kind], Any] =
            new GatedCell[Set[Kind]]

          left.nullableCell.register(firstRight.contramap((_: A) => None))
          right.firstCell.register(firstRight.contramap(Some(_)))
          firstRight.register(firstCell)
        }
      }
      case class Transform[A, B](
          inner: SyntaxCell[A],
          function: A => B,
          inverse: B => Seq[A],
          syntax: Syntax[B]) extends SyntaxCell[B] {

        override def init(): Unit = {
          inner.init()

          inner.productiveCell.register(productiveCell)
          inner.nullableCell.register(nullableCell.contramap(function))
          inner.firstCell.register(firstCell)
        }
      }
      abstract class Recursive[A] extends SyntaxCell[A] {
        def inner: SyntaxCell[A]
        val id: RecId

        var inited: Boolean = false

        override def init(): Unit = {
          if (!inited) {
            inited = true

            inner.init()

            inner.productiveCell.register(productiveCell)
            inner.nullableCell.register(nullableCell)
            inner.firstCell.register(firstCell)
          }
        }
      }
      object Recursive {
        def apply[A](cell: => SyntaxCell[A], recId: RecId, syn: Syntax[A]): SyntaxCell[A] =
          new Recursive[A] {
            override val id: RecId = recId
            override lazy val inner: SyntaxCell[A] = cell
            override val syntax: Syntax[A] = syn
          }

        def unapply[A](that: SyntaxCell[A]): Option[(SyntaxCell[A], RecId, Syntax[A])] = {
          if (that.isInstanceOf[Recursive[_]]) {
            val other = that.asInstanceOf[Recursive[A]]
            Some((other.inner, other.id, other.syntax))
          }
          else {
            None
          }
        }
      }
    }

    /** Result of parsing.
      *
      * @group result
      */
    sealed trait ParseResult[A] {

      /** Parser for the rest of input. */
      val rest: Parser[A]

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
    case class Parsed[A](value: A, rest: Parser[A]) extends ParseResult[A]

    /** Indicates that the provided `token` was not expected at that point.
      *
      * The parser at the point of error is returned.
      *
      * @param token The token at fault.
      * @param rest  Parser at the point of error.
      *
      * @group result
      */
    case class UnexpectedToken[A](token: Token, rest: Parser[A]) extends ParseResult[A]

    /** Indicates that end of input was unexpectedly encountered.
      *
      * The `syntax` for subsequent input is provided.
      *
      * @param syntax Syntax at the end of input.
      *
      * @group result
      */
    case class UnexpectedEnd[A](rest: Parser[A]) extends ParseResult[A]

    /** LL(1) parser.
      *
      * @group parsing
      */
    sealed trait Parser[A] {

      /** The value, if any, corresponding to the empty sequence of tokens in `this` parser.
        *
        * @group property
        */
      def nullable: Option[A]

      /** Indicates if the empty sequence is described by `this` parser.
        *
        * @group property
        */
      def isNullable: Boolean = nullable.nonEmpty

      /** Indicates if there exists a finite sequence of tokens that `this` parser describes.
        *
        * @group property
        */
      def isProductive: Boolean = isNullable || first.nonEmpty

      /** Returns the set of token kinds that are accepted as the first token by `this` parser.
        *
        * @group property
        */
      def first: Set[Kind]

      /** Syntax corresponding to this parser.
        *
        * @group property
        */
      def syntax: Syntax[A]

      /** Parses a sequence of tokens.
        *
        * @group parsing
        */
      def apply(tokens: Iterator[Token]): ParseResult[A]
    }

    private sealed trait Tree[A] {
      val nullable: Option[A]
      @inline def isNullable: Boolean = nullable.nonEmpty
      @inline def isProductive: Boolean = isNullable || first.nonEmpty
      val first: HashSet[Kind]
      val syntax: Syntax[A]
    }

    private object Tree {
      sealed abstract case class Success[A](value: A) extends Tree[A]
      sealed abstract case class Failure[A]() extends Tree[A]
      sealed abstract case class Elem(kind: Kind) extends Tree[Token]
      sealed abstract case class Sequence[A, B](left: Tree[A], right: Tree[B]) extends Tree[A ~ B]
      sealed abstract case class Disjunction[A](left: Tree[A], right: Tree[A]) extends Tree[A]
      sealed abstract case class Transform[A, B](inner: Tree[A], function: A => B, inverse: B => Seq[A]) extends Tree[B]

      sealed abstract class Recursive[A] extends Tree[A] {

        /** Inner parser for this recursive parser. */
        def inner: Tree[A]

        /** Unique identifier for this recursive parser. */
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
      }

      object Recursive {
        def unapply[A](that: Tree[A]): Option[(RecId, Tree[A])] = {
          if (that.isInstanceOf[Recursive[_]]) {
            val other = that.asInstanceOf[Recursive[A]]
            Some((other.id, other.inner))
          }
          else {
            None
          }
        }
      }

      def epsilon[A](value: A): Tree[A] = {
        new Success(value) {
          override val nullable: Option[A] = Some(value)
          override val first: HashSet[Kind] = HashSet()
          override val syntax: Syntax[A] = self.epsilon(value)
        }
      }
    }

    private trait Context[A, B] {
      import Context._

      def isEmpty: Boolean = this match {
        case Empty() => true
        case _ => false
      }

      def plugAll(value: A): Iterable[FocusedSyntax[B]] = this match {
        case PrependValue(first, rest) => rest.plugAll(first ~ value)
        case FollowBy(syntax, rest) => List(FocusedPair(syntax, PrependValue(value, rest)))
        case ApplyFunction(function, rest) => rest.plugAll(function(value))
        case Alternatives(rests) => rests.flatMap(_.plugAll(value))
        case _: Empty[t] => List(FocusedPair(Tree.epsilon(value), this))
      }
    }

    private object Context {
      case class PrependValue[A, B, C](value: A, rest: Context[A ~ B, C]) extends Context[B, C]
      case class FollowBy[A, B, C](syntax: Tree[B], rest: Context[A ~ B, C]) extends Context[A, C]
      case class ApplyFunction[A, B, C](function: A => B, rest: Context[B, C]) extends Context[A, C]
      case class Alternatives[A, B](rest: ListBuffer[Context[A, B]]) extends Context[A, B]
      case class Empty[A]() extends Context[A, A]
    }

    private case class State[A](focuseds: Iterable[FocusedSyntax[A]]) extends Parser[A] {

      override def nullable: Option[A] = focuseds.flatMap(_.nullable).headOption

      override def isNullable: Boolean = nullable.nonEmpty

      override def isProductive: Boolean = isNullable || first.nonEmpty

      override def first: Set[Kind] = ???

      override def syntax: Syntax[A] = ???

      override def apply(tokens: Iterator[Token]): ParseResult[A] = {
        var current: State[A] = this

        while (tokens.hasNext) {
          val token = tokens.next()
          val kind = getKind(token)

          val located = current.locateAll(kind)
          val pierced = State(located).pierceAll(kind)
          val plugged = pierced.flatMap(_.plugAll(token))

          if (plugged.isEmpty) {
            return UnexpectedToken(token, current)
          }

          current = State(plugged)
        }

        current.nullable match {
          case None => UnexpectedEnd(current)
          case Some(value) => Parsed(value, current)
        }
      }

      def locateAll(kind: Kind): Iterable[FocusedSyntax[A]] = {

        val res: ListBuffer[FocusedSyntax[A]] = new ListBuffer()

        def locate(focused: FocusedSyntax[A]): Unit = {
          val FocusedPair(syntax, context) = focused

          if (syntax.first.contains(kind)) {
            res += focused
          }

          if (syntax.isNullable && !context.isEmpty) { // TODO: Check first of context also here for performance.
            val nullValue = syntax.nullable.get

            for (focused <- context.plugAll(nullValue)) {
              locate(focused)
            }
          }
        }

        for (focused <- focuseds) {
          locate(focused)
        }

        res
      }

      def pierceAll(kind: Kind): Iterable[Context[Token, A]] = {

        import Tree._
        import Context._

        val res: ListBuffer[Context[Token, A]] = new ListBuffer()
        val cache: HashMap[RecId, ListBuffer[Context[_, A]]] = new HashMap()

        def pierce[B](syntax: Tree[B], context: Context[B, A]): Unit = syntax match {
          case Elem(_) => res += context
          case Disjunction(lhs, rhs) =>
            if (lhs.first.contains(kind)) {
              pierce(lhs, context)

              if (rhs.first.contains(kind)) {
                pierce(rhs, context)
              }
            }
            else {
              pierce(rhs, context)
            }
          case Sequence(lhs, rhs) => {
            if (lhs.first.contains(kind)) {
              pierce(lhs, FollowBy(rhs, context))

              if (lhs.isNullable && rhs.first.contains(kind)) {
                pierce(rhs, PrependValue(lhs.nullable.get, context))
              }
            }
            else {
              pierce(rhs, PrependValue(lhs.nullable.get, context))
            }
          }
          case Transform(inner, function, _) => {
            pierce(inner, ApplyFunction(function, context))
          }
          case Recursive(id, inner) => {
            cache.get(id) match {
              case Some(buffer) => buffer += context
              case None => {
                val buffer: ListBuffer[Context[_, A]] = new ListBuffer()
                buffer += context
                cache(id) = buffer
                pierce(inner, Alternatives(buffer.asInstanceOf[ListBuffer[Context[B, A]]]))
              }
            }
          }
          case _ => ()
        }

        for (focused <- focuseds) {
          val FocusedPair(syntax, context) = focused
          pierce(syntax, context)
        }

        res
      }
    }

    private sealed trait FocusedSyntax[A] {
      def nullable: Iterable[A]
    }
    private case class FocusedPair[A, B](syntax: Tree[A], context: Context[A, B]) extends FocusedSyntax[B] {
      override def nullable: Iterable[B] = syntax.nullable match {
        case None => List()
        case Some(value) =>
          if (context.isEmpty) {
            List(value.asInstanceOf[B])
          } else {
            context.plugAll(value).flatMap(_.nullable)
          }
      }
    }
  }
}