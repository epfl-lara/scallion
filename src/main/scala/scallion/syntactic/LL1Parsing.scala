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

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.collection.mutable.HashMap

import scallion.util.internal.propagators._

/** This trait implements LL(1) parsing with derivatives. */
trait LL1Parsing { self: Syntaxes =>

  /** Factory of LL(1) parsers. */
  object LL1 {

    /** Describes a LL(1) conflict.
      *
      * @group conflict
      */
    sealed trait Conflict {

      import Syntax._

      /** Source of the conflict. */
      val source: Disjunction[_]
    }

    /** Contains the description of the various LL(1) conflicts.
      *
      * @group conflict
      */
    object Conflict {

      import Syntax._

      /** Indicates that both branches of a disjunction are nullable.
        *
        * @param source The source of the conflict.
        */
      case class NullableConflict(source: Disjunction[_]) extends Conflict

      /** Indicates that two branches of a disjunction share some same first token kinds.
        *
        * @param source      The source of the conflict.
        * @param ambiguities The conflicting kinds.
        */
      case class FirstConflict(source: Disjunction[_],
                               ambiguities: Set[Kind]) extends Conflict

      /** Indicates that an ambiguity arises due to a disjunction appearing somewhere in
        * the left-hand side of a sequence, that conflicts with the right-hand side of
        * that sequence.
        *
        * @param source      The source of the conflict.
        * @param root        The sequence in which the conflict occured.
        * @param ambiguities The conflicting kinds.
        */
      case class FollowConflict(source: Disjunction[_],
                                root: Sequence[_, _],
                                ambiguities: Set[Kind]) extends Conflict
    }

    import Conflict._

    /** Follow-last set tagged with its source. */
    private case class ShouldNotFollowEntry(source: Syntax.Disjunction[_], kinds: Set[Kind])

    /** Indicates that a syntax is not LL(1) due to various conflicts.
      *
      * @group conflict
      */
    case class ConflictException(conflicts: Set[Conflict]) extends Exception("Syntax is not LL(1).")

    /** Builds a LL(1) parser from a syntax description.
      *
      * @param syntax The description of the syntax.
      * @group parsing
      */
    def build[A](syntax: Syntax[A]): Either[Set[Conflict], Parser[A]] =
      util.Try(apply(syntax)) match {
        case util.Success(parser) => Right(parser)
        case util.Failure(ConflictException(conflicts)) => Left(conflicts)
        case util.Failure(exception) => throw exception
      }

    /** Builds a LL(1) parser from a syntax description.
      *
      * @param syntax The description of the syntax.
      * @throws ConflictException in case the syntax is not LL(1).
      * @group parsing
      */
    def apply[A](syntax: Syntax[A]): Parser[A] = {

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

      var recChecked: Set[RecId] = Set()
      var conflicts: Set[Conflict] = Set()

      def checkConflicts[A](syntaxCell: SyntaxCell[A]): Unit = syntaxCell match {
        case SyntaxCell.Success(value, _) => ()
        case SyntaxCell.Failure(_) => ()
        case SyntaxCell.Elem(kind, _) => ()
        case SyntaxCell.Disjunction(left, right, syntax) => {
          checkConflicts(left)
          checkConflicts(right)
          if (left.nullableCell.get.nonEmpty && right.nullableCell.get.nonEmpty) {
            conflicts += NullableConflict(syntaxCell.syntax.asInstanceOf[Syntax.Disjunction[_]])
          }
          val intersecting = left.firstCell.get.intersect(right.firstCell.get)
          if (intersecting.nonEmpty) {
            conflicts += FirstConflict(
              syntaxCell.syntax.asInstanceOf[Syntax.Disjunction[_]], intersecting)
          }
        }
        case SyntaxCell.Sequence(left: SyntaxCell[tA], right: SyntaxCell[tB], syntax) => {
          checkConflicts(left)
          checkConflicts(right)
          val firstSet: Set[Kind] = right.firstCell.get
          val snfEntries: Set[ShouldNotFollowEntry] = left.snfCell.get
          for (entry <- snfEntries) {
            val ambiguities = entry.kinds.intersect(firstSet)
            if (ambiguities.nonEmpty) {
              conflicts += FollowConflict(
                entry.source, syntaxCell.syntax.asInstanceOf[Syntax.Sequence[_, _]], ambiguities)
            }
          }
        }
        case SyntaxCell.Transform(inner: SyntaxCell[tA], function, inverse, syntax) => {
          checkConflicts(inner)
        }
        case SyntaxCell.Recursive(recInner, recId, _) => if (!recChecked.contains(recId)) {
          recChecked += recId
          checkConflicts(recInner)
        }
      }

      checkConflicts(syntaxCell)

      if (conflicts.nonEmpty) {
        throw ConflictException(conflicts)
      }

      val recTrees: HashMap[RecId, Any] = new HashMap()

      def buildTree[A](syntaxCell: SyntaxCell[A]): Tree[A] = syntaxCell match {
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
              override lazy val inner: Tree[A] = buildTree(recInner)
              override val nullable: Option[A] = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[A] = syntaxCell.syntax
            }

            recTrees += recId -> rec

            rec
          }
          case Some(rec) => rec.asInstanceOf[Tree[A]]
        }
      }

      Focused(buildTree(syntaxCell), Empty())
    }

    private sealed trait SyntaxCell[A] {
      def init(): Unit

      val syntax: Syntax[A]
      val productiveCell: Cell[Unit, Unit, Boolean] = new BooleanCell
      val nullableCell: Cell[A, A, Option[A]] = new OptionCell[A]
      val firstCell: Cell[Set[Kind], Set[Kind], Set[Kind]] = new SetCell[Kind]
      val snfCell: Cell[Set[ShouldNotFollowEntry],
        Set[ShouldNotFollowEntry], Set[ShouldNotFollowEntry]] = new SetCell[ShouldNotFollowEntry]
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

          // Should not follow
          left.snfCell.register(snfCell)
          right.snfCell.register(snfCell)

          val snfLeft: Cell[Option[Set[ShouldNotFollowEntry]], Set[ShouldNotFollowEntry], Any] =
            new GatedCell[Set[ShouldNotFollowEntry]]

          left.firstCell.register(snfLeft.contramap(ks =>
            Some(Set(ShouldNotFollowEntry(syntax.asInstanceOf[Syntax.Disjunction[_]], ks)))))
          right.nullableCell.register(snfLeft.contramap((_: A) => None))
          snfLeft.register(snfCell)

          val snfRight: Cell[Option[Set[ShouldNotFollowEntry]], Set[ShouldNotFollowEntry], Any] =
            new GatedCell[Set[ShouldNotFollowEntry]]

          left.nullableCell.register(snfRight.contramap((_: A) => None))
          right.firstCell.register(snfRight.contramap(ks =>
            Some(Set(ShouldNotFollowEntry(syntax.asInstanceOf[Syntax.Disjunction[_]], ks)))))
          snfRight.register(snfCell)
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

          // Should not follow
          val snfLeft: Cell[Option[Set[ShouldNotFollowEntry]], Set[ShouldNotFollowEntry], Any] =
            new GatedCell[Set[ShouldNotFollowEntry]]

          left.snfCell.register(snfLeft.contramap(Some(_)))
          right.nullableCell.register(snfLeft.contramap((_: B) => None))
          snfLeft.register(snfCell)

          val snfRight: Cell[Option[Set[ShouldNotFollowEntry]], Set[ShouldNotFollowEntry], Any] =
            new GatedCell[Set[ShouldNotFollowEntry]]

          left.productiveCell.register(snfRight.contramap((_: Unit) => None))
          right.snfCell.register(snfRight.contramap(Some(_)))
          snfRight.register(snfCell)
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
          inner.snfCell.register(snfCell)
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
            inner.snfCell.register(snfCell)
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

    private case class Focused[A, B](tree: Tree[B], context: Context[B, A]) extends Parser[A] {

      override def nullable: Option[A] = {

        @tailrec
        def go[C](focused: Focused[A, C]): Option[A] =
          focused.tree.nullable match {
            case Some(value) => focused.context.empty(value) match {
              case Some(casted) => Some(casted)
              case None => go(focused.context.plug(value))
            }
            case None => None
          }

        go(this)
      }

      override def first: Set[Kind] = {

        @tailrec
        def go[C](context: Context[C, A], acc: Set[Kind]): Set[Kind] = context match {
          case Empty() => acc
          case Layered(layer: Layer[tA, tB], tail) => layer.followTree match {
            case Some(tree) => {
              val unioned = acc union tree.first
              if (tree.isNullable) {
                go(tail, unioned)
              }
              else {
                unioned
              }
            }
            case None => go(tail, acc)
          }
        }

        if (tree.isNullable) {
          go(context, tree.first)
        }
        else {
          tree.first
        }
      }

      override def syntax: Syntax[A] = {

        @tailrec def go[B](context: Context[B, A], syntax: Syntax[B]): Syntax[A] = context match {
          case e: Empty[tA] => syntax
          case Layered(layer, tail) => go(tail, layer(syntax))
        }

        go(context, tree.syntax)
      }

      override def apply(tokens: Iterator[Token]): ParseResult[A] = {
        var current: Focused[A, _] = this
        while (tokens.hasNext) {
          val token = tokens.next()
          val kind: Kind = getKind(token)

          current.locate(kind) match {
            case None => return UnexpectedToken(token, current)
            case Some(focused) => {
              current = focused.pierce(token, kind)
            }
          }
        }

        current.nullable match {
          case Some(value) => Parsed(value, current)
          case None => UnexpectedEnd(current)
        }
      }

      private def locate(kind: Kind): Option[Focused[A, _]] = {
        @tailrec
        def go[B](tree: Tree[B], context: Context[B, A]): Option[Focused[A, _]] = {
          if (tree.first.contains(kind)) Some(Focused(tree, context))
          else if (context.isEmpty) None
          else tree.nullable match {
            case Some(value) => {
              context.plug(value) match {
                case newFocused: Focused[tA, tB] => go(newFocused.tree, newFocused.context)
              }
            }
            case None => None
          }
        }

        go(tree, context)
      }

      private def pierce(token: Token, kind: Kind): Focused[A, _] = {
        tree.pierce(kind, context).plug(token)
      }
    }

    private sealed trait Tree[A] {
      val nullable: Option[A]
      @inline def isNullable: Boolean = nullable.nonEmpty
      @inline def isProductive: Boolean = isNullable || first.nonEmpty
      val first: HashSet[Kind]
      val syntax: Syntax[A]

      def pierce[B](kind: Kind, context: Context[A, B]): Context[Token, B] = {

        @tailrec
        def go[A, B](tree: Tree[A], context: Context[A, B]): Context[Token, B] = tree match {
          case Tree.Elem(_) => context
          case tree: Tree.Sequence[tA, tB] =>
            if (tree.left.first.contains(kind)) {
              go(tree.left, Layer.FollowBy[tA, tB](tree.right) +: context)
            }
            else {
              go(tree.right, Layer.PrependValue[tA, tB](tree.left.nullable.get) +: context)
            }
          case Tree.Disjunction(left, right) =>
            if (left.first.contains(kind)) {
              go(left, context)
            }
            else {
              go(right, context)
            }
          case tree: Tree.Transform[tA, tB] =>
            go(tree.inner, Layer.ApplyFunction[tA, tB](tree.function, tree.inverse) +: context)
          case Tree.Recursive(_, tree) =>
            go(tree, context)
        }


        val rev = pierceCache.get(kind) match {
          case Some(rev) => rev
          case None => {
            val extra = go(this, Empty[A]())
            val rev = extra.reverse
            pierceCache(kind) = rev
            rev
          }
        }

        rev.toContext(context)
      }

      private val pierceCache: HashMap[Kind, RevContext[Token, A]] = new HashMap()
    }

    private object Tree {
      sealed abstract case class Success[A](value: A) extends Tree[A]
      sealed abstract case class Failure[A]() extends Tree[A]
      sealed abstract case class Elem(kind: Kind) extends Tree[Token]
      sealed abstract case class Sequence[A, B](left: Tree[A], right: Tree[B]) extends Tree[A ~ B]
      sealed abstract case class Disjunction[A](left: Tree[A], right: Tree[A]) extends Tree[A]
      sealed abstract case class Transform[A, B](
        inner: Tree[A], function: A => B, inverse: B => Seq[A]) extends Tree[B]

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

    /** Reverse of a context.
      *
      * When a context has a pointer to the first element and the tail,
      * a reverse context has a pointer to the last element and the init.
      */
    private sealed trait RevContext[A, B] {

      def toContext[C](acc: Context[B, C]): Context[A, C] = {

        @tailrec
        def go[E](rev: RevContext[A, E], acc: Context[E, C]): Context[A, C] = rev match {
          case RevEmpty() => acc.asInstanceOf[Context[A, C]]
          case RevLayered(init, last) => go(init, Layered(last, acc))
        }

        go(this, acc)
      }
    }

    /** Empty reversed context. */
    private case class RevEmpty[A]() extends RevContext[A, A]

    /** Layer of extra context at the end of a reversed context. */
    private case class RevLayered[A, B, C](
        init: RevContext[A, B],
        last: Layer[B, C]) extends RevContext[A, C]

    /** Represents a context around a [[Tree]].
      *
      * It denotes a single point within a tree.
      *
      * The context may contain many layers, each
      * successive layer indicating which
      * combinator was used on top.
      *
      * The single focal point is always
      * situated on the left of the tree.
      * The can not be any other tree preceding it,
      * nor any alternatives to it.
      */
    private sealed trait Context[A, B] {

      def empty(value: A): Option[B]

      def plug(value: A): Focused[B, _] = {

        @tailrec
        def go[A, B](context: Context[A, B], value: A): Focused[B, _] =
          context match {
            case Empty() => Focused(Tree.epsilon(value), context)
            case Layered(head, tail) => head(value) match {
              case Left(newValue) => go(tail, newValue)
              case Right(LayeredTree(tree, layer)) => Focused(tree, layer +: tail)
            }
          }

        go(this, value)
      }

      /** Adds an extra layer of context. */
      def +:[C](that: Layer[C, A]): Context[C, B] =
        Layered(that, this)

      /** Returns true if the context is empty, false otherwise. */
      def isEmpty: Boolean

      /** Reverses this context. */
      def reverse: RevContext[A, B] = {

        @tailrec
        def go[E](ctx: Context[E, B], acc: RevContext[A, E]): RevContext[A, B] = ctx match {
          case e: Empty[tA] => acc
          case Layered(head, tail) => go(tail, RevLayered(acc, head))
        }

        go(this, RevEmpty())
      }
    }

    /** Indicates that there are no extra layers of context. */
    private case class Empty[A]() extends Context[A, A] {
      override def empty(value: A): Option[A] = Some(value)
      override def isEmpty = true
    }

    /** Layer of extra context on top on a context. */
    private case class Layered[A, B, C](
        head: Layer[A, B],
        tail: Context[B, C]) extends Context[A, C] {
      override def empty(value: A): Option[C] = None
      override def isEmpty = false
    }

    private case class LayeredTree[A, B](tree: Tree[A], layer: Layer[A, B])

    private sealed trait Layer[A, B] {
      type FollowType
      def apply(value: A): Either[B, LayeredTree[_, B]]
      def apply(syntax: Syntax[A]): Syntax[B]
      def followTree: Option[Tree[FollowType]]
    }

    private object Layer {
      case class ApplyFunction[A, B](function: A => B, inverse: B => Seq[A]) extends Layer[A, B] {
        override type FollowType = Nothing
        override def apply(value: A): Either[B, LayeredTree[_, B]] =
          Left(function(value))
        override def apply(syntax: Syntax[A]): Syntax[B] =
          syntax.map(function, inverse)
        override def followTree: Option[Tree[Nothing]] = None
      }

      case class PrependValue[A, B](first: A) extends Layer[B, A ~ B] {
        override type FollowType = Nothing
        override def apply(second: B): Either[A ~ B, LayeredTree[_, A ~ B]] =
          Left(first ~ second)
        override def apply(syntax: Syntax[B]): Syntax[A ~ B] =
          self.epsilon(first) ~ syntax
        override def followTree: Option[Tree[Nothing]] = None
      }

      case class FollowBy[A, B](second: Tree[B]) extends Layer[A, A ~ B] {
        override type FollowType = B
        override def apply(first: A): Either[A ~ B, LayeredTree[_, A ~ B]] =
          Right(LayeredTree(second, PrependValue(first)))
        override def apply(syntax: Syntax[A]): Syntax[A ~ B] =
          syntax ~ second.syntax
        override def followTree: Option[Tree[B]] = Some(second)
      }
    }
  }
}