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
package gzpwd

import scala.language.implicitConversions

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.collection.mutable.{ HashMap, ListBuffer, PriorityQueue, Queue }

import java.util.WeakHashMap

import scallion.util.internal._

/** This trait implements generalized zippy parsing with derivatives. */
trait Parsing { self: Syntaxes =>

  /** Factory of parsers. */
  object GZPWD {

    type Value[A] = A

    object Value {
      def single[A](value: A): Value[A] = value
      def disjunction[A](left: Value[A], right: Value[A]): Value[A] = left
      def sequence[A, B](left: Value[A], right: Value[B]): Value[A ~ B] = left ~ right
      def transform[A, B](inner: Value[A], function: A => B): Value[B] = function(inner) // inner.map(function)
      def syntax[A](value: Value[A]): Syntax[A] = epsilon(value)
    }

    /** Cache of transformation from syntax to parser. */
    private val syntaxToTreeCache: WeakHashMap[Syntax[_], Tree[_]] = new WeakHashMap()

    /** Builds a parser from a syntax description.
      *
      * @param syntax The description of the syntax.
      * @group parsing
      */
    def apply[A](syntax: Syntax[A]): Parser[A] = {

      // Handles caching.
      if (syntaxToTreeCache.containsKey(syntax)) {
        val tree: Tree[A] = syntaxToTreeCache.get(syntax).asInstanceOf[Tree[A]]
        return State(List(FocusedTree.Entry(tree, Context.Empty[A]())))
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
        case Syntax.Marked(mark, inner) =>
          SyntaxCell.Marked(buildCell(inner), mark, syntax)
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
              override val isNullable: Boolean = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[A] = syntaxCell.syntax
            }
          case SyntaxCell.Failure(_) =>
            new Tree.Failure[A]() {
              override val isNullable: Boolean = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[A] = syntaxCell.syntax
            }
          case SyntaxCell.Elem(kind, _) =>
            new Tree.Elem(kind) {
              override val isNullable: Boolean = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[Token] = syntaxCell.syntax
            }
          case SyntaxCell.Disjunction(left, right, _) =>
            new Tree.Disjunction[A](buildTree(left), buildTree(right)) {
              override val isNullable: Boolean = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[A] = syntaxCell.syntax
            }
          case SyntaxCell.Sequence(left: SyntaxCell[tA], right: SyntaxCell[tB], _) =>
            new Tree.Sequence[tA, tB](buildTree(left), buildTree(right)) {
              override val isNullable: Boolean = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[tA ~ tB] = syntaxCell.syntax
            }
          case SyntaxCell.Marked(inner, mark, _) =>
            new Tree.Marked[A](buildTree(inner), mark) {
              override val isNullable: Boolean = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[A] = syntaxCell.syntax
            }
          case SyntaxCell.Transform(inner: SyntaxCell[tA], function, inverse, _) =>
            new Tree.Transform[tA, A](buildTree(inner), function, inverse) {
              override val isNullable: Boolean = syntaxCell.nullableCell.get
              override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
              override val syntax: Syntax[A] = syntaxCell.syntax
            }
          case SyntaxCell.Recursive(recInner, rId, _) => recTrees.get(rId) match {
            case None => {
              val rec = new Tree.Recursive[A] {
                override val recId = rId
                override lazy val inner: Tree[A] = syntaxToTreeCache.get(recInner.syntax).asInstanceOf[Tree[A]]
                override val isNullable: Boolean = syntaxCell.nullableCell.get
                override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
                override val syntax: Syntax[A] = syntaxCell.syntax
              }

              recTrees += rId -> rec

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

      State(List(FocusedTree.Entry(tree, Context.Empty[A]())))
    }

    private sealed trait SyntaxCell[A] {
      def init(): Unit

      val syntax: Syntax[A]
      val productiveCell: Cell[Unit, Unit, Boolean] = new BooleanCell
      val nullableCell: Cell[Unit, Unit, Boolean] = new BooleanCell
      val firstCell: Cell[Set[Kind], Set[Kind], Set[Kind]] = new SetCell[Kind]
    }

    private object SyntaxCell {
      case class Success[A](value: A, syntax: Syntax[A]) extends SyntaxCell[A] {
        override def init(): Unit = {
          productiveCell(())
          nullableCell(())
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
          val mergeNullable: Cell[Either[Unit, Unit], Unit, Option[Unit]] =
            new MergeOnceCell[Unit, Unit, Unit]((_: Unit, _: Unit) => ())

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

          left.nullableCell.register(firstRight.contramap((_: Unit) => None))
          right.firstCell.register(firstRight.contramap(Some(_)))
          firstRight.register(firstCell)
        }
      }
      case class Marked[A](
          inner: SyntaxCell[A],
          mark: Mark,
          syntax: Syntax[A]) extends SyntaxCell[A] {

        override def init(): Unit = {
          inner.init()

          inner.productiveCell.register(productiveCell)
          inner.nullableCell.register(nullableCell)
          inner.firstCell.register(firstCell)
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
          inner.nullableCell.register(nullableCell)
          inner.firstCell.register(firstCell)
        }
      }
      abstract class Recursive[A] extends SyntaxCell[A] {
        def inner: SyntaxCell[A]
        val recId: RecId

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
        def apply[A](cell: => SyntaxCell[A], rId: RecId, syn: Syntax[A]): SyntaxCell[A] =
          new Recursive[A] {
            override val recId: RecId = rId
            override lazy val inner: SyntaxCell[A] = cell
            override val syntax: Syntax[A] = syn
          }

        def unapply[A](that: SyntaxCell[A]): Option[(SyntaxCell[A], RecId, Syntax[A])] = {
          if (that.isInstanceOf[Recursive[_]]) {
            val other = that.asInstanceOf[Recursive[A]]
            Some((other.inner, other.recId, other.syntax))
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
      def getValue: Option[Value[A]] = this match {
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
    case class Parsed[A](value: Value[A], rest: Parser[A]) extends ParseResult[A]

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

    /** Zippy parsing with derivatives parser.
      *
      * @group parsing
      */
    sealed trait Parser[A] {

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

      /** The value, if any, corresponding to the empty sequence of tokens in `this` parser.
        *
        * @group property
        */
      def nullable: Option[Value[A]]

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

    private sealed trait Tree[A] { tree =>
      final val id: Int = {
        val res = Tree.nextId
        Tree.nextId += 1
        res
      }
      val isNullable: Boolean
      @inline def isProductive: Boolean = isNullable || first.nonEmpty
      val first: HashSet[Kind]
      val syntax: Syntax[A]
      def nullable: Option[Value[A]]
    }

    private object Tree {
      private var nextId: Int = 0

      sealed abstract case class Success[A](value: A) extends Tree[A] {
        override val nullable = Some(Value.single(value))
      }
      sealed abstract case class Failure[A]() extends Tree[A] {
        override val nullable = None
      }
      sealed abstract case class Elem(kind: Kind) extends Tree[Token] {
        override val nullable = None
      }
      sealed abstract case class Sequence[A, B](left: Tree[A], right: Tree[B]) extends Tree[A ~ B] {
        override lazy val nullable = for {
          l <- left.nullable
          r <- right.nullable
        } yield Value.sequence(l, r)
      }
      sealed abstract case class Disjunction[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
        override lazy val nullable = (left.nullable, right.nullable) match {
          case (Some(l), Some(r)) => Some(Value.disjunction(l, r))
          case (Some(l), None) => Some(l)
          case (None, Some(r)) => Some(r)
          case (None, None) => None
        }
      }
      sealed abstract case class Marked[A](inner: Tree[A], mark: Mark) extends Tree[A] {
        override lazy val nullable = inner.nullable
      }
      sealed abstract case class Transform[A, B](inner: Tree[A], function: A => B, inverse: B => Seq[A]) extends Tree[B] {
        override lazy val nullable = inner.nullable.map(v => Value.transform(v, function))
      }

      sealed abstract class Recursive[A] extends Tree[A] {

        override lazy val nullable: Option[Value[A]] = inner.nullable  // TODO: This assumes non-left-recursivity.

        /** Inner parser for this recursive parser. */
        def inner: Tree[A]

        /** Unique identifier for this recursive parser. */
        val recId: RecId

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
            this.recId == that.recId
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
            Some((other.recId, other.inner))
          }
          else {
            None
          }
        }
      }

      def epsilon[A](value: A): Tree[A] = {
        new Success(value) {
          override val isNullable: Boolean = true
          override val first: HashSet[Kind] = HashSet()
          override val syntax: Syntax[A] = self.epsilon(value)
        }
      }
    }

    private trait Context[A, B] {
      import Context._

      val first: Set[Kind]

      def isEmpty: Boolean = this match {
        case Empty() => true
        case _ => false
      }
    }

    private object Context {
      case class PrependBy[A, B, C](value: Value[A], rest: Context[A ~ B, C]) extends Context[B, C] {
        override val first: Set[Kind] = rest.first
      }
      case class FollowBy[A, B, C](syntax: Tree[B], rest: Context[A ~ B, C]) extends Context[A, C] {
        override val first: Set[Kind] =
          if (syntax.isNullable) {
            syntax.first union rest.first
          } else {
            syntax.first
          }
      }
      final class Shared[A, B] private(val id: Int, val rest: Context[A, B]) extends Context[A, B] {
        override def toString: String = "Shared(" + id.toString + ", " + rest.toString + ")"
        override val first: Set[Kind] = rest.first
      }
      object Shared {
        private var nextId: Int = 0

        def create[A, B](rest: Context[A, B]): Shared[A, B] = {
          val id = nextId
          nextId += 1
          new Shared(id, rest)
        }

        def unapply[A, B](context: Context[A, B]): Option[(Int, Context[A, B])] = {
          if (context.isInstanceOf[Shared[_, _]]) {
            val casted = context.asInstanceOf[Shared[A, B]]
            Some((casted.id, casted.rest))
          }
          else {
            None
          }
        }
      }

      case class ApplyMark[A, B](mark: Mark, rest: Context[A, B]) extends Context[A, B] {
        override val first: Set[Kind] = rest.first
      }
      case class ApplyFunction[A, B, C](function: A => B, inverse: B => Seq[A], rest: Context[B, C]) extends Context[A, C] {
        override val first: Set[Kind] = rest.first
      }
      case class Empty[A]() extends Context[A, A] {
        override val first: Set[Kind] = Set.empty[Kind]
      }
      case class Alternatives[A, B](rests: Iterable[Context[A, B]]) extends Context[A, B] {
        override lazy val first = rests.foldLeft(Set.empty[Kind])(_ union _.first)
      }
    }

    private sealed trait FocusedSyntax[B]
    private object FocusedSyntax {
      case class Entry[A, B](value: Syntax[A], context: Context[A, B]) extends FocusedSyntax[B]
    }

    private sealed trait FocusedValue[B]
    private object FocusedValue {
      case class Entry[A, B](value: Value[A], context: Context[A, B]) extends FocusedValue[B]
    }

    private sealed trait FocusedTree[B] {
      def first: Set[Kind]
    }
    private object FocusedTree {
      case class Entry[A, B](tree: Tree[A], context: Context[A, B]) extends FocusedTree[B] {
        override def first: Set[Kind] =
          if (tree.isNullable) {
            tree.first union context.first
          }
          else {
            tree.first
          }
      }
    }

    private case class State[A](focuseds: Iterable[FocusedTree[A]]) extends Parser[A] {

      override def first: Set[Kind] = focuseds.foldLeft(Set.empty[Kind])(_ union _.first)

      override def nullable: Option[Value[A]] = {
        val res: ListBuffer[Value[A]] = new ListBuffer()

        val queue: PriorityQueue[(Int, FocusedValue[A])] = new PriorityQueue()(Ordering.by(_._1))

        val tasks: Queue[FocusedValue[A]] = new Queue()

        import Context._

        @tailrec
        def go[B](value: Value[B], context: Context[B, A]): Unit = context match {
          case Empty() => res += value.asInstanceOf[Value[A]]
          case PrependBy(left, rest) => go(Value.sequence(left, value), rest)
          case FollowBy(right, rest) => if (right.isNullable) go(Value.sequence(value, right.nullable.get), rest)
          case ApplyMark(mark, rest) => go(value, rest)
          case ApplyFunction(function, inverse, rest) => go(Value.transform(value, function), rest)
          case Shared(id, rest) => queue.enqueue((id, FocusedValue.Entry(value, rest)))
          case Alternatives(rests) =>
            if (rests.size == 1) {
              go(value, rests.head)
            }
            else {
              tasks.enqueue(rests.toSeq.map(FocusedValue.Entry(value, _)): _*)
            }
        }

        for (focused@FocusedTree.Entry(tree, context) <- focuseds) {
          if (tree.isNullable) {
            go(tree.nullable.get, context)
          }
        }

        while (tasks.nonEmpty || queue.nonEmpty) {
          while (tasks.nonEmpty) {
            val FocusedValue.Entry(value, context) = tasks.dequeue()
            go(value, context)
          }
          while (queue.nonEmpty) {
            queue.dequeue() match {
              case (id, FocusedValue.Entry(value: Value[tA], context)) => {

                var allValues = value
                var stop = false

                while (!stop && queue.nonEmpty) {
                  val (nextId, focused@FocusedValue.Entry(nextValue, _)) = queue.dequeue()

                  if (nextId == id) {
                    allValues = Value.disjunction(allValues, nextValue.asInstanceOf[Value[tA]])
                  }
                  else {
                    queue.enqueue((nextId, focused))
                    stop = true
                  }
                }

                go(allValues, context)
              }
            }
          }
        }

        if (res.isEmpty) {
          None
        }
        else {
          Some(res.reduceLeft(Value.disjunction(_, _)))
        }
      }

      override def syntax: Syntax[A] = {
        val res: ListBuffer[Syntax[A]] = new ListBuffer()

        val queue: PriorityQueue[(Int, FocusedSyntax[A])] = new PriorityQueue()(Ordering.by(_._1))

        val tasks: Queue[FocusedSyntax[A]] = new Queue()

        import Context._

        @tailrec
        def go[B](value: Syntax[B], context: Context[B, A]): Unit = context match {
          case Empty() => res += value.asInstanceOf[Syntax[A]]
          case PrependBy(left, rest) => go(Value.syntax(left) ~ value, rest)
          case FollowBy(right, rest) => go(value ~ right.syntax, rest)
          case ApplyMark(mark, rest) => go(value, rest)
          case ApplyFunction(function, inverse, rest) => go(value.map(function, inverse), rest)
          case Shared(id, rest) => queue.enqueue((id, FocusedSyntax.Entry(value, rest)))
          case Alternatives(rests) =>
            if (rests.size == 1) {
              go(value, rests.head)
            }
            else {
              tasks.enqueue(rests.toSeq.map((ctx: Context[B, A]) => FocusedSyntax.Entry[B, A](value, ctx)): _*)
            }
        }

        for (focused@FocusedTree.Entry(tree, context) <- focuseds) {
          go(tree.syntax, context)
        }

        while (tasks.nonEmpty || queue.nonEmpty) {
          while (tasks.nonEmpty) {
            val FocusedSyntax.Entry(value, context) = tasks.dequeue()
            go(value, context)
          }
          while (queue.nonEmpty) {
            queue.dequeue() match {
              case (id, FocusedSyntax.Entry(value: Syntax[tA], context)) => {

                var allValues = value
                var stop = false

                while (!stop && queue.nonEmpty) {
                  val (nextId, focused@FocusedSyntax.Entry(nextValue, _)) = queue.dequeue()

                  if (nextId == id) {
                    allValues = allValues | nextValue.asInstanceOf[Syntax[tA]]
                  }
                  else {
                    queue.enqueue((nextId, focused))
                    stop = true
                  }
                }

                go(allValues, context)
              }
            }
          }
        }

        oneOf(res : _*)
      }

      override def apply(tokens: Iterator[Token]): ParseResult[A] = {

        def locateAll(focuseds: Iterable[FocusedTree[A]], kind: Kind): Iterable[FocusedTree[A]] = {

          val resTrees: ListBuffer[Tree[Any]] = new ListBuffer()

          val resContexts: HashMap[Int, ListBuffer[Context[Any, A]]] = new HashMap()

          val queue: PriorityQueue[(Int, FocusedValue[A])] = new PriorityQueue()(Ordering.by(_._1))

          val tasks: Queue[FocusedValue[A]] = new Queue()

          import Context._

          @tailrec
          def locate[B](value: Value[B], context: Context[B, A]): Unit = context match {
            case Empty() => ()
            case PrependBy(left, rest) => locate(Value.sequence(left, value), rest)
            case FollowBy(right, rest) => {
              val matched = right.first.contains(kind)

              if (matched) {
                val buffer = resContexts.getOrElse(right.id, {
                  val buffer = new ListBuffer[Context[Any, A]]()
                  resContexts(right.id) = buffer
                  resTrees += right.asInstanceOf[Tree[Any]]
                  buffer
                })
                buffer += PrependBy(value, rest).asInstanceOf[Context[Any, A]]
              }

              if (right.isNullable && (!matched || rest.first.contains(kind))) {
                locate(Value.sequence(value, right.nullable.get), rest)
              }
            }
            case ApplyMark(mark, rest) => locate(value, rest)
            case ApplyFunction(function, inverse, rest) => locate(Value.transform(value, function), rest)
            case Shared(id, rest) => queue.enqueue((id, FocusedValue.Entry(value, rest)))
            case Alternatives(rests) =>
              if (rests.size == 1) {
                locate(value, rests.head)
              }
              else {
                tasks.enqueue(rests.toSeq.map(FocusedValue.Entry(value, _)): _*)
              }
          }

          for (FocusedTree.Entry(tree, context) <- focuseds) {
            if (tree.first.contains(kind)) {
              val buffer = resContexts.getOrElse(tree.id, {
                val buffer = new ListBuffer[Context[Any, A]]()
                resContexts(tree.id) = buffer
                resTrees += tree.asInstanceOf[Tree[Any]]
                buffer
              })
              buffer += context.asInstanceOf[Context[Any, A]]
            }

            if (tree.isNullable && context.first.contains(kind)) {
              locate(tree.nullable.get, context)
            }
          }

          while (tasks.nonEmpty || queue.nonEmpty) {
            while (tasks.nonEmpty) {
              val FocusedValue.Entry(value, context) = tasks.dequeue()
              locate(value, context)
            }
            while (queue.nonEmpty) {
              queue.dequeue() match {
                case (id, FocusedValue.Entry(value: Value[tA], context)) => {

                  var allValues = value
                  var stop = false

                  while (!stop && queue.nonEmpty) {
                    val (nextId, focused@FocusedValue.Entry(nextValue, _)) = queue.dequeue()

                    if (nextId == id) {
                      allValues = Value.disjunction(allValues, nextValue.asInstanceOf[Value[tA]])
                    }
                    else {
                      queue.enqueue((nextId, focused))
                      stop = true
                    }
                  }

                  locate(allValues, context)
                }
              }
            }
          }

          resTrees.map { (t: Tree[Any]) =>
            val cs = resContexts(t.id)
            FocusedTree.Entry(t, if (cs.size == 1) cs.head else Alternatives(cs))
          }
        }

        def pierceAll(focuseds: Iterable[FocusedTree[A]], kind: Kind): Iterable[Context[Token, A]] = {

          import Tree._
          import Context._

          val res: ListBuffer[Context[Token, A]] = new ListBuffer()
          val cache: HashMap[RecId, ListBuffer[Context[Any, A]]] = new HashMap()

          def pierce[B](syntax: Tree[B], context: Context[B, A]): Unit = syntax match {
            case Elem(_) => res += context
            case Disjunction(lhs, rhs) => {
              val inLeft = lhs.first.contains(kind)
              val inRight = !inLeft || rhs.first.contains(kind)

              val shared = if (inLeft && inRight) {
                Shared.create(context)
              }
              else {
                context
              }

              if (inLeft) {
                pierce(lhs, shared)
              }
              if (inRight) {
                pierce(rhs, shared)
              }
            }
            case Sequence(lhs, rhs) => {
              val inLeft = lhs.first.contains(kind)
              val inRight = !inLeft || (lhs.isNullable && rhs.first.contains(kind))

              val shared = if (inLeft && inRight) {
                Shared.create(context)
              }
              else {
                context
              }

              if (inLeft) {
                pierce(lhs, FollowBy(rhs, shared))
              }
              if (inRight) {
                pierce(rhs, PrependBy(lhs.nullable.get, shared))
              }
            }
            case Marked(inner, mark) => {
              pierce(inner, ApplyMark(mark, context))
            }
            case Transform(inner, function, inverse) => {
              pierce(inner, ApplyFunction(function, inverse, context))
            }
            case Recursive(recId, inner) => {
              cache.get(recId) match {
                case None => {
                  val alternatives = new ListBuffer[Context[B, A]]()
                  alternatives += context
                  cache(recId) = alternatives.asInstanceOf[ListBuffer[Context[Any, A]]]
                  pierce(inner, Alternatives(alternatives))
                }
                case Some(alternatives) => alternatives += context.asInstanceOf[Context[Any, A]]
              }
            }
            case _ => ()
          }

          for (FocusedTree.Entry(syntax, context) <- focuseds) {
            pierce(syntax, context)
          }

          res
        }


        var current: Iterable[FocusedTree[A]] = focuseds

        while (tokens.hasNext) {

          val token = tokens.next()
          val kind = getKind(token)

          val located = locateAll(current, kind)
          val pierced = pierceAll(located, kind)
          val plugged = pierced.map(FocusedTree.Entry(Tree.epsilon(token), _))

          if (plugged.isEmpty) {
            return UnexpectedToken(token, State(current))
          }

          current = plugged
        }

        val endState = State(current)
        endState.nullable match {
          case None => UnexpectedEnd(endState)
          case Some(value) => Parsed(value, endState)
        }
      }
    }
  }
}