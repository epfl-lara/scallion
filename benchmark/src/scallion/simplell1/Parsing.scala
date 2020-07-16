package scallion
package simplell1

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.collection.mutable.HashMap

import scallion.util.internal._

trait Parsing { self: Syntaxes =>

  object SimpleLL1 {

    /** Builds a simple LL(1) parser from a syntax description. */
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
        case SyntaxCell.Marked(inner, mark, _) =>
          new Tree.Marked[A](buildTree(inner), mark) {
            override val nullable: Option[A] = syntaxCell.nullableCell.get
            override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
            override val syntax: Syntax[A] = syntaxCell.syntax
          }
        case SyntaxCell.Transform(inner: SyntaxCell[tA], function, inverse, _) =>
          new Tree.Transform[tA, A](buildTree(inner), function, inverse) {
            override val nullable: Option[A] = syntaxCell.nullableCell.get
            override val first: HashSet[Kind] = HashSet(syntaxCell.firstCell.get.toSeq: _*)
            override val syntax: Syntax[A] = syntaxCell.syntax
          }
        case SyntaxCell.Recursive(recInner, recId, _) => recTrees.get(recId) match {
          case None => {
            val rec: Tree[A] = new Tree.Recursive[A] {
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

      buildTree(syntaxCell)
    }

    private sealed trait SyntaxCell[A] {
      def init(): Unit

      val syntax: Syntax[A]
      val productiveCell: Cell[Unit, Unit, Boolean] = new BooleanCell
      val nullableCell: Cell[A, A, Option[A]] = new OptionCell[A]
      val firstCell: Cell[Set[Kind], Set[Kind], Set[Kind]] = new SetCell[Kind]
      val snfCell: Cell[Set[Kind], Set[Kind], Set[Kind]] = new SetCell[Kind]
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

          val snfLeft: Cell[Option[Set[Kind]], Set[Kind], Any] =
            new GatedCell[Set[Kind]]

          left.firstCell.register(snfLeft.contramap(Some(_)))
          right.nullableCell.register(snfLeft.contramap((_: A) => None))
          snfLeft.register(snfCell)

          val snfRight: Cell[Option[Set[Kind]], Set[Kind], Any] =
            new GatedCell[Set[Kind]]

          left.nullableCell.register(snfRight.contramap((_: A) => None))
          right.firstCell.register(snfRight.contramap(Some(_)))
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
          val snfLeft: Cell[Option[Set[Kind]], Set[Kind], Any] =
            new GatedCell[Set[Kind]]

          left.snfCell.register(snfLeft.contramap(Some(_)))
          right.nullableCell.register(snfLeft.contramap((_: B) => None))
          snfLeft.register(snfCell)

          val snfRight: Cell[Option[Set[Kind]], Set[Kind], Any] =
            new GatedCell[Set[Kind]]

          left.productiveCell.register(snfRight.contramap((_: Unit) => None))
          right.snfCell.register(snfRight.contramap(Some(_)))
          snfRight.register(snfCell)
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
          inner.snfCell.register(snfCell)
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
    sealed trait Parser[A] { self =>

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

    private sealed trait Tree[A] extends Parser[A] {
      override val nullable: Option[A]
      override val first: HashSet[Kind]
      override val syntax: Syntax[A]

      def derive(token: Token, kind: Kind): Tree[A]

      override def apply(tokens: Iterator[Token]): ParseResult[A] = {

        var current: Tree[A] = this

        while (tokens.hasNext) {
          val token = tokens.next()
          val kind = getKind(token)

          if (!current.first.contains(kind)) {
            return UnexpectedToken(token, current)
          }
          current = current.derive(token, kind)
        }

        current.nullable match {
          case None => UnexpectedEnd(current)
          case Some(value) => Parsed(value, current)
        }
      }
    }

    private object Tree {
      sealed abstract case class Success[A](value: A) extends Tree[A] {
        override def derive(token: Token, kind: Kind): Tree[A] =
          Tree.failure
      }
      sealed abstract case class Failure[A]() extends Tree[A] {
        override def derive(token: Token, kind: Kind): Tree[A] =
          Tree.failure
      }
      sealed abstract case class Elem(kind: Kind) extends Tree[Token] {
        override def derive(token: Token, other: Kind): Tree[Token] =
          if (kind == other) Tree.epsilon(token) else Tree.failure
      }
      sealed abstract case class Sequence[A, B](left: Tree[A], right: Tree[B]) extends Tree[A ~ B] {
        override def derive(token: Token, kind: Kind): Tree[A ~ B] =
          left.nullable match {
            case Some(value) if right.first.contains(kind) => Tree.sequence(left, right.derive(token, kind))
            case _ => Tree.sequence(left.derive(token, kind), right)
          }
      }
      sealed abstract case class Disjunction[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
        override def derive(token: Token, kind: Kind): Tree[A] =
          if (left.first.contains(kind)) left.derive(token, kind) else right.derive(token, kind)
      }
      sealed abstract case class Marked[A](inner: Tree[A], mark: Mark) extends Tree[A] {
        override def derive(token: Token, kind: Kind): Tree[A] =
          Tree.marked(inner.derive(token, kind), mark)
      }
      sealed abstract case class Transform[A, B](
          inner: Tree[A], function: A => B, inverse: B => Seq[A]) extends Tree[B] {
        override def derive(token: Token, kind: Kind): Tree[B] =
          Tree.transform(inner.derive(token, kind), function, inverse)
      }
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

        override def derive(token: Token, kind: Kind): Tree[A] =
          inner.derive(token, kind)
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

      def failure[A]: Tree[A] = {
        new Failure[A]() {
          override val nullable: Option[A] = None
          override val first: HashSet[Kind] = HashSet()
          override val syntax: Syntax[A] = self.failure
        }
      }

      def sequence[A, B](left: Tree[A], right: Tree[B]): Tree[A ~ B] = {
        new Sequence[A, B](left, right) {
          override val nullable: Option[A ~ B] = for {
            l <- left.nullable
            r <- right.nullable
          } yield l ~ r
          override val first: HashSet[Kind] = left.first union right.first
          override val syntax: Syntax[A ~ B] = left.syntax ~ right.syntax
        }
      }

      def marked[A](inner: Tree[A], mark: Mark): Tree[A] = {
        new Marked(inner, mark) {
          override val nullable: Option[A] = inner.nullable
          override val first: HashSet[Kind] = inner.first
          override val syntax: Syntax[A] = inner.syntax.mark(mark)
        }
      }

      def transform[A, B](inner: Tree[A], function: A => B, inverse: B => Seq[A]): Tree[B] = {
        new Transform(inner, function, inverse) {
          override val nullable: Option[B] = inner.nullable.map(function)
          override val first: HashSet[Kind] = inner.first
          override val syntax: Syntax[B] = inner.syntax.map(function, inverse)
        }
      }
    }
  }
}