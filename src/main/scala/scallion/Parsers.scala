package scallion

case class ~[+A, +B](_1: A, _2: B)

trait Parsers[Token, Kind] {

  import Parser._

  def getKind(token: Token): Kind

  sealed abstract class Parser[+A] {

    def nullable: Option[A]
    def isProductive: Boolean

    @inline def first: Set[Kind] = collectFirst(Set())
    @inline def shouldNotFollow: Set[Kind] = collectShouldNotFollow(Set())
    @inline def calledLeft(id: AnyRef): Boolean = collectCalledLeft(id, Set())
    @inline def isLL1: Boolean = collectIsLL1(Set())

    def collectNullable(recs: Set[AnyRef]): Option[A]
    def collectFirst(recs: Set[AnyRef]): Set[Kind]
    def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind]
    def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean
    def collectIsProductive(recs: Set[AnyRef]): Boolean
    def collectIsLL1(recs: Set[AnyRef]): Boolean

    def derive(token: Token): Parser[A]

    def map[B](function: A => B): Parser[B] = this match {
      case Failure => Failure
      case Success(value) => Success(function(value))
      case Transform(other, inner) => Transform(other andThen function, inner)
      case _ => Transform(function, this)
    }

    def prepend[B](prefix: Seq[B])(implicit ev1: Parser[A] <:< Parser[Seq[B]], ev2: A <:< Seq[B]): Parser[Seq[B]] = ev1(this) match {
      case Failure => Failure
      case Success(value) => Success(prefix ++ value)
      case Prepend(rest, inner) => Prepend(prefix ++ rest, inner)
      case _ => Prepend(prefix, this)
    }

    def merge[B](that: Parser[B]): Parser[(A, B)] = (this, that) match {
      case (Failure, _) => Failure
      case (_, Failure) => Failure
      case (Success(a), Success(b)) => Success((a, b))
      case _ => Sequence(this, that)
    }

    def ++[B](that: Parser[Seq[B]])(implicit ev1: Parser[A] <:< Parser[Seq[B]], ev2: A <:< Seq[B]): Parser[Seq[B]] = (this, that) match {
      case (Failure, _) => Failure
      case (_, Failure) => Failure
      case (Success(a), Success(b)) => Success(a ++ b)
      //case (_, Concat(left, right)) => (this ++ left) ++ right  // Left is where values hide.
      case _ => Concat(this, that)
    }

    def ~>~[B](that: Parser[B]): Parser[B] = this.merge(that).map(_._2)

    def ~<~[B](that: Parser[B]): Parser[A] = this.merge(that).map(_._1)

    def :+[B](that: Parser[B])(implicit ev1: Parser[A] <:< Parser[Seq[B]], ev2: A <:< Seq[B]): Parser[Seq[B]] =
      this ++ that.map(Vector(_))

    def +:[B](that: Parser[B])(implicit ev1: Parser[A] <:< Parser[Seq[B]], ev2: A <:< Seq[B]): Parser[Seq[B]] =
      that.map(Vector(_)) ++ this

    def ~[B](that: Parser[B]): Parser[A ~ B] =
      this.merge(that).map {
        case (lhs, rhs) => scallion.~(lhs, rhs)
      }

    def |[B >: A](that: Parser[B]): Parser[B] = (this, that) match {
      case (Failure, _) => that
      case (_, Failure) => this
      case _ => Disjunction(this, that)
    }

    def parse(it: Iterator[Token]): Option[A] = {
      require(isLL1)

      var p: Parser[A] = this
      while (it.hasNext && p != Failure /*p.isProductive*/) {
        p = p.derive(it.next())
      }
      p.nullable
    }
  }
    object Parser {
    case class Success[A](value: A) extends Parser[A] {
      override val nullable: Option[A] = Some(value)
      override val isProductive: Boolean = true
      override def collectNullable(recs: Set[AnyRef]): Option[A] = Some(value)
      override def collectFirst(recs: Set[AnyRef]): Set[Kind] = Set()
      override def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] = Set()
      override def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean = false
      override def collectIsLL1(recs: Set[AnyRef]): Boolean = true
      override def collectIsProductive(recs: Set[AnyRef]): Boolean = true
      override def derive(token: Token): Parser[A] = Failure
    }
    case object Failure extends Parser[Nothing] {
      override val nullable: Option[Nothing] = None
      override val isProductive: Boolean = false
      override def collectNullable(recs: Set[AnyRef]): Option[Nothing] = None
      override def collectFirst(recs: Set[AnyRef]): Set[Kind] = Set()
      override def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] = Set()
      override def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean = false
      override def collectIsLL1(recs: Set[AnyRef]): Boolean = true
      override def collectIsProductive(recs: Set[AnyRef]): Boolean = false
      override def derive(token: Token): Parser[Nothing] = Failure
    }
    case class Elem(kind: Kind) extends Parser[Token] {
      override val nullable: Option[Token] = None
      override val isProductive: Boolean = true
      override def collectNullable(recs: Set[AnyRef]): Option[Token] = None
      override def collectFirst(recs: Set[AnyRef]): Set[Kind] = Set(kind)
      override def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] = Set()
      override def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean = false
      override def collectIsLL1(recs: Set[AnyRef]): Boolean = true
      override def collectIsProductive(recs: Set[AnyRef]): Boolean = true
      override def derive(token: Token): Parser[Token] = if (getKind(token) == kind) Success(token) else Failure
    }
    case class Transform[A, B](function: A => B, inner: Parser[A]) extends Parser[B] {
      override lazy val nullable: Option[B] = inner.nullable.map(function)
      override lazy val isProductive: Boolean = inner.isProductive
      override def collectNullable(recs: Set[AnyRef]): Option[B] = inner.collectNullable(recs).map(function)
      override def collectFirst(recs: Set[AnyRef]): Set[Kind] = inner.collectFirst(recs)
      override def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] = inner.collectShouldNotFollow(recs)
      override def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean = false
      override def collectIsLL1(recs: Set[AnyRef]): Boolean = inner.collectIsLL1(recs)
      override def collectIsProductive(recs: Set[AnyRef]): Boolean = inner.collectIsProductive(recs)
      override def derive(token: Token): Parser[B] = inner.derive(token).map(function)
    }
    case class Prepend[A](prefix: Seq[A], inner: Parser[Seq[A]]) extends Parser[Seq[A]] {
      override lazy val nullable: Option[Seq[A]] = inner.nullable.map(prefix ++ _)
      override lazy val isProductive: Boolean = inner.isProductive
      override def collectNullable(recs: Set[AnyRef]): Option[Seq[A]] = inner.collectNullable(recs).map(prefix ++ _)
      override def collectFirst(recs: Set[AnyRef]): Set[Kind] = inner.collectFirst(recs)
      override def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] = inner.collectShouldNotFollow(recs)
      override def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean = false
      override def collectIsLL1(recs: Set[AnyRef]): Boolean = inner.collectIsLL1(recs)
      override def collectIsProductive(recs: Set[AnyRef]): Boolean = inner.collectIsProductive(recs)
      override def derive(token: Token): Parser[Seq[A]] = inner.derive(token).prepend(prefix)
    }
    case class Sequence[A, B](left: Parser[A], right: Parser[B]) extends Parser[(A, B)] {
      override lazy val nullable: Option[(A, B)] = for {
        leftValue <- left.nullable
        rightValue <- right.nullable
      } yield (leftValue, rightValue)
      override lazy val isProductive: Boolean = left.isProductive && right.isProductive
      override def collectNullable(recs: Set[AnyRef]): Option[(A, B)] = for {
        leftValue <- left.collectNullable(recs)
        rightValue <- right.collectNullable(recs)
      } yield (leftValue, rightValue)
      override def collectFirst(recs: Set[AnyRef]): Set[Kind] = left.nullable match {
        case Some(_) => left.collectFirst(recs) ++ right.collectFirst(recs)
        case None => left.collectFirst(recs)
      }
      override def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] = right.nullable match {
        case Some(_) => right.collectShouldNotFollow(recs) ++ left.collectShouldNotFollow(recs)
        case None => right.collectShouldNotFollow(recs)
      }
      override def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean = {
        left.collectCalledLeft(id, recs) || (left.nullable.nonEmpty && right.collectCalledLeft(id, recs))
      }
      override def collectIsLL1(recs: Set[AnyRef]): Boolean = {
        left.collectIsLL1(recs) && right.collectIsLL1(recs) &&
        (if (left.nullable.nonEmpty) (left.shouldNotFollow & right.first).isEmpty else true)
      }
      override def collectIsProductive(recs: Set[AnyRef]): Boolean =
        left.collectIsProductive(recs) && right.collectIsProductive(recs)
      override def derive(token: Token): Parser[(A, B)] = {
        val derived = left.derive(token)

        if (!derived.isProductive) {
          left.nullable match {
            case Some(leftValue) => right.derive(token).map(rightValue => (leftValue, rightValue))
            case None => Failure
          }
        }
        else {
          derived.merge(right)
        }
      }
    }
    case class Concat[A, B](left: Parser[Seq[A]], right: Parser[Seq[A]]) extends Parser[Seq[A]] {
      override lazy val nullable: Option[Seq[A]] = for {
        leftValue <- left.nullable
        rightValue <- right.nullable
      } yield leftValue ++ rightValue
      override lazy val isProductive: Boolean = left.isProductive && right.isProductive
      override def collectNullable(recs: Set[AnyRef]): Option[Seq[A]] = for {
        leftValue <- left.collectNullable(recs)
        rightValue <- right.collectNullable(recs)
      } yield leftValue ++ rightValue
      override def collectFirst(recs: Set[AnyRef]): Set[Kind] = left.nullable match {
        case Some(_) => left.collectFirst(recs) ++ right.collectFirst(recs)
        case None => left.collectFirst(recs)
      }
      override def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] = right.nullable match {
        case Some(_) => right.collectShouldNotFollow(recs) ++ left.collectShouldNotFollow(recs)
        case None => right.collectShouldNotFollow(recs)
      }
      override def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean = {
        left.collectCalledLeft(id, recs) || (left.nullable.nonEmpty && right.collectCalledLeft(id, recs))
      }
      override def collectIsLL1(recs: Set[AnyRef]): Boolean = {
        left.collectIsLL1(recs) && right.collectIsLL1(recs) &&
        (if (left.nullable.nonEmpty) (left.shouldNotFollow & right.first).isEmpty else true)
      }
      override def collectIsProductive(recs: Set[AnyRef]): Boolean =
        left.collectIsProductive(recs) && right.collectIsProductive(recs)
      override def derive(token: Token): Parser[Seq[A]] = {
        val derived = left.derive(token)

        if (!derived.isProductive) {
          left.nullable match {
            case Some(leftValue) => right.derive(token).prepend(leftValue)
            case None => Failure
          }
        }
        else {
          derived ++ right
        }
      }
    }
    case class Disjunction[A](left: Parser[A], right: Parser[A]) extends Parser[A] {
      override lazy val nullable: Option[A] = left.nullable orElse right.nullable
      override lazy val isProductive: Boolean = left.isProductive || right.isProductive
      override def collectNullable(recs: Set[AnyRef]): Option[A] =
        left.collectNullable(recs) orElse right.collectNullable(recs)
      override def collectFirst(recs: Set[AnyRef]): Set[Kind] =
        left.collectFirst(recs) ++ right.collectFirst(recs)
      override def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] = {
        val fromLeft: Set[Kind] = if (right.nullable.nonEmpty) left.first else Set()
        val fromRight: Set[Kind] = if (left.nullable.nonEmpty) right.first else Set()

        fromRight ++ fromLeft ++ left.collectShouldNotFollow(recs) ++ right.collectShouldNotFollow(recs)
      }
      override def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean =
        left.collectCalledLeft(id, recs) || right.collectCalledLeft(id, recs)
      override def collectIsLL1(recs: Set[AnyRef]): Boolean =
        left.collectIsLL1(recs) && right.collectIsLL1(recs) &&
        (left.nullable.isEmpty || right.nullable.isEmpty) &&
        (left.first & right.first).isEmpty
      override def collectIsProductive(recs: Set[AnyRef]): Boolean =
        left.collectIsProductive(recs) || right.collectIsProductive(recs)
      override def derive(token: Token): Parser[A] = {
        val derived = left.derive(token)

        if (derived.isProductive) {
          derived
        }
        else {
          right.derive(token)
        }
      }
    }
    case class Lazy[A](computation: () => Parser[A]) extends Parser[A] {
      lazy val inner: Parser[A] = computation()

      override lazy val nullable: Option[A] = inner.collectNullable(Set(this))
      override lazy val isProductive: Boolean = inner.collectIsProductive(Set(this))

      override def collectNullable(recs: Set[AnyRef]): Option[A] =
        if (recs.contains(this)) None else inner.collectNullable(recs + this)
      override def collectFirst(recs: Set[AnyRef]): Set[Kind] =
        if (recs.contains(this)) Set() else inner.collectFirst(recs + this)
      override def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] =
        if (recs.contains(this)) Set() else inner.collectShouldNotFollow(recs + this)
      override def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean =
        if (recs.contains(this)) false else (this eq id) || inner.collectCalledLeft(id, recs + this)
      override def collectIsLL1(recs: Set[AnyRef]): Boolean =
        if (recs.contains(this)) true else !inner.calledLeft(this) && inner.collectIsLL1(recs + this)
      override def collectIsProductive(recs: Set[AnyRef]): Boolean =
        if (recs.contains(this)) false else inner.collectIsProductive(recs + this)
      override def derive(token: Token): Parser[A] =
        inner.derive(token)
    }
  }

  def elem(kind: Kind): Parser[Token] = Elem(kind)
  def accept[A](kind: Kind)(function: PartialFunction[Token, A]): Parser[A] = elem(kind).map(function)
  def recursive[A](parser: => Parser[A]): Parser[A] = Lazy(() => parser)
  def epsilon[A](value: A): Parser[A] = Success(value)
  def failure[A]: Parser[A] = Failure
  def repsep[A](rep: Parser[A], sep: Parser[Any]): Parser[Seq[A]] = {
    lazy val rest: Parser[Seq[A]] = recursive((sep ~>~ rep) +: rest | epsilon(Vector()))

    rep +: rest | epsilon(Vector())
  }
}
