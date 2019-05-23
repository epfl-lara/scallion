package scallion

case class ~[+A, +B](_1: A, _2: B)

trait Parsers[Token, Kind] {

  import Parser._

  def getKind(token: Token): Kind

  sealed trait Parser[+A] {

    /** The value, if any, produced by this parser without consuming more input. */
    def nullable: Option[A]

    /** Indicates if there exists a sequence of tokens that the parser can accept. */
    def isProductive: Boolean

    /** Returns the set of tokens that are accepted as the next token. */
    @inline def first: Set[Kind] = collectFirst(Set())

    /** Returns the set of tokens that should not be accept
      * as the next token by a subsequent parser. */
    @inline def shouldNotFollow: Set[Kind] = collectShouldNotFollow(Set())

    /** Checks if a `Recursive` parser can be entered without consuming input first.
      *
      * @param id The reference of the `Recursive` parser.
      */
    @inline def calledLeft(id: AnyRef): Boolean = collectCalledLeft(id, Set())

    /** Checks if this parser corresponds to a LL(1) grammar. */
    @inline def isLL1: Boolean = collectIsLL1(Set())

    /** Returns all LL(1) conflicts in the parser. */
    @inline def conflicts: Set[LL1Conflict] = collectLL1Conflicts(Set())

    // All the functions below have an argument `recs` which
    // contains the set of all `Recursive` parser on which the call
    // was already performed.
    //
    // This is done to handle the potentially cyclic structure of parsers
    // introduced by `Recursive`.

    def collectNullable(recs: Set[AnyRef]): Option[A]
    def collectFirst(recs: Set[AnyRef]): Set[Kind]
    def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind]
    def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean
    def collectIsProductive(recs: Set[AnyRef]): Boolean
    def collectIsLL1(recs: Set[AnyRef]): Boolean
    def collectLL1Conflicts(recs: Set[AnyRef]): Set[LL1Conflict]

    /** Feeds a token to the parser and obtain a parser for the rest of input. */
    def derive(token: Token): Parser[A]

    override def toString = repr(0, Map.empty)

    def repr(level: Int, recs: Map[AnyRef, String]): String

    /** Apply a function to the parsed values. */
    def map[B](function: A => B): Parser[B] = this match {
      case Failure => Failure
      case Success(value) => Success(function(value))
      case Transform(other, inner) => Transform(other andThen function, inner)
      case _ => Transform(function, this)
    }

    /** Sequences `this` and `that` parser. The parsed values are concatenated. */
    def ++[B](that: Parser[Seq[B]])(implicit ev1: Parser[A] <:< Parser[Seq[B]], ev2: A <:< Seq[B]): Parser[Seq[B]] = (this, that) match {
      case (Failure, _) => Failure
      case (_, Failure) => Failure
      case (Success(a), Success(b)) => Success(a ++ b)
      case (_, Concat(left, right)) => (this ++ left) ++ right
      case _ => Concat(this, that)
    }

    /** Sequences `this` and `that` parser. The parsed value from `that` is returned. */
    def ~>~[B](that: Parser[B]): Parser[B] = this.~(that).map(_._2)

    /** Sequences `this` and `that` parser. The parsed value from `this` is returned. */
    def ~<~[B](that: Parser[B]): Parser[A] = this.~(that).map(_._1)

    /** Sequences `this` and `that` parser. The parsed value from `that` is appended to that from `this`. */
    def :+[B](that: Parser[B])(implicit ev1: Parser[A] <:< Parser[Seq[B]], ev2: A <:< Seq[B]): Parser[Seq[B]] =
      this ++ that.map(Vector(_))

    /** Sequences `this` and `that` parser. The parsed value from `that` is prepended to that from `this`. */
    def +:[B](that: Parser[B])(implicit ev1: Parser[A] <:< Parser[Seq[B]], ev2: A <:< Seq[B]): Parser[Seq[B]] =
      that.map(Vector(_)) ++ this

    /** Sequences `this` and `that` parser. The parsed values are returned as a pair. */
    def ~[B](that: Parser[B]): Parser[A ~ B] = (this, that) match {
      case (Failure, _) => Failure
      case (_, Failure) => Failure
      case (Success(a), Success(b)) => Success(scallion.~(a, b))
      case _ => Sequence(this, that)
    }

    /** Disjunction of `this` and `that` parser. */
    def |[B >: A](that: Parser[B]): Parser[B] = (this, that) match {
      case (Failure, _) => that
      case (_, Failure) => this
      case _ => Disjunction(this, that)
    }


    /** Makes the parser nullable. */
    def opt: Parser[Option[A]] = this.map(Some(_)) | epsilon(None)

    /** Consumes a sequence of tokens and parses into a value. */
    def apply(it: Iterator[Token]): ParseResult[A] = {
      require(isLL1 && this.isProductive)

      var parser: Parser[A] = this
      while (it.hasNext) {
        val token = it.next()
        val newParser = parser.derive(token)
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
  }

  /** Result of calling `parse` on a `Parser`. */
  sealed abstract class ParseResult[+A] {
    val parser: Parser[A]
  }

  /** Indicates that the input has been fully processed, resulting in a `value`.
    *
    * A `parser` for subsequent input is also provided.
    */
  case class Parsed[A](value: A, parser: Parser[A]) extends ParseResult[A]

  /** Indicates that the provided `token` was not expected at that point.
    *
    * The `parser` that rejected the token is returned.
    */
  case class UnexpectedToken[A](token: Token, parser: Parser[A]) extends ParseResult[A]

  /** Indicates that end of input was unexpectedly encountered.
    *
    * The `parser` for subsequent input is provided.
    */
  case class UnexpectedEnd[A](parser: Parser[A]) extends ParseResult[A]

  sealed abstract class LL1Conflict
  case class NullableConflict(parser: Parser[Any]) extends LL1Conflict
  case class FirstConflict(parser: Parser[Any]) extends LL1Conflict
  case class FollowConflict(parser: Parser[Any]) extends LL1Conflict
  case class LeftRecursiveConflict(parser: Parser[Any]) extends LL1Conflict

  object Parser {
    case class Success[A](value: A) extends Parser[A] {
      override val nullable: Option[A] = Some(value)
      override val isProductive: Boolean = true
      override def collectNullable(recs: Set[AnyRef]): Option[A] = Some(value)
      override def collectFirst(recs: Set[AnyRef]): Set[Kind] = Set()
      override def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] = Set()
      override def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean = false
      override def collectIsLL1(recs: Set[AnyRef]): Boolean = true
      override def collectLL1Conflicts(recs: Set[AnyRef]): Set[LL1Conflict] = Set()
      override def collectIsProductive(recs: Set[AnyRef]): Boolean = true
      override def derive(token: Token): Parser[A] = Failure
      override def repr(level: Int, recs: Map[AnyRef, String]): String = "epsilon(" + value.toString + ")"
    }
    case object Failure extends Parser[Nothing] {
      override val nullable: Option[Nothing] = None
      override val isProductive: Boolean = false
      override def collectNullable(recs: Set[AnyRef]): Option[Nothing] = None
      override def collectFirst(recs: Set[AnyRef]): Set[Kind] = Set()
      override def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] = Set()
      override def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean = false
      override def collectIsLL1(recs: Set[AnyRef]): Boolean = true
      override def collectLL1Conflicts(recs: Set[AnyRef]): Set[LL1Conflict] = Set()
      override def collectIsProductive(recs: Set[AnyRef]): Boolean = false
      override def derive(token: Token): Parser[Nothing] = Failure
      override def repr(level: Int, recs: Map[AnyRef, String]): String = "failure"
    }
    case class Elem(kind: Kind) extends Parser[Token] {
      override val nullable: Option[Token] = None
      override val isProductive: Boolean = true
      override def collectNullable(recs: Set[AnyRef]): Option[Token] = None
      override def collectFirst(recs: Set[AnyRef]): Set[Kind] = Set(kind)
      override def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] = Set()
      override def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean = false
      override def collectIsLL1(recs: Set[AnyRef]): Boolean = true
      override def collectLL1Conflicts(recs: Set[AnyRef]): Set[LL1Conflict] = Set()
      override def collectIsProductive(recs: Set[AnyRef]): Boolean = true
      override def derive(token: Token): Parser[Token] = if (getKind(token) == kind) Success(token) else Failure
      override def repr(level: Int, recs: Map[AnyRef, String]): String = "elem(" + kind + ")"
    }
    case class Transform[A, B](function: A => B, inner: Parser[A]) extends Parser[B] {
      override lazy val nullable: Option[B] = inner.nullable.map(function)
      override lazy val isProductive: Boolean = inner.isProductive
      override def collectNullable(recs: Set[AnyRef]): Option[B] = inner.collectNullable(recs).map(function)
      override def collectFirst(recs: Set[AnyRef]): Set[Kind] = inner.collectFirst(recs)
      override def collectShouldNotFollow(recs: Set[AnyRef]): Set[Kind] = inner.collectShouldNotFollow(recs)
      override def collectCalledLeft(id: AnyRef, recs: Set[AnyRef]): Boolean = false
      override def collectIsLL1(recs: Set[AnyRef]): Boolean = inner.collectIsLL1(recs)
      override def collectLL1Conflicts(recs: Set[AnyRef]): Set[LL1Conflict] = inner.collectLL1Conflicts(recs)
      override def collectIsProductive(recs: Set[AnyRef]): Boolean = inner.collectIsProductive(recs)
      override def derive(token: Token): Parser[B] = inner.derive(token).map(function)
      override def repr(level: Int, recs: Map[AnyRef, String]): String = inner.repr(10, recs) + ".map(<function>)"
    }
    case class Sequence[A, B](left: Parser[A], right: Parser[B]) extends Parser[A ~ B] {
      override lazy val nullable: Option[A ~ B] = for {
        leftValue <- left.nullable
        rightValue <- right.nullable
      } yield scallion.~(leftValue, rightValue)
      override lazy val isProductive: Boolean = left.isProductive && right.isProductive
      override def collectNullable(recs: Set[AnyRef]): Option[A ~ B] = for {
        leftValue <- left.collectNullable(recs)
        rightValue <- right.collectNullable(recs)
      } yield scallion.~(leftValue, rightValue)
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
      override def collectLL1Conflicts(recs: Set[AnyRef]): Set[LL1Conflict] = {
        val base = left.collectLL1Conflicts(recs) union right.collectLL1Conflicts(recs)

        if (left.nullable.nonEmpty && (left.shouldNotFollow & right.first).nonEmpty) {
          base + FollowConflict(this)
        }
        else {
          base
        }
      }
      override def collectIsProductive(recs: Set[AnyRef]): Boolean =
        left.collectIsProductive(recs) && right.collectIsProductive(recs)
      override def derive(token: Token): Parser[A ~ B] = {
        val derived = left.derive(token)

        if (!derived.isProductive) {
          left.nullable match {
            case Some(leftValue) => Success(leftValue) ~ right.derive(token)
            case None => Failure
          }
        }
        else {
          derived ~ right
        }
      }
      override def repr(level: Int, recs: Map[AnyRef, String]): String = {
        val l = left.repr(9, recs)
        val r = right.repr(10, recs)

        if (level > 9) {
          "(" + l + " ~ " + r + ")"
        }
        else {
          l + " ~ " + r
        }
      }
    }
    case class Concat[A](left: Parser[Seq[A]], right: Parser[Seq[A]]) extends Parser[Seq[A]] {
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
      override def collectLL1Conflicts(recs: Set[AnyRef]): Set[LL1Conflict] = {
        val base = left.collectLL1Conflicts(recs) union right.collectLL1Conflicts(recs)

        if (left.nullable.nonEmpty && (left.shouldNotFollow & right.first).nonEmpty) {
          base + FollowConflict(this)
        }
        else {
          base
        }
      }
      override def collectIsProductive(recs: Set[AnyRef]): Boolean =
        left.collectIsProductive(recs) && right.collectIsProductive(recs)
      override def derive(token: Token): Parser[Seq[A]] = {
        val derived = left.derive(token)

        if (!derived.isProductive) {
          left.nullable match {
            case Some(leftValue) => Success(leftValue) ++ right.derive(token)
            case None => Failure
          }
        }
        else {
          derived ++ right
        }
      }
      override def repr(level: Int, recs: Map[AnyRef, String]): String = {
        val l = left.repr(7, recs)
        val r = right.repr(8, recs)

        if (level > 7) {
          "(" + l + " ++ " + r + ")"
        }
        else {
          l + " ++ " + r
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
      override def collectLL1Conflicts(recs: Set[AnyRef]): Set[LL1Conflict] = {
        val base = left.collectLL1Conflicts(recs) union right.collectLL1Conflicts(recs)

        val nullableConflict: Set[LL1Conflict] = if (left.nullable.nonEmpty && right.nullable.nonEmpty) {
          Set(NullableConflict(this))
        }
        else {
          Set()
        }

        val firstConflict: Set[LL1Conflict] = if ((left.first & right.first).nonEmpty) {
          Set(FirstConflict(this))
        }
        else {
          Set()
        }

        base union nullableConflict union firstConflict
      }
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
      override def repr(level: Int, recs: Map[AnyRef, String]): String = {
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
    case class Recursive[A](computation: () => Parser[A]) extends Parser[A] {
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
      override def collectLL1Conflicts(recs: Set[AnyRef]): Set[LL1Conflict] = 
        if (recs.contains(this)) Set() else {
          val base = inner.collectLL1Conflicts(recs + this)

          if (inner.calledLeft(this)) {
            base + LeftRecursiveConflict(this)
          }
          else {
            base
          }
        }
      override def collectIsProductive(recs: Set[AnyRef]): Boolean =
        if (recs.contains(this)) false else inner.collectIsProductive(recs + this)
      override def derive(token: Token): Parser[A] =
        inner.derive(token)
      override def repr(level: Int, recs: Map[AnyRef, String]): String = {
        recs.get(this) match {
          case None => {
            val n = (recs.size + 1).toString
            "recursive<" + n + ">(" + inner.repr(0, recs + (this -> n)) + ")"
          }
          case Some(n) => "<" + n + ">"
        }
      }
    }
  }

  /** Parser that accepts tokens of the provided `kind`. */
  def elem(kind: Kind): Parser[Token] = Elem(kind)

  /** Parser that accepts tokens of the provided `kind`.
    * A function directly is applied on the successfully matched token. */
  def accept[A](kind: Kind)(function: PartialFunction[Token, A]): Parser[A] = elem(kind).map(function)

  /** Indicates that the parser can be recursively invoke itself. */
  def recursive[A](parser: => Parser[A]): Parser[A] = Recursive(() => parser)

  /** Parser that produces the given `value` without consuming any input. */
  def epsilon[A](value: A): Parser[A] = Success(value)

  /** Parser that always fails. */
  def failure[A]: Parser[A] = Failure

  /** Parser that represents 0 or more repetitions of the `rep` parser. */
  def many[A](rep: Parser[A]): Parser[Seq[A]] = {
    lazy val rest: Parser[Seq[A]] = recursive(rep +: rest | epsilon(Vector()))
    rest
  }

  /** Parser that represents 1 or more repetitions of the `rep` parser. */
  def many1[A](rep: Parser[A]): Parser[Seq[A]] = rep +: many(rep)

  /** Parser that represents 0 or more repetitions of the `rep` parser, separated by `sep`. */
  def repsep[A](rep: Parser[A], sep: Parser[Any]): Parser[Seq[A]] = rep1sep(rep, sep) | epsilon(Vector())

  /** Parser that represents 1 or more repetitions of the `rep` parser, separated by `sep`. */
  def rep1sep[A](rep: Parser[A], sep: Parser[Any]): Parser[Seq[A]] = {
    lazy val rest: Parser[Seq[A]] = recursive((sep ~>~ rep) +: rest | epsilon(Vector()))
    rep +: rest
  }

  /** Parser that represents the disjunction of several `parsers`. */
  def oneOf[A](parsers: Parser[A]*): Parser[A] =
    parsers.foldRight(failure[A]) {
      case (parser, acc) => parser | acc
    }

  /** Parser that accepts repetitions of `elem` separated by left-associative `op`.
    * The value returned is reduced left-to-right. */
  def infixLeft[A](elem: Parser[A], op: Parser[(A, A) => A]): Parser[A] =
    (elem ~ many(op ~ elem)).map {
      case first ~ opElems => opElems.foldLeft(first) {
        case (acc, (op ~ elem)) => op(acc, elem)
      }
    }

  /** Parser that accepts repetitions of `elem` separated by right-associative `op`.
    * The value returned is reduced right-to-left. */
  def infixRight[A](elem: Parser[A], op: Parser[(A, A) => A]): Parser[A] =
    (elem ~ many(op ~ elem)).map {
      case first ~ opElems => {
        val (ops, elems) = opElems.map(t => (t._1, t._2)).unzip
        val allElems = first +: elems
        val elemOps = allElems.zip(ops)
        elemOps.foldRight(allElems.last) {
          case ((elem, op), acc) => op(elem, acc)
        }
      }
    }
}
