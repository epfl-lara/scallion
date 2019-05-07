package scallion

/** Contains definitions for recursive descent LL(1) parsers. */
trait Parsers extends Tokens with Positions {

  /** Representation of tokens. Used for representing the next possible tokens. */
  type Repr

  /** Indicates that this class has a representation. */
  trait HasRepr {
    def repr: Repr
  }

  /** The type of errors produced by the parsers. */
  type ErrorMessage

  /** Input stream with a single token look-ahead. */
  class Input(it: Iterator[(Token, Range)]) {

    /** The current token. */
    private var optCurrentToken: Option[Token] = None

    /** The range of the current token. */
    private var optCurrentRange: Option[Range] = None

    /** The range of the previous token. */
    private var optPreviousRange: Option[Range] = None

    /** Number of tokens consumed so far (excluding current). */
    private var consumedTokens: Int = 0

    // Immediately tries to fetch the first token.
    fetchNext()

    /** Get the next token from the iterator, if any. */
    private def fetchNext(): Unit = {
      if (it.hasNext) {
        val (token, range) = it.next()
        optCurrentToken  = Some(token)
        optPreviousRange = optCurrentRange
        optCurrentRange  = Some(range)
      }
      else if (optCurrentToken.nonEmpty) {
        optCurrentToken  = None
        optPreviousRange = optCurrentRange
        optCurrentRange  = None
      }
    }

    /** Checks if there are no more tokens to consume.
     *
     * When the input is empty, returns true.
     */
    def atEnd: Boolean = optCurrentToken.isEmpty

    /** Checks if the input starts at the current token.
     *
     * When the input is empty, returns true.
     */
    def atStart: Boolean = optPreviousRange.isEmpty

    /** The current token. Fails when the input has ended. */
    def current: Token = optCurrentToken.getOrElse {
      throw new IllegalStateException("Input past end.")
    }

    /** The range of the current token. Fails when the input has ended. */
    def currentRange: Range = optCurrentRange.getOrElse {
      throw new IllegalStateException("Input past end.")
    }

    /** The range of the previous token. Fails when the input is at the start. */
    def previousRange: Range = optPreviousRange.getOrElse {
      throw new IllegalStateException("Input at start.")
    }

    /** Start position of the current token. Fails when the input has ended. */
    def currentStart: Position = currentRange._1

    /** End position of the previous token. Fails when the input is at the start. */
    def previousEnd: Position = previousRange._2

    /** Skips the current token, if any. */
    def skip(): Unit = {
      if (optCurrentToken.nonEmpty) {
        consumedTokens += 1
      }

      fetchNext()
    }

    /** Number of tokens consumed. */
    def currentIndex: Int = consumedTokens
  }

  /** Result of a parser run. */
  sealed abstract class ParseResult[+T] {

    /** Applies a function to the parsed value. */
    def map[S](function: T => S): ParseResult[S] = this match {
      case Complete(value) => Complete(function(value))
      case Incomplete(rest) => Incomplete(rest.map(function))
      case f@Failed(_, _) => f
    }
  }

  /** Successfully parsed `value`. */
  case class Complete[T](value: T) extends ParseResult[T]

  /** Indicates incomplete input. */
  case class Incomplete[T](rest: Parser[T]) extends ParseResult[T]

  /** Indicates a failed parse. */
  case class Failed(range: Range, error: ErrorMessage) extends ParseResult[Nothing]

  /** Pairs together two values.
    *
    * Designed to be used as an infix operator.
    */
  case class &&[+A, +B](fst: A, snd: B)

  /** Resolves sequences of tokens into values of type `T`.
    *
    * The type of parsers that can be described by are LL(1) parsers:
    * - The input is consumed left-to-right.
    * - In case of sequences, the leftmost parser are queried first.
    * - A single token of lookahead is available.
    *
    * Importantly, such parsers do not support backtracking.
    */
  sealed abstract class Parser[+T] {

    import Combinators._

    // Abstract members.

    /** Indicates if this parser accepts the empty string. */
    def acceptsEmpty: Boolean

    /** Returns representation of the first token that can be consumed. */
    def reprs: Seq[Repr]

    /** Parses the input. */
    def parse(input: Input): ParseResult[T]

    def isLeftRecursive(accept: Set[Int] = Set(), reject: Set[Int] = Set()): Boolean = false

    // Concrete members

    /** Applies a function to the parsed value. */
    def map[U](function: T => U): Parser[U] =
      Transform(this, function)

    /** Sequences `this` and `that` parser.
      *
      * Returns both values as a pair.
      */
    def <>[U](that: Parser[U]): Parser[(T, U)] =
      Sequence(this, that)

    /** Sequences `this` and `that` parser.
      *
      * Returns both values as a `&&`-pair.
      */
    def &&[U](that: Parser[U]): Parser[T && U] =
      Transform(Sequence(this, that), (p: (T, U)) => Parsers.this.&&(p._1, p._2))

    /** Sequences `this` and `that` parser.
      *
      * Returns only the first value.
      */
    def <<[U](that: Parser[U]): Parser[T] =
      Transform(Sequence(this, that), (p: (T, U)) => p._1)

    /** Sequences `this` and `that` parser.
      *
      * Returns only the second value.
      */
    def >>[U](that: Parser[U]): Parser[U] =
      Transform(Sequence(this, that), (p: (T, U)) => p._2)

    /** Falls back to `that` parser in case `this` parser
      *  fails without consuming input.
      */
    def |[U >: T](that: Parser[U]): Parser[U] =
      Disjunction(this, that)

    /** Specifies the error message in case `this` parser
      *  fails without consuming input.
      */
    def explain(error: ErrorMessage): Parser[T] =
      Disjunction(this, Failure(error))

    /** Associates this `parser` with an `associativity`.
      *
      * @see The function `operators`.
      */
    def |>[A](associativity: Associativity)
        (implicit ev: this.type <:< Parser[((A, A) => A)]): Level[A] =
      Level(ev(this), associativity)
  }

  /** Contains primitive parser combinators. */
  object Combinators {

    /** Sequential composition of parsers. */
    case class Sequence[S, T](pre: Parser[S], post: Parser[T]) extends Parser[(S, T)] {
      override def acceptsEmpty: Boolean = pre.acceptsEmpty && post.acceptsEmpty
      override def reprs: Seq[Repr] = if (pre.acceptsEmpty) pre.reprs ++ post.reprs else pre.reprs
      override def parse(input: Input): ParseResult[(S, T)] = pre.parse(input) match {
        case Complete(preValue) => post.parse(input) match {
          case Complete(postValue) => Complete((preValue, postValue))
          case Incomplete(rest) => Incomplete(rest.map(postValue => (preValue, postValue)))
          case f@Failed(_, _) => f
        }
        case Incomplete(rest) => Incomplete(Sequence(rest, post))
        case f@Failed(_, _) => f
      }
      override def isLeftRecursive(accept: Set[Int], reject: Set[Int]): Boolean =
        if (pre.isLeftRecursive(accept, reject)) true
        else if (pre.acceptsEmpty) post.isLeftRecursive(accept, reject)
        else post.isLeftRecursive(accept union reject, Set())
    }

    /** Disjunction of parsers. */
    case class Disjunction[S](left: Parser[S], right: Parser[S]) extends Parser[S] {
      override def acceptsEmpty: Boolean = left.acceptsEmpty || right.acceptsEmpty
      override def reprs: Seq[Repr] = left.reprs ++ right.reprs
      override def parse(input: Input): ParseResult[S] = {
        val previousIndex = input.currentIndex
        left.parse(input) match {
          case c@Complete(leftValue) => c
          case i@Incomplete(rest) => {
            if (input.currentIndex > previousIndex) {
              // We commit to the left branch,
              // since some input was consumed.
              i
            }
            else {
              Incomplete(this)
            }
          }
          case f@Failed(_, _) => {
            if (input.currentIndex > previousIndex) {
              // We commit to the error,
              // since some input was consumed.
              f
            }
            else {
              // We try the right branch.
              right.parse(input)
            }
          }
        }
      }
      override def isLeftRecursive(accept: Set[Int], reject: Set[Int]): Boolean =
        left.isLeftRecursive(accept, reject) || right.isLeftRecursive(accept, reject)
    }

    /** Produces value without consuming input. */
    case class Success[T](value: T) extends Parser[T] {
      override val acceptsEmpty: Boolean = true
      override val reprs: Seq[Repr] = Seq()
      override def parse(input: Input): ParseResult[T] = {
        if (input.atEnd) {
          Incomplete(this)
        }
        else {
          Complete(value)
        }
      }
    }

    /** Produces an error without consuming input. */
    case class Failure(error: ErrorMessage) extends Parser[Nothing] {
      override val acceptsEmpty: Boolean = false
      override val reprs: Seq[Repr] = Seq()
      override def parse(input: Input): ParseResult[Nothing] = {
        if (input.atEnd) {
          Incomplete(this)
        }
        else {
          val start = input.currentStart
          Failed((start, start), error)
        }
      }
    }

    /** Matches a token against a single `token`. */
    case class Element(
        token: Token,
        reprs: Seq[Repr],
        error: Token => ErrorMessage) extends Parser[Token] {

      override val acceptsEmpty: Boolean = false
      override def parse(input: Input): ParseResult[Token] = {
        if (input.atEnd) {
          Incomplete(this)
        }
        else {
          val candidate = input.current
          if (candidate == token) {
            input.skip()
            Complete(candidate)
          }
          else {
            Failed(input.currentRange, error(candidate))
          }
        }
      }
    }

    /** Matches a single token against a partial function. */
    case class Accept[A](
        classifier: Token => Either[ErrorMessage, A],
        reprs: Seq[Repr]) extends Parser[A] {

      override val acceptsEmpty: Boolean = false
      override def parse(input: Input): ParseResult[A] = {
        if (input.atEnd) {
          Incomplete(this)
        }
        else {
          val token = input.current
          classifier(token) match {
            case Right(value) => {
              input.skip()
              Complete(value)
            }
            case Left(error) =>
              Failed(input.currentRange, error)
          }
        }
      }
    }

    /** Applies a function on the parsed value. */
    case class Transform[S, T](parser: Parser[S], function: S => T) extends Parser[T] {
      override def acceptsEmpty: Boolean = parser.acceptsEmpty
      override def reprs: Seq[Repr] = parser.reprs
      override def parse(input: Input): ParseResult[T] =
        parser.parse(input).map(function)
      override def isLeftRecursive(accept: Set[Int], reject: Set[Int]): Boolean =
        parser.isLeftRecursive(accept, reject)
    }

    /** Repeatedly queries the given `parser`. */
    case class Repeat[T](parser: Parser[T]) extends Parser[Seq[T]] {
      override val acceptsEmpty: Boolean = true
      override def reprs: Seq[Repr] = parser.reprs
      override def parse(input: Input): ParseResult[Seq[T]] = {
        val previousIndex = input.currentIndex
        parser.parse(input) match {
          case Complete(value) =>
            if (input.currentIndex == previousIndex) {
              Complete(Stream.continually(value))
            }
            else {
              parse(input).map(values => value +: values)
            }
          case Incomplete(rest) =>
            if (input.currentIndex == previousIndex) {
              Incomplete(this)
            }
            else {
              Incomplete(Sequence(rest, Repeat(parser)).map {
                case (value, values) => value +: values
              })
            }
          case f@Failed(_, _) =>
            if (input.currentIndex == previousIndex) {
              Complete(Seq())
            }
            else f
        }
      }
      override def isLeftRecursive(accept: Set[Int], reject: Set[Int]): Boolean =
        parser.isLeftRecursive(accept, reject)
    }

    /** Decorated a parsed value with the start and end position of the
      * sequences of tokens consumed.
      *
      * In case the underlying parser doesn't consume any tokens,
      * the range will be equals to `(start, start)`, where
      * `start` is the start position of the next token.
      */
    case class Positioned[T](
        parser: Parser[T],
        optStartPos: Option[Position] = None,
        optStartIndex: Option[Int] = None) extends Parser[(T, Range)] {

      override def acceptsEmpty: Boolean = parser.acceptsEmpty
      override def reprs: Seq[Repr] = parser.reprs
      override def parse(input: Input): ParseResult[(T, Range)] = {
        if (input.atEnd) {
          Incomplete(this)
        }
        else {
          val startPosition = optStartPos.getOrElse(input.currentStart)
          val startIndex = optStartIndex.getOrElse(input.currentIndex)

          parser.parse(input) match {
            case Complete(value) => {
              val range = {
                if (input.currentIndex > startIndex) {
                  (startPosition, input.previousEnd)
                }
                else {
                  (startPosition, startPosition)
                }
              }
              Complete((value, range))
            }
            case Incomplete(rest) => {
              Incomplete(Positioned(rest,
                optStartPos=Some(startPosition),
                optStartIndex=Some(startIndex)))
            }
            case f@Failed(_, _) => f
          }
        }
      }
      override def isLeftRecursive(accept: Set[Int], reject: Set[Int]): Boolean =
        parser.isLeftRecursive(accept, reject)
    }

    /** Lazy wrapper around a parser. Allows for recursive parser values. */
    case class Lazy[A](computation: () => Parser[A]) extends Parser[A] {
      val id = Lazy.nextId()
      private lazy val parser: Parser[A] = computation()

      override def acceptsEmpty: Boolean = parser.acceptsEmpty
      override def reprs: Seq[Repr] = parser.reprs
      override def parse(input: Input): ParseResult[A] = parser.parse(input)
      override def isLeftRecursive(accept: Set[Int], reject: Set[Int]): Boolean =
        if (reject.contains(id)) true
        else if (accept.contains(id)) false
        else parser.isLeftRecursive(accept, reject + id)
    }

    object Lazy {
      private var currentId = 0
      private def nextId(): Int = synchronized {
        currentId += 1
        currentId
      }
    }
  }

  import Combinators._

  /** A parser that produces `value` without consuming any input. */
  def success[A](value: A): Parser[A] = Success(value)

  /** A parser that fails without consuming any input. */
  def fail(error: ErrorMessage): Parser[Nothing] = Failure(error)

  /** A parser that matches against `token`.
    *
    * Consumes the next input token if it matches, fails otherwise.
    */
  def elem(token: Token with HasRepr, error: Token => ErrorMessage): Parser[Token] =
    Element(token, Seq(token.repr), error)

  /** A parser that matches single tokens in the domain of the partial function.
    *
    * Consumes the next input token if it matches, fails otherwise.
    */
  def accepts[A](error: Token => ErrorMessage, reprs: Repr*)(function: PartialFunction[Token, A]): Parser[A] =
    Accept((token: Token) => function.lift(token).map(Right(_)).getOrElse(Left(error(token))), reprs)

  /** A parser that matches single tokens in the domain of the partial function.
    *
    * Consumes the next input token if it matches, fails otherwise.
    */
  def classify[A](reprs: Repr*)(function: Token => Either[ErrorMessage, A]): Parser[A] =
    Accept(function, reprs)

  /** A parser that tries the provided `parsers` until one
    * either produces a value or consumes input.
    */
  def oneOf[A](error: ErrorMessage)(parsers: Parser[A]*): Parser[A] = {
    val zero: Parser[A] = fail(error)
    parsers.foldRight(zero) {
      case (parser, rest) => parser | rest
    }
  }

  /** A parser that repeatedly applies the provided parser until it fails.
    *
    * If the last invocation fails without consuming input,
    * the sequence of parsed values is returned.
    * Otherwise, the error is produced.
    */
  def many[A](parser: Parser[A]): Parser[Seq[A]] = Repeat(parser)

  /** A parser that repeatedly applies the provided parser until it fails.
    * Also fails when the underlying parser immediatly fails.
    *
    * If the last invocation fails without consuming input,
    * the sequence of parsed values is returned.
    * Otherwise, the error is produced.
    */
  def many1[A](parser: Parser[A]): Parser[Seq[A]] =
    Sequence(parser, Repeat(parser)).map {
      case (x, xs) => x +: xs
    }

  /** A parser that catches failures that do not consume input.
    *
    * When the underlying parser fails without consuming any tokens,
    * the resulting parser instead produces `None`.
    */
  def opt[A](parser: Parser[A]): Parser[Option[A]] =
    parser.map(Some(_)) | success(None)

  /** Indicates that the parser may recursively call itself. */
  def rec[A](parser: => Parser[A]): Parser[A] =
    Lazy(() => parser)

  /** A parser that matches against the `EndToken` token. */
  def end(error: Token => ErrorMessage): Parser[Unit] =
    Element(EndToken, Seq(), error).map((_: Token) => ())

  /** A parser that ensures that the consumed input ends with an `EndToken`. */
  def phrase[A](parser: Parser[A], error: Token => ErrorMessage): Parser[A] =
    parser << end(error)

  /** A parser that matches against non-empty sequences of `rep` separated by `sep`. */
  def repsep[A](rep: Parser[A], sep: Parser[Any]): Parser[Seq[A]] =
    rep <> many(sep >> rep) map {
      case (x, xs) => x +: xs
    }

  /** A parser that applies any number of prefixes to a parser. */
  def prefixes[A](prefix: Parser[A => A], parser: Parser[A]): Parser[A] =
    many(prefix) <> parser map {
      case (ps, v) => ps.foldRight(v) { case (p, acc) => p(acc) }
    }

  /** A parser that applies any number of postfix to a parser. */
  def postfixes[A](parser: Parser[A], postfix: Parser[A => A]): Parser[A] =
    parser <> many(postfix) map {
      case (v, ps) => ps.foldLeft(v) { case (acc, p) => p(acc) }
    }

  /** A parser that matches sequences of `operand` separated by (left-associative) `operator`. */
  def infixesLeft[A](operand: Parser[A], operator: Parser[(A, A) => A]): Parser[A] =
    operand <> many(operator <> operand) map {
      case (v, opUs) => opUs.foldLeft(v) {
        case (acc, (op, u)) => op(acc, u)
      }
    }

  /** A parser that matches sequences of `operand` separated by (right-associative) `operator`. */
  def infixesRight[A](operand: Parser[A], operator: Parser[(A, A) => A]): Parser[A] =
    operand <> many(operator <> operand) map {
      case (v, Seq()) => v
      case (v, opUs) => {
        val (ops, us) = opUs.unzip
        val vs = (v +: us)

        vs.init.zip(ops).foldRight(vs.last) {
          case ((u, op), acc) => op(u, acc)
        }
      }
    }

  sealed abstract class Associativity
  object Associativity {
    case object Left extends Associativity
    case object Right extends Associativity
  }

  case class Level[A](operator: Parser[(A, A) => A], assoc: Associativity)

  /** A parser that matches against applications of infix operators. */
  def operators[A](operand: Parser[A])(levels: Level[A]*): Parser[A] = {
    levels.foldRight(operand) {
      case (Level(op, assoc), acc) => assoc match {
        case Associativity.Left => infixesLeft(acc, op)
        case Associativity.Right => infixesRight(acc, op)
      }
    }
  }


  // Debug functions.
  def checkRecs(parser: Parser[Any]): Unit = {
    if (parser.isLeftRecursive()) {
      throw new Error("Parser is left-recursive")
    }
  }
}