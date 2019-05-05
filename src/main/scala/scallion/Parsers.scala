package scallion

trait Parsers extends Tokenizers {

  type Repr = Seq[Character]

  trait HasRepr {
    def repr: Repr
  }

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

  sealed abstract class ParseResult[+T] {
    def map[S](function: T => S): ParseResult[S] = this match {
      case Complete(value) => Complete(function(value))
      case Incomplete(rest) => Incomplete(rest.map(function))
      case f@Failed(_, _) => f
    }
  }
  case class Complete[T](value: T) extends ParseResult[T]
  case class Incomplete[T](rest: Parser[T]) extends ParseResult[T]
  case class Failed(range: Range, error: ErrorMessage) extends ParseResult[Nothing]

  case class &&[+A, +B](fst: A, snd: B)

  /** Resolves sequences of tokens into values of type `T`.
   *
   * This class describes LL(1) parsers:
   * - The input is consumed left-to-right.
   * - In case of sequences, the leftmost parser are queried first.
   * - A single token of lookahead is available.
   *
   * Importantly, the parsers do not support backtracking.
   */
  sealed abstract class Parser[+T] {

    import Combinators._

    def acceptsEmpty: Boolean
    def reprs: Seq[Repr]
    def parse(input: Input): ParseResult[T]

    final def explain(error: ErrorMessage): Parser[T] =
      Disjunction(this, Failure(error))
    final def map[U](function: T => U): Parser[U] =
      Transform(this, function)
    final def <>[U](that: => Parser[U]): Parser[(T, U)] =
      Sequence(this, Lazy(() => that))
    final def &&[U](that: => Parser[U]): Parser[T && U] =
      Transform(Sequence(this, Lazy(() => that)), (p: (T, U)) => Parsers.this.&&(p._1, p._2))
    final def <<[U](that: => Parser[U]): Parser[T] =
      Transform(Sequence(this, Lazy(() => that)), (p: (T, U)) => p._1)
    final def >>[U](that: => Parser[U]): Parser[U] =
      Transform(Sequence(this, Lazy(() => that)), (p: (T, U)) => p._2)
    final def |[U >: T](that: Parser[U]): Parser[U] =
      Disjunction(this, that)
    final def filter(error: T => ErrorMessage)(predicate: T => Boolean): Parser[T] =
      Filter(this, predicate, error, (repr: Repr) => true)
    final def validate[U](function: T => Either[ErrorMessage, U]): Parser[U] =
      this.map(function).filter(_.left.get)(_.isRight).map(_.right.get)
  }

  object Combinators {
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
    }

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
    }

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

    case class Accept[A](
        classifier: PartialFunction[Token, A],
        reprs: Seq[Repr],
        error: Token => ErrorMessage) extends Parser[A] {

      override val acceptsEmpty: Boolean = false
      override def parse(input: Input): ParseResult[A] = {
        if (input.atEnd) {
          Incomplete(this)
        }
        else {
          val token = input.current
          classifier.lift(token) match {
            case Some(value) => {
              input.skip()
              Complete(value)
            }
            case None => Failed(input.currentRange, error(token))
          }
        }
      }
    }

    case class Transform[S, T](parser: Parser[S], function: S => T) extends Parser[T] {
      override def acceptsEmpty: Boolean = parser.acceptsEmpty
      override def reprs: Seq[Repr] = parser.reprs
      override def parse(input: Input): ParseResult[T] =
        parser.parse(input).map(function)
    }

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
    }

    case class Filter[T](
        parser: Parser[T],
        predicate: T => Boolean,
        error: T => ErrorMessage,
        reprPredicate: Repr => Boolean,
        optStartPos: Option[Position] = None,
        optStartIndex: Option[Int] = None) extends Parser[T] {

      override def acceptsEmpty: Boolean = parser.acceptsEmpty
      override def reprs: Seq[Repr] = parser.reprs.filter(reprPredicate)
      override def parse(input: Input): ParseResult[T] = {
        if (input.atEnd) {
          Incomplete(this)
        }
        else {
          val startPosition = optStartPos.getOrElse(input.currentStart)
          val startIndex = optStartIndex.getOrElse(input.currentIndex)
          parser.parse(input) match {
            case f@Failed(_, _) => f
            case Incomplete(rest) =>
              Incomplete(copy(parser=rest,
                optStartPos=Some(startPosition),
                optStartIndex=Some(startIndex)))
            case Complete(value) if !predicate(value) => {
              val range = {
                if (input.currentIndex > startIndex) {
                  (startPosition, input.previousEnd)
                }
                else {
                  (startPosition, startPosition)
                }
              }
              Failed(range, error(value))
            }
            case c@Complete(_) => c
          }
        }
      }
    }

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
    }

    case class Lazy[A](computation: () => Parser[A]) extends Parser[A] {
      private lazy val parser: Parser[A] = computation()

      override def acceptsEmpty: Boolean = parser.acceptsEmpty
      override def reprs: Seq[Repr] = parser.reprs
      override def parse(input: Input): ParseResult[A] = parser.parse(input)
    }
  }

  import Combinators._

  /** Produces `value` without consuming any input. */
  def success[A](value: A): Parser[A] = Success(value)

  /** Fails without consuming any input. */
  def fail(error: ErrorMessage): Parser[Nothing] = Failure(error)

  /** Matches a single token against `token`.
   *
   * Consumes the next input token if it matches, fails otherwise.
   */
  def elem(token: Token with HasRepr, error: Token => ErrorMessage): Parser[Token] =
    Element(token, Seq(token.repr), error)

  /** Matches a single token against partial function.
   *
   * Consumes the next input token if it matches, fails otherwise.
   */
  def accepts[A](error: Token => ErrorMessage, reprs: Repr*)(function: PartialFunction[Token, A]): Parser[A] =
    Accept(function, reprs, error)

  /** Successively applies the provided `parsers` until one
   *  either produces a value or consumes input.
   */
  def oneOf[A](error: ErrorMessage)(parsers: Parser[A]*): Parser[A] = {
    val zero: Parser[A] = fail(error)
    parsers.foldRight(zero) {
      case (parser, rest) => parser | rest
    }
  }

  /** Repeatedly applies the provided parser. */
  def many[A](parser: Parser[A]): Parser[Seq[A]] = Repeat(parser)

  /** Applies the provided parser at least once. */
  def many1[A](parser: Parser[A]): Parser[Seq[A]] =
    Sequence(parser, Repeat(parser)).map {
      case (x, xs) => x +: xs
    }

  /** Catches failures that do not consume input.
   *
   * When the underlying parser fails without consuming any tokens,
   * the resulting parser instead produces None.
   */
  def opt[A](parser: Parser[A]): Parser[Option[A]] =
    parser.map(Some(_)) | success(None)

  /** Matches against the `EndToken` token. */
  def end(error: Token => ErrorMessage): Parser[Unit] =
    Element(EndToken, Seq(), error).map((_: Token) => ())

  def phrase[A](parser: Parser[A], error: Token => ErrorMessage): Parser[A] =
    parser << end(error)

  def repsep[A](rep: Parser[A], sep: Parser[Any]): Parser[Seq[A]] =
    rep <> many(sep >> rep) map {
      case (x, xs) => x +: xs
    }

  def prefixes[A](prefix: Parser[A => A], parser: Parser[A]): Parser[A] =
    many(prefix) <> parser map {
      case (ps, v) => ps.foldRight(v) { case (p, acc) => p(acc) }
    }

  def postfixes[A](parser: Parser[A], postfix: Parser[A => A]): Parser[A] =
    parser <> many(postfix) map {
      case (v, ps) => ps.foldLeft(v) { case (acc, p) => p(acc) }
    }

  def infixesLeft[A](parser: Parser[A], operator: Parser[(A, A) => A]): Parser[A] =
    parser <> many(operator <> parser) map {
      case (v, opUs) => opUs.foldLeft(v) {
        case (acc, (op, u)) => op(acc, u)
      }
    }

  def infixesRight[A](parser: Parser[A], operator: Parser[(A, A) => A]): Parser[A] =
    parser <> many(operator <> parser) map {
      case (v, Seq()) => v
      case (v, opUs) => {
        val (ops, us) = opUs.unzip
        val vs = (v +: us)

        vs.init.zip(ops).foldRight(vs.last) {
          case ((u, op), acc) => op(u, acc)
        }
      }
    }
}