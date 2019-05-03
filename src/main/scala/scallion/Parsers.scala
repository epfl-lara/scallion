package scallion

trait Parsers extends Tokenizers {
  
  /** Represents a groups of tokens and representative character sequences. */
  trait TokenClassifier { self =>
    def accepts(token: Token): Boolean
    def representatives: Seq[Seq[Character]]

    def ++(that: TokenClassifier): TokenClassifier = new TokenClassifier {
      override def accepts(token: Token): Boolean =
        self.accepts(token) && that.accepts(token)
    
      override def representatives: Seq[Seq[Character]] =
        self.representatives ++ that.representatives
    }
  }
  object TokenClassifier {
    case class Singleton(token: Token) extends TokenClassifier {
      override def accepts(other: Token): Boolean = other == token
      override def representatives: Seq[Seq[Character]] = Seq(represent(token))
    }
    case object Empty extends TokenClassifier {
      override def accepts(token: Token): Boolean = false
      override def representatives: Seq[Seq[Character]] = Seq()
    }

    def apply(reprs: Seq[Character]*)(predicate: Token => Boolean): TokenClassifier = new TokenClassifier {
      override def accepts(token: Token): Boolean = predicate(token)
      override def representatives: Seq[Seq[Character]] = reprs
    }
  }

  type Error

  class Input(it: Iterator[(Token, Range)]) {
    private var optCurrentToken: Option[Token] = None
    private var optCurrentRange: Option[Range] = None
    private var optPreviousRange: Option[Range] = None
    private var consumedTokens: Int = 0

    fetchNext()

    private def fetchNext(): Unit = if (it.hasNext) {
      val (token, range) = it.next()
      optCurrentToken  = Some(token)
      optPreviousRange = optCurrentRange
      optCurrentRange  = Some(range)
    } else {
      optCurrentToken  = None
      optPreviousRange = optCurrentRange
      optCurrentRange  = None
    }

    def atEnd: Boolean = optCurrentToken.isEmpty
    def atStart: Boolean = optPreviousRange.isEmpty

    def current: Token = optCurrentToken.getOrElse(throw new IllegalStateException("Input at end."))
    def currentRange: Range = optCurrentRange.getOrElse(throw new IllegalStateException("Input at end."))
    def previousRange: Range = optPreviousRange.getOrElse(throw new IllegalStateException("Input at start."))

    def currentStart: Position = currentRange._1
    def previousEnd: Position = previousRange._2

    def skip(): Unit = if (optCurrentToken.nonEmpty) {
      fetchNext()
      consumedTokens += 1
    }

    def currentIndex: Int = consumedTokens
  }

  sealed abstract class ParseResult[+T] {
    def map[S](function: T => S): ParseResult[S] = this match {
      case Complete(value) => Complete(function(value))
      case Incomplete(rest) => Incomplete(rest.map(function))
      case Failed(error) => Failed(error)
    }
  }
  case class Complete[T](value: T) extends ParseResult[T]
  case class Incomplete[T](rest: Parser[T]) extends ParseResult[T]
  case class Failed(error: Error) extends ParseResult[Nothing]

  case class &&[+A, +B](fst: A, snd: B)

  case class Lazy[A](computation: () => A) {
    lazy val value = computation()
  }

  sealed abstract class Parser[+T] {

    import Combinators._

    def acceptsEmpty: Boolean
    def first: TokenClassifier
    def parse(input: Input): ParseResult[T]

    final def explain(error: Error): Parser[T] = Explain(this, error)
    final def map[U](function: T => U): Parser[U] = Transform(this, function)
    final def <>[U](that: => Parser[U]): Parser[(T, U)] = Sequence(this, that)
    final def &&[U](that: => Parser[U]): Parser[T && U] = Transform(Sequence(this, that), (p: (T, U)) => Parsers.this.&&(p._1, p._2))
    final def <<[U](that: => Parser[U]): Parser[T] = Transform(Sequence(this, that), (p: (T, U)) => p._1)
    final def >>[U](that: => Parser[U]): Parser[U] = Transform(Sequence(this, that), (p: (T, U)) => p._2)
    final def |[U >: T](that: Parser[U]): Parser[U] = Disjunction(this, that)
  }

  object Combinators {
    case class Sequence[S, T](pre: Parser[S], post: Lazy[Parser[T]]) extends Parser[(S, T)] {
      override def acceptsEmpty: Boolean = pre.acceptsEmpty && post.value.acceptsEmpty
      override def first: TokenClassifier = if (pre.acceptsEmpty) pre.first ++ post.value.first else pre.first
      override def parse(input: Input): ParseResult[(S, T)] = pre.parse(input) match {
        case Complete(preValue) => post.value.parse(input) match {
          case Complete(postValue) => Complete((preValue, postValue))
          case Incomplete(rest) => Incomplete(rest.map(postValue => (preValue, postValue)))
          case Failed(error) => Failed(error)
        }
        case Incomplete(rest) => Incomplete(Sequence(rest, post))
        case Failed(error) => Failed(error)
      }
    }

    object Sequence {
      def apply[S, T](pre: Parser[S], post: => Parser[T]): Sequence[S, T] = Sequence(pre, Lazy(() => post))
    }

    case class Disjunction[S](left: Parser[S], right: Parser[S]) extends Parser[S] {
      override def acceptsEmpty: Boolean = left.acceptsEmpty || right.acceptsEmpty
      override def first: TokenClassifier = left.first ++ right.first
      override def parse(input: Input): ParseResult[S] = {
        val previousIndex = input.currentIndex
        left.parse(input) match {
          case Complete(leftValue) => Complete(leftValue)
          case Incomplete(rest) => {
            if (input.currentIndex > previousIndex) {
              Incomplete(rest)
            }
            else {
              Incomplete(Disjunction(left, right))
            }
          }
          case Failed(error) => {
            if (input.currentIndex > previousIndex) {
              Failed(error)
            }
            else {
              right.parse(input)
            }
          }
        }
      }
    }

    case class Success[T](value: T) extends Parser[T] {
      override val acceptsEmpty: Boolean = true
      override val first: TokenClassifier = TokenClassifier.Empty
      override def parse(input: Input): ParseResult[T] = Complete(value)
    }

    case class Failure(error: Error) extends Parser[Nothing] {
      override val acceptsEmpty: Boolean = false
      override val first: TokenClassifier = TokenClassifier.Empty
      override def parse(input: Input): ParseResult[Nothing] = Failed(error)
    }

    case class Accept(TokenClassifier: TokenClassifier, error: Error) extends Parser[Token] {
      override val acceptsEmpty: Boolean = false
      override val first: TokenClassifier = TokenClassifier
      override def parse(input: Input): ParseResult[Token] = if (!input.atEnd) {
        val token = input.current
        if (TokenClassifier.accepts(token)) {
          input.skip()
          Complete(token)
        } else Failed(error)
      } else {
        Incomplete(this)
      }
    }

    case class Transform[S, T](parser: Parser[S], function: S => T) extends Parser[T] {
      override val acceptsEmpty: Boolean = parser.acceptsEmpty
      override val first: TokenClassifier = parser.first
      override def parse(input: Input): ParseResult[T] = parser.parse(input).map(function)
    }

    case class Repeat[T](parser: Parser[T]) extends Parser[Stream[T]] {
      override val acceptsEmpty: Boolean = true
      override val first: TokenClassifier = parser.first
      override def parse(input: Input): ParseResult[Stream[T]] = {
        val previousIndex = input.currentIndex
        parser.parse(input) match {
          case Complete(value) =>
            if (input.currentIndex == previousIndex) Complete(Stream.continually(value))
            else parse(input).map(values => value +: values)
          case Incomplete(rest) =>
            if (input.currentIndex == previousIndex) Complete(Stream.Empty)
            else Incomplete(Sequence(rest, Repeat(parser)).map { case (value, values) => value +: values})
          case Failed(error) =>
            if (input.currentIndex == previousIndex) Complete(Stream.Empty)
            else Failed(error)
        }
      }
    }

    case class Explain[T](parser: Parser[T], error: Error) extends Parser[T] {
      override val acceptsEmpty: Boolean = parser.acceptsEmpty
      override val first: TokenClassifier = parser.first
      override def parse(input: Input): ParseResult[T] = {
        val previousIndex = input.currentIndex

        parser.parse(input) match {
          case Failed(_) if (input.currentIndex == previousIndex) => Failed(error)
          case result => result
        }
      }
    }

    case class Positioned[T](
        parser: Parser[T],
        optStartPos: Option[Position] = None,
        optStartIndex: Option[Int] = None) extends Parser[(T, Option[Range])] {
      
      override val acceptsEmpty: Boolean = parser.acceptsEmpty
      override val first: TokenClassifier = parser.first
      override def parse(input: Input): ParseResult[(T, Option[Range])] = {
        val startPosition =
          if (optStartPos.nonEmpty) optStartPos
          else if (!input.atEnd) Some(input.currentStart)
          else None
        val startIndex = optStartIndex.getOrElse(input.currentIndex)

        parser.parse(input) match {
          case Complete(value) => {
            val endIndex = input.currentIndex
            val optRange = if (endIndex > startIndex) {
              startPosition.map((_, input.previousEnd))
            } else {
              None
            }
            Complete((value, optRange))
          }
          case Incomplete(rest) => {
            Incomplete(Positioned(rest, optStartPos=startPosition, optStartIndex=Some(startIndex)))
          }
          case Failed(error) => Failed(error)
        }
      }
    }
  }

  import Combinators._

  def success[A](value: A): Parser[A] = Success(value)
  def fail(error: Error): Parser[Nothing] = Failure(error)
  def elem(token: Token, error: Error): Parser[Token] = Accept(TokenClassifier.Singleton(token), error)
  def member(tokenClassifier: TokenClassifier, error: Error): Parser[Token] = Accept(tokenClassifier, error)
  def accepts[A](error: Error, reprs: Seq[Character]*)(function: PartialFunction[Token, A]): Parser[A] =
    Accept(TokenClassifier(reprs: _*)(function.isDefinedAt(_)), error).map(function(_))

  def oneOf[A](error: Error)(parsers: Parser[A]*): Parser[A] = {
    val zero: Parser[A] = fail(error)
    parsers.foldRight(zero) {
      case (parser, rest) => parser | rest
    }
  }

  def many[A](parser: Parser[A]): Parser[Stream[A]] = Repeat(parser)
  def opt[A](parser: Parser[A]): Parser[Option[A]] = parser.map(Some(_)) | success(None)

  def end(error: Error): Parser[Unit] = elem(EndToken, error).map((_: Token) => ())

  def phrase[A](parser: Parser[A], error: Error): Parser[A] =
    parser << end(error)

  def repsep[A](rep: Parser[A], sep: Parser[Any]): Parser[Stream[A]] =
    (rep <> many(sep >> rep)).map {
      case (x, xs) => x +: xs
    }
}