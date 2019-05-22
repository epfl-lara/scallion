package scallion

import scala.collection.mutable.ArrayBuffer

/** Contains definitions for lexers. */
trait Lexers[Token, Character, Position] extends RegExps[Character] {

  //---- Lexer ----//

  /** Associates a regular expression with a token generator. */
  case class Producer(regExp: RegExp, makeToken: (Seq[Character], (Position, Position)) => Token)

  // Notation for writing producers.
  case object |> {
    def unapply(arg: Producer) = Producer.unapply(arg)
  }

  implicit class ProducerDecorator(regExp: RegExp) {
    def |>(makeToken: (Seq[Character], (Position, Position)) => Token) =
      Producer(regExp, makeToken)
    def |>(makeToken: Seq[Character] => Token) =
      Producer(regExp, (cs: Seq[Character], _: (Position, Position)) => makeToken(cs))
    def |>(token: Token) =
      Producer(regExp, (_: Seq[Character], _: (Position, Position)) => token)
  }


  /** Tokenizes an input source with respect to a sequence of token producers. */
  class Lexer(producers: Seq[Producer]) {

    /** Returns an iterator that produces tokens from the source. */
    def apply(
        source: Source[Character, Position],
        errorToken: (Seq[Character], (Position, Position)) => Token,
        skipToken: Token => Boolean = (_: Token) => false): Iterator[Token] =

      new Iterator[Token] {

        /** Indicates if the source has ended. */
        private var ended: Boolean = false

        /** Cache for the next. Computed by `hasNext()` or `next()`. */
        private var cacheNext: Option[Token] = None

        /** Queries the source for the next token and update the state. */
        private def fetchNext(): Unit = tokenizeOne(source) match {
          case Some(token) if skipToken(token) => fetchNext()
          case Some(token) => cacheNext = Some(token)
          case None => {
            ended = true

            val endPos = source.currentPosition
            val content = source.back()
            val startPos = source.currentPosition

            if (source.atEnd) {
              cacheNext = None
            } else {
              cacheNext = Some(errorToken(content, (startPos, endPos)))
            }
          }
        }

        override def hasNext(): Boolean = cacheNext match {
          case Some(_) => true
          case None => if (ended) false else {
            fetchNext()
            hasNext()
          }
        }

        override def next(): Token = cacheNext match {
          case Some(token) => {
            cacheNext = None
            token
          }
          case None if ended => throw new NoSuchElementException("Token iterator ended.")
          case None => {
            fetchNext()
            next()
          }
        }
      }

    /** Tries to produce a single token from the source. */
    private def tokenizeOne(source: Source[Character, Position]): Option[Token] = {

      // The first producer that was successful on the longest subsequence so far.
      var lastSuccessfulProducer: Option[Producer] = None

      // The producers which can potentially produce
      // something depending on the next characters.
      var activeProducers: Seq[Producer] = producers

      // The sequence of characters that are consumed so far.
      val buffer: ArrayBuffer[Character] = new ArrayBuffer[Character]()

      // The start position in the source.
      val startPos = source.currentPosition

      // The position after the consumed characters.
      var endPos = startPos

      // The position after the consumed and looked-ahead characters.
      var currentPos = startPos

      // Loops as long as some producers can potentially produce,
      // and as long as the source is not empty.
      while (activeProducers.nonEmpty && !source.atEnd) {
        val char = source.ahead()  // Next character.
        currentPos = source.currentPosition

        // Feeds the character to all regexps,
        // resulting in new regexps that handle the rest of input.
        activeProducers = activeProducers.flatMap {
          case Producer(regExp, makeToken) => regExp.derive(char) match {
            // When the regExp is EmptySet, we can ignore the producer.
            case RegExp.EmptySet => None
            // Otherwise, we update it.
            case derived => Some(Producer(derived, makeToken))
          }
        }

        // Finds the first producer that accepts the entire subsequence, if any.
        activeProducers.find(_.regExp.acceptsEmpty).foreach { (prod: Producer) =>
          buffer.appendAll(source.consume())  // Consumes all that was read so far.
          endPos = currentPos
          lastSuccessfulProducer = Some(prod)
        }
      }


      lastSuccessfulProducer.map {
        case Producer(_, makeToken) => {

          // Resets the looked-ahead pointer in the source.
          // Only done in case of a successful tokenization.
          source.back()

          val range = (startPos, endPos)
          val token = makeToken(buffer.toSeq, range)

          token
        }
      }
    }
  }

  object Lexer {

    /** Creates a lexer for a sequence of producers.
      *
      * @param producers The producers, in decreasing priority.
      */
    def apply(producers: Producer*): Lexer = new Lexer(producers)
  }
}