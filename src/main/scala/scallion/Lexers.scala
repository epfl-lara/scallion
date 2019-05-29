package scallion

import scala.collection.mutable.ArrayBuffer

import scallion.util._

/** Contains definitions for lexers.
  *
  * @groupprio lexer 0
  * @groupname lexer Lexers
  *
  * @groupprio producer 1
  * @groupname producer Producers
  */
trait Lexers[Token, Character, Position] extends RegExps[Character] {

  //---- Producers ----//

  /** Associates a regular expression with a token generator.
    *
    * @group producer
    */
  case class Producer(regExp: RegExp, makeToken: (Seq[Character], (Position, Position)) => Token)

  /** Adds methods to build a`Producer` to a `RegExp`.
    *
    * @group producer
    */
  implicit class ProducerDecorator(regExp: RegExp) {

    /** Creates a `Producer`. */
    def |>(makeToken: (Seq[Character], (Position, Position)) => Token) =
      Producer(regExp, makeToken)

    /** Creates a `Producer`. */
    def |>(makeToken: Seq[Character] => Token) =
      Producer(regExp, (cs: Seq[Character], _: (Position, Position)) => makeToken(cs))

    /** Creates a `Producer`. */
    def |>(token: Token) =
      Producer(regExp, (_: Seq[Character], _: (Position, Position)) => token)
  }

  //---- Lexers ----//

  /** Tokenizes an input source with respect to a sequence of token producers.
    *
    * @group lexer
    */
  class Lexer(producers: List[Producer]) {

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

    /** Spawn a thread that immediately start producing tokens from the source. */
    def spawn(
        source: Source[Character, Position],
        errorToken: (Seq[Character], (Position, Position)) => Token,
        skipToken: Token => Boolean = (_: Token) => false,
        threshold: Int = 50): Iterator[Token] = {

      var buffer: Vector[Token] = Vector()

      val it = new BufferedIterator[Token]

      val thread = new Thread {
        override def run: Unit = {
          while (true) {
            tokenizeOne(source) match {
              case Some(token) => if (!skipToken(token)) {
                buffer = buffer :+ token

                if (buffer.size >= threshold) {
                  it.addAll(buffer)
                  buffer = Vector()
                }
              }
              case None => {
                val endPos = source.currentPosition
                val content = source.back()
                val startPos = source.currentPosition

                if (!source.atEnd) {
                  buffer = buffer :+ errorToken(content, (startPos, endPos))
                }

                if (buffer.nonEmpty) {
                  it.addAll(buffer)
                }

                it.end()

                return
              }
            }
          }
        }
      }

      thread.start()

      it
    }

    /** Tries to produce a single token from the source. */
    private def tokenizeOne(source: Source[Character, Position]): Option[Token] = {

      // The first producer that was successful on the longest subsequence so far.
      var lastSuccessfulProducer: Option[Producer] = None

      // The producers which can potentially produce
      // something depending on the next characters.
      var activeProducers: List[Producer] = producers

      // The sequence of characters that are consumed so far.
      var buffer: Vector[Character] = Vector()

      // The start position in the source.
      val startPos = source.currentPosition

      // The position after the consumed characters.
      var endPos = startPos

      // Loops as long as some producers can potentially produce,
      // and as long as the source is not empty.
      while (activeProducers.nonEmpty && !source.atEnd) {
        val char = source.ahead()  // Next character.

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
          endPos = source.currentPosition
          buffer ++= source.consume()  // Consumes all that was read so far.
          lastSuccessfulProducer = Some(prod)
        }
      }


      lastSuccessfulProducer.map {
        case Producer(_, makeToken) => {

          // Resets the looked-ahead pointer in the source.
          // Only done in case of a successful tokenization.
          source.back()

          val range = (startPos, endPos)
          val token = makeToken(buffer, range)

          token
        }
      }
    }
  }

  /** Contains utilities to build lexers.
    *
    * @group lexer
    */
  object Lexer {

    /** Creates a lexer for a sequence of producers.
      *
      * @param producers The producers, in decreasing priority.
      */
    def apply(producers: Producer*): Lexer = new Lexer(producers.toList)
  }
}