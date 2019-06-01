/* Copyright 2019 EPFL, Lausanne
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

package scallion
package lexing

import scala.collection.mutable.ArrayBuffer

import scallion.input.Source
import scallion.util.internal.BufferedIterator

/** Contains definitions for lexers.
  *
  * @groupprio lexer 0
  * @groupname lexer Lexers
  *
  * @groupprio producer 1
  * @groupname producer Producers
  */
trait Lexers[Token, Character, Position] extends RegExps[Character] with Automatons[Character] {

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
        private def fetchNext(): Unit = tokenizeOneAutomata(source) match {
          case Some(token) if skipToken(token) => fetchNext()
          case Some(token) => cacheNext = Some(token)
          case None => {
            ended = true

            val endPos = source.currentPosition
            val content = source.backContent()
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
            tokenizeOneAutomata(source) match {
              case Some(token) => if (!skipToken(token)) {
                buffer = buffer :+ token

                if (buffer.size >= threshold) {
                  it.addAll(buffer)
                  buffer = Vector()
                }
              }
              case None => {
                val endPos = source.currentPosition
                val content = source.backContent()
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

    // The makeToken functions, as an array for fast access.
    private val makeTokens = producers.map(_.makeToken).toArray

    // The automata corresponding to the regular expressions, as an array for fast access.
    private val dfas = producers.map(p => DFA(NFA(p.regExp))).toArray

    // The lists of start positions and indices. A list for fast flatMap.
    private val starts = List.tabulate(dfas.size)(n => (0, n))

    /** Tries to produce a single token from the source. Uses automata. */
    private def tokenizeOneAutomata(source: Source[Character, Position]): Option[Token] = {

      // The state and index of active automata.
      var states = starts

      // The buffer containing successfully consumed input.
      val buffer: ArrayBuffer[Character] = new ArrayBuffer()

      // Start position of the consumed input.
      val startPos = source.currentPosition

      // End position of the consumed input.
      var endPos = startPos

      // Index of the last successful state machine.
      var successful: Option[Int] = None

      while (states.nonEmpty && !source.atEnd) {
        val char = source.ahead()

        // Indicates if an automaton was accepting
        // for this character already.
        var accepted = false

        // Updates the states of all automata and
        // filter out the ones which can no longer
        // reach an accepting state.
        states = states.flatMap {
          case (current, index) => {
            val dfa = dfas(index)
            val next = dfa(current, char)

            // Also records the first accepting automaton.
            if (!accepted && dfa.isAccepting(next)) {
              endPos = source.currentPosition
              buffer ++= source.consume()
              successful = Some(index)
              accepted = true
            }

            if (dfa.isLive(next)) {
              (next, index) :: Nil
            }
            else {
              Nil
            }
          }
        }
      }

      // Creates the token, if any.
      successful.map { (index: Int) =>
        // Resets the looked-ahead pointer in the source.
        // Only done in case of a successful tokenization.
        source.back()

        val range = (startPos, endPos)
        val token = makeTokens(index)(buffer, range)

        token
      }
    }

    /** Tries to produce a single token from the source. Uses regular expression derivation. */
    private def tokenizeOneDerivation(source: Source[Character, Position]): Option[Token] = {

      // The first producer that was successful on the longest subsequence so far.
      var lastSuccessfulProducer: Option[Producer] = None

      // The producers which can potentially produce
      // something depending on the next characters.
      var activeProducers: List[Producer] = producers

      // The sequence of characters that are consumed so far.
      val buffer: ArrayBuffer[Character] = new ArrayBuffer()

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
            case RegExp.EmptySet => Nil
            // Otherwise, we update it.
            case derived => Producer(derived, makeToken) :: Nil
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