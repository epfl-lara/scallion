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

import org.scalatest._

import scallion.lexical._

class RegExpAutomataTests extends FlatSpec with RegExps[Char] with CharRegExps with Automatons[Char] {

  // Automaton with mutable current state for easier tests.
  class Automaton(regExp: RegExp) {
    val dfa = DFA(NFA(regExp))
    var current = dfa.start

    def next(character: Character): (Boolean, Boolean) = {
      current = dfa(current, character)
      (dfa.isAccepting(current), dfa.isLive(current))
    }

    def reset(): Unit = current = dfa.start
  }

  def compile(regExp: RegExp): Automaton = new Automaton(regExp)

  import RegExp._

  "Automaton" should "work for empty set" in {
    val automaton = compile(EmptySet)
    assert(automaton.next('A') == (false, false))
  }

  it should "work for empty string" in {
    val automaton = compile(EmptyStr)
    assert(automaton.next('A') == (false, false))
  }

  it should "work for single matching elements" in {
    val automaton = compile(elem('A'))
    assert(automaton.next('A') == (true, false))
  }

  it should "work for single non-matching elements" in {
    val automaton = compile(elem(_.isDigit))
    assert(automaton.next('A') == (false, false))
  }

  it should "support resets" in {
    val automaton = compile(elem(_.isDigit))
    assert(automaton.next('A') == (false, false))

    automaton.reset()

    assert(automaton.next('0') == (true, false))
    assert(automaton.next('1') == (false, false))

    automaton.reset()

    assert(automaton.next('1') == (true, false))
  }

  it should "work for sequences" in {
    val automaton = compile(word("ABC"))

    assert(automaton.next('A') == (false, true))
    assert(automaton.next('B') == (false, true))
    assert(automaton.next('C') == (true, false))

    automaton.reset()

    assert(automaton.next('A') == (false, true))
    assert(automaton.next('X') == (false, false))
    assert(automaton.next('C') == (false, false))
  }

  it should "work for disjunctions" in {
    val automaton = compile(oneOf("ABC"))

    assert(automaton.next('A') == (true, false))
    assert(automaton.next('B') == (false, false))

    automaton.reset()

    assert(automaton.next('B') == (true, false))

    automaton.reset()

    assert(automaton.next('X') == (false, false))
  }

  it should "work for repetitions" in {
    val automaton = compile(many(oneOf("ABC")))

    assert(automaton.next('A') == (true, true))
    assert(automaton.next('B') == (true, true))

    automaton.reset()

    assert(automaton.next('B') == (true, true))
    assert(automaton.next('X') == (false, false))

    automaton.reset()

    assert(automaton.next('X') == (false, false))
  }

  it should "work for complex expressions" in {
    val automaton = compile {
      opt {
        elem('-')
      } ~
      {
        elem('0') |
        nonZero ~ many(digit)
      } ~
      opt {
        elem('.') ~ many1(digit)
      } ~
      opt {
        oneOf("eE") ~
        opt(oneOf("+-")) ~
        many1(digit)
      }
    }

    assert(automaton.next('+') == (false, false))
    assert(automaton.next('0') == (false, false))

    automaton.reset()

    assert(automaton.next('0') == (true, true))
    assert(automaton.next('1') == (false, false))

    automaton.reset()

    for (c <- "2123") {
      assert(automaton.next(c) == (true, true))
    }
    for (c <- ".") {
      assert(automaton.next(c) == (false, true))
    }
    for (c <- "2313") {
      assert(automaton.next(c) == (true, true))
    }
    for (c <- "e-") {
      assert(automaton.next(c) == (false, true))
    }
    for (c <- "324") {
      assert(automaton.next(c) == (true, true))
    }

    assert(automaton.next('-') == (false, false))

    automaton.reset()

    assert(automaton.next('-') == (false, true))
    for (c <- "32") {
      assert(automaton.next(c) == (true, true))
    }
    for (c <- "E+") {
      assert(automaton.next(c) == (false, true))
    }
    for (c <- "324") {
      assert(automaton.next(c) == (true, true))
    }

    assert(automaton.next('+') == (false, false))

    automaton.reset()

     for (c <- "0") {
      assert(automaton.next(c) == (true, true))
    }
    for (c <- "e") {
      assert(automaton.next(c) == (false, true))
    }
    for (c <- "324") {
      assert(automaton.next(c) == (true, true))
    }

    assert(automaton.next('+') == (false, false))
  }
}