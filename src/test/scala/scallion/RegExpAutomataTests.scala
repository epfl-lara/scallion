package scallion

import org.scalatest._

class RegExpAutomataTests extends FlatSpec with RegExps[Char] with CharRegExps with Compiled[Char] {

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

    for (c <- "2173") {
      assert(automaton.next(c) == (true, true))
    }

    automaton.reset()
  }
}