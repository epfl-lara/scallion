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

import scallion.lexing._

class RegExpsTests extends FlatSpec with RegExps[Char] with CharRegExps {

  "elem (predicate)" should "not accept empty" in {
    val regexp = elem(_.isDigit)
    assert(!regexp.acceptsEmpty)
  }

  it should "accept characters specified" in {
    val regexp = elem(_.isDigit)
    assert(regexp.derive('0').acceptsEmpty)
  }

  it should "reject other characters" in {
    val regexp = elem(_.isDigit)
    assert(!regexp.derive('A').acceptsEmpty)
  }

  it should "only accept a single repetition" in {
    val regexp = elem(_.isDigit)
    assert(!regexp.derive('0').derive('0').acceptsEmpty)
  }



  "elem (value)" should "not accept empty" in {
    val regexp = elem('1')
    assert(!regexp.acceptsEmpty)
  }

  it should "accept the given character" in {
    val regexp = elem('1')
    assert(regexp.derive('1').acceptsEmpty)
  }

  it should "reject other characters" in {
    val regexp = elem('1')
    assert(!regexp.derive('A').acceptsEmpty)
  }

  it should "only accept a single repetition" in {
    val regexp = elem('1')
    assert(!regexp.derive('1').derive('1').acceptsEmpty)
  }



  "a | b" should "accept empty if a accepts empty" in {
    val regexp = opt(elem('a')) | elem('b')
    assert(regexp.acceptsEmpty)
  }

  it should "accept empty if b accepts empty" in {
    val regexp = elem('a') | opt(elem('b'))
    assert(regexp.acceptsEmpty)
  }

  it should "accept empty when both a and b accept empty" in {
    val regexp = opt(elem('a')) | opt(elem('b'))
    assert(regexp.acceptsEmpty)
  }

  it should "not accept empty when both a and b do not accept empty" in {
    val regexp = elem('a') | elem('b')
    assert(!regexp.acceptsEmpty)
  }

  it should "accept from a" in {
    val regexp = word("yes") | word("oui")
    assert(regexp.derive('y').derive('e').derive('s').acceptsEmpty)
  }

  it should "accept from b" in {
    val regexp = word("yes") | word("oui")
    assert(regexp.derive('o').derive('u').derive('i').acceptsEmpty)
  }

  it should "reject otherwise" in {
    val regexp = word("yes") | word("oui")
    assert(!regexp.derive('y').derive('e').acceptsEmpty)
    assert(!regexp.derive('o').derive('e').derive('i').acceptsEmpty)
    assert(!regexp.derive('o').derive('u').derive('s').acceptsEmpty)
  }



  "a ~ b" should "accept empty if both a and b accept empty" in {
    val regexp = opt(elem('a')) ~ opt(elem('b'))
    assert(regexp.acceptsEmpty)
  }

  it should "not accept empty otherwise" in {
    assert(!(elem('a') ~ opt(elem('b'))).acceptsEmpty)
    assert(!(opt(elem('a')) ~ elem('b')).acceptsEmpty)
    assert(!(elem('a') ~ elem('b')).acceptsEmpty)
  }

  it should "accept a followed by b" in {
    val regexp = (word("hel") | word("ve")) ~ word("lo")
    assert(regexp.derive('h').derive('e').derive('l').derive('l').derive('o').acceptsEmpty)
    assert(regexp.derive('v').derive('e').derive('l').derive('o').acceptsEmpty)
  }

  it should "reject otherwise" in {
    val regexp = (word("hel") | word("ve")) ~ word("lo")
    assert(!regexp.derive('h').derive('e').derive('l').acceptsEmpty)
    assert(!regexp.derive('v').derive('e').acceptsEmpty)
    assert(!regexp.derive('l').derive('o').acceptsEmpty)
    assert(!regexp.derive('f').derive('o').derive('o').acceptsEmpty)
  }



  "oneOf" should "not accept empty" in {
    val regexp = oneOf("ABCXYZ")
    assert(!regexp.acceptsEmpty)
  }

  it should "accept any of the given characters" in {
    val regexp = oneOf("ABCXYZ")
    assert(regexp.derive('A').acceptsEmpty)
    assert(regexp.derive('B').acceptsEmpty)
    assert(regexp.derive('C').acceptsEmpty)
    assert(regexp.derive('X').acceptsEmpty)
    assert(regexp.derive('Y').acceptsEmpty)
    assert(regexp.derive('Z').acceptsEmpty)
  }

  it should "reject other characters" in {
    val regexp = oneOf("ABCXYZ")
    assert(!regexp.derive('D').acceptsEmpty)
    assert(!regexp.derive('W').acceptsEmpty)
    assert(!regexp.derive('0').acceptsEmpty)
  }

  it should "only accept a single repetition" in {
    val regexp = oneOf("ABCXYZ")
    assert(!regexp.derive('A').derive('A').acceptsEmpty)
  }



  "word" should "accept empty if the word is empty" in {
    val regexp = word("")
    assert(regexp.acceptsEmpty)
  }

  it should "not accept empty for non-empty words" in {
    assert(!word("foo").acceptsEmpty)
    assert(!word("a").acceptsEmpty)
    assert(!word("1234").acceptsEmpty)
  }

  it should "match against the exact sequence" in {
    assert(word("hello").derive('h').derive('e').derive('l').derive('l').derive('o').acceptsEmpty)
  }

  it should "reject other sequences" in {
    assert(!word("hello").derive('h').derive('a').derive('l').derive('l').derive('o').acceptsEmpty)
    assert(!word("hello").derive('h').derive('e').acceptsEmpty)
    assert(!word("hello").derive('l').derive('l').derive('o').acceptsEmpty)
    assert(!word("hello").derive('h').derive('e').derive('l').derive('o').acceptsEmpty)
  }



  "many" should "accept empty" in {
    val regexp = many(elem(_ => false))
    assert(regexp.acceptsEmpty)
  }

  it should "match against any number of repetitions" in {
    val regexp = many(elem('X'))
    assert(regexp.acceptsEmpty)
    assert(regexp.derive('X').acceptsEmpty)
    assert(regexp.derive('X').derive('X').acceptsEmpty)
    assert(regexp.derive('X').derive('X').derive('X').derive('X').derive('X').acceptsEmpty)
  }

  it should "reject other sequences" in {
    val regexp = many(elem('X'))
    assert(!regexp.derive('Y').acceptsEmpty)
    assert(!regexp.derive('X').derive('X').derive('Y').acceptsEmpty)
    assert(!regexp.derive('Y').derive('X').derive('X').derive('X').derive('X').acceptsEmpty)
    assert(!regexp.derive('X').derive('X').derive('Y').derive('X').derive('X').acceptsEmpty)
  }

  it should "not fix the choice" in {
    val regexp = many(elem('X') | elem('Y') | elem('Z'))
    assert(regexp.derive('X').derive('Y').acceptsEmpty)
    assert(regexp.derive('Y').derive('Z').acceptsEmpty)
    assert(regexp.derive('X').derive('Y').derive('Z').derive('X').derive('X').acceptsEmpty)
  }



  "many1" should "not accept empty" in {
    val regexp = many1(elem(_ => true))
    assert(!regexp.acceptsEmpty)
  }

  it should "match against any non-zero number of repetitions" in {
    val regexp = many1(elem('X'))
    assert(regexp.derive('X').acceptsEmpty)
    assert(regexp.derive('X').derive('X').acceptsEmpty)
    assert(regexp.derive('X').derive('X').derive('X').derive('X').derive('X').acceptsEmpty)
  }

  it should "reject other sequences" in {
    val regexp = many1(elem('X'))
    assert(!regexp.derive('Y').acceptsEmpty)
    assert(!regexp.derive('X').derive('X').derive('Y').acceptsEmpty)
    assert(!regexp.derive('Y').derive('X').derive('X').derive('X').derive('X').acceptsEmpty)
    assert(!regexp.derive('X').derive('X').derive('Y').derive('X').derive('X').acceptsEmpty)
  }



  "opt" should "accept empty" in {
    val regexp = opt(word("ok"))
    assert(regexp.acceptsEmpty)
  }

  it should "also accept the original sequences" in {
    val regexp = opt(word("ok"))
    assert(regexp.derive('o').derive('k').acceptsEmpty)
  }

  it should "not otherwise affect the original" in {
    val regexp = opt(word("ok"))
    assert(!regexp.derive('o').acceptsEmpty)
    assert(!regexp.derive('k').acceptsEmpty)
    assert(!regexp.derive('o').derive('o').derive('k').acceptsEmpty)
  }



  "times" should "accept the specified number of repetitions" in {
    assert(elem('A').times(0).acceptsEmpty)
    assert(elem('A').times(1).derive('A').acceptsEmpty)
    assert(elem('A').times(3).derive('A').derive('A').derive('A').acceptsEmpty)
  }

  it should "also not accept otherwise" in {
    val regexp = elem('A').times(2)
    assert(!regexp.derive('o').derive('k').acceptsEmpty)
    assert(!regexp.derive('A').acceptsEmpty)
    assert(!regexp.derive('A').derive('A').derive('A').acceptsEmpty)
  }

  it should "not fix the choice" in {
    val regexp = (elem('X') | elem('Y') | elem('Z')).times(2)
    assert(regexp.derive('X').derive('Y').acceptsEmpty)
    assert(regexp.derive('Y').derive('Z').acceptsEmpty)
  }


  "digit" should "accept single digits" in {
    assert(digit.derive('0').acceptsEmpty)
    assert(digit.derive('1').acceptsEmpty)
    assert(digit.derive('2').acceptsEmpty)
    assert(digit.derive('3').acceptsEmpty)
    assert(digit.derive('4').acceptsEmpty)
    assert(digit.derive('5').acceptsEmpty)
    assert(digit.derive('6').acceptsEmpty)
    assert(digit.derive('7').acceptsEmpty)
    assert(digit.derive('8').acceptsEmpty)
    assert(digit.derive('9').acceptsEmpty)
  }

  it should "not accept more than one digit" in {
    assert(!digit.derive('0').derive('0').acceptsEmpty)
    assert(!digit.derive('3').derive('4').derive('5').acceptsEmpty)
  }

  it should "reject other sequences" in {
    assert(!digit.acceptsEmpty)
    assert(!digit.derive('A').acceptsEmpty)
    assert(!digit.derive('1').derive('X').acceptsEmpty)
    assert(!digit.derive('Y').derive('2').acceptsEmpty)
  }



  "nonZero" should "accept single non-zero digits" in {
    assert(nonZero.derive('1').acceptsEmpty)
    assert(nonZero.derive('2').acceptsEmpty)
    assert(nonZero.derive('3').acceptsEmpty)
    assert(nonZero.derive('4').acceptsEmpty)
    assert(nonZero.derive('5').acceptsEmpty)
    assert(nonZero.derive('6').acceptsEmpty)
    assert(nonZero.derive('7').acceptsEmpty)
    assert(nonZero.derive('8').acceptsEmpty)
    assert(nonZero.derive('9').acceptsEmpty)
  }

  it should "reject zero" in {
    assert(!nonZero.derive('0').acceptsEmpty)
  }

  it should "not accept more than one digit" in {
    assert(!nonZero.derive('1').derive('1').acceptsEmpty)
    assert(!nonZero.derive('3').derive('4').derive('5').acceptsEmpty)
  }

  it should "reject other sequences" in {
    assert(!nonZero.acceptsEmpty)
    assert(!nonZero.derive('A').acceptsEmpty)
    assert(!nonZero.derive('1').derive('0').acceptsEmpty)
    assert(!nonZero.derive('0').derive('2').acceptsEmpty)
  }



  "hex" should "accept single digits" in {
    assert(hex.derive('0').acceptsEmpty)
    assert(hex.derive('1').acceptsEmpty)
    assert(hex.derive('2').acceptsEmpty)
    assert(hex.derive('3').acceptsEmpty)
    assert(hex.derive('4').acceptsEmpty)
    assert(hex.derive('5').acceptsEmpty)
    assert(hex.derive('6').acceptsEmpty)
    assert(hex.derive('7').acceptsEmpty)
    assert(hex.derive('8').acceptsEmpty)
    assert(hex.derive('9').acceptsEmpty)
  }

  it should "accept a - f" in {
    assert(hex.derive('a').acceptsEmpty)
    assert(hex.derive('b').acceptsEmpty)
    assert(hex.derive('c').acceptsEmpty)
    assert(hex.derive('d').acceptsEmpty)
    assert(hex.derive('e').acceptsEmpty)
    assert(hex.derive('f').acceptsEmpty)
  }

  it should "accept A - F" in {
    assert(hex.derive('A').acceptsEmpty)
    assert(hex.derive('B').acceptsEmpty)
    assert(hex.derive('C').acceptsEmpty)
    assert(hex.derive('D').acceptsEmpty)
    assert(hex.derive('E').acceptsEmpty)
    assert(hex.derive('F').acceptsEmpty)
  }

  it should "not accept more than one digit" in {
    assert(!hex.derive('1').derive('1').acceptsEmpty)
    assert(!hex.derive('3').derive('A').derive('5').acceptsEmpty)
  }

  it should "reject other sequences" in {
    assert(!hex.acceptsEmpty)
    assert(!hex.derive('G').acceptsEmpty)
    assert(!hex.derive('g').acceptsEmpty)
    assert(!hex.derive('1').derive('z').acceptsEmpty)
    assert(!hex.derive('K').derive('2').acceptsEmpty)
  }



  "whiteSpace" should "accept single space" in {
    assert(whiteSpace.derive(' ').acceptsEmpty)
  }

  it should "accept single tab" in {
    assert(whiteSpace.derive('\t').acceptsEmpty)
  }

  it should "accept single new line" in {
    assert(whiteSpace.derive('\n').acceptsEmpty)
  }

  it should "accept carriage return" in {
    assert(whiteSpace.derive('\r').acceptsEmpty)
  }
}