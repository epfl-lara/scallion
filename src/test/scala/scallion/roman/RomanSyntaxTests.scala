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

package scallion.roman

import org.scalatest._

class RomanSyntaxTests extends FlatSpec with Inside {

  import RomanSyntax._

  val parser = Parser(number)

  def tokenize(string: String): Iterator[Symbol] =
    string.map {
      case 'I' => I
      case 'V' => V
      case 'X' => X
      case 'L' => L
      case 'C' => C
      case 'D' => D
      case 'M' => M
    }.iterator

  def display(symbols: Seq[Symbol]): String =
    symbols.map(_.toString).mkString("")

  it should "be able to parse various examples" in {
    assert(parser(tokenize("")).getValue == Some(0))
    assert(parser(tokenize("I")).getValue == Some(1))
    assert(parser(tokenize("XII")).getValue == Some(12))
    assert(parser(tokenize("XCIII")).getValue == Some(93))
    assert(parser(tokenize("MCDXLIV")).getValue == Some(1444))
    assert(parser(tokenize("MMMD")).getValue == Some(3500))
    assert(parser(tokenize("XLVI")).getValue == Some(46))
    assert(parser(tokenize("MCDXCI")).getValue == Some(1491))
    assert(parser(tokenize("XVII")).getValue == Some(17))
    assert(parser(tokenize("MDCCCI")).getValue == Some(1801))
    assert(parser(tokenize("MV")).getValue == Some(1005))
    assert(parser(tokenize("CXVIII")).getValue == Some(118))
    assert(parser(tokenize("XXXIX")).getValue == Some(39))
  }

  // it should "be able to pretty print various examples" in {
  //   assert(display(number.unapply(232).next()) == "CCXXXII")
  //   assert(display(number.unapply(1337).next()) == "MCCCXXXVII")
  //   assert(display(number.unapply(14).next()) == "XIV")
  //   assert(display(number.unapply(3009).next()) == "MMMIX")
  // }

  // it should "support completions" in {
  //   val completedParsers = number(tokenize("I")).rest.toSyntax.completions(x => Seq(x)).toList

  //   val values = completedParsers.map(_.nullable.get).toSet

  //   assert(values == Set(1, 2, 3, 4, 9))

  //   val trails = completedParsers.flatMap(_.trails.toList).toSet

  //   assert(trails == Set(Seq(), Seq(I), Seq(I, I), Seq(V), Seq(X)))
  // }

  // "Pretty printing" should "lead to a single value for valid numbers" in {
  //   for (i <- 0 until 4000) {
  //     val xs = number.unapply(i).toList
  //     assert(xs.size == 1)
  //   }
  // }

  // it should "lead to no values for invalid members" in {
  //   assert(!number.unapply(-1).hasNext)
  //   assert(!number.unapply(4000).hasNext)
  //   assert(!number.unapply(-24241).hasNext)
  //   assert(!number.unapply(124214).hasNext)
  // }

  // "Parsing" should "be possible on all pretty printed values" in {
  //   for (i <- 0 until 4000) {
  //     number.unapply(i).map(_.iterator).foreach { symbols =>
  //       assert(number(symbols).getValue == Some(i))
  //     }
  //   }
  // }
}