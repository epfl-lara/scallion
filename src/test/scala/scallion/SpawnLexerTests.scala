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

import scallion.input._
import scallion.lexing._

class SpawnLexerTests extends FlatSpec with Lexers[String, Char, (Int, Int)] with CharRegExps {

  object PairPositioner extends Positioner[Char, (Int, Int)] {
    override val start = (0, 0)
    override def increment(pos: (Int, Int), char: Char): (Int, Int) =
      if (char == '\n') (pos._1 + 1, 0) else (pos._1, pos._2 + 1)
  }

  def source(text: String): Source[Char, (Int, Int)] = Source.fromString(text, PairPositioner)

  "Lexer.spawn" should "be able to produce a single token" in {
    val lexer =
      Lexer(many1(digit) |> { cs => cs.mkString }) onError {
        (cs, _) => "<error>"
      }

    assert(lexer.spawn(source("1234")).toList == List("1234"))
  }

  it should "be able to produce a single token multiple times" in {
    val lexer =
      Lexer(digit.times(2) |> { cs => cs.mkString }) onError {
        (cs, _) => "<error>"
      }

    assert(lexer.spawn(source("123456")).toList == List("12", "34", "56"))
  }

  it should "handle nullable regular expressions" in {
    val lexer =
      Lexer(many(digit) |> { cs => cs.mkString }) onError {
        (cs, _) => "<error>"
      }

    assert(lexer.spawn(source("ABCDEF")).toList == List("<error>"))
    assert(lexer.spawn(source("1234")).toList == List("1234"))
  }

  it should "accept the longest match" in {
    val lexer =
      Lexer(many(digit | elem('X')) |> { cs => "N:" + cs.mkString },
            many(hex) |> { cs => "H:" + cs.mkString }) onError {
        (cs, _) => "<error>"
      }

    assert(lexer.spawn(source("123A4X56XF")).toList == List("H:123A4", "N:X56X", "H:F"))
  }

  it should "correctly prioritize tokens" in {
    val lexer =
      Lexer(many(digit) ~ elem('Z') |> { cs => "N:" + cs.mkString },
            many(hex) ~ elem('Z') |> { cs => "H:" + cs.mkString }) onError {
        (cs, _) => "<error>"
      }

    assert(lexer.spawn(source("12Z34ZZ1AEZ")).toList == List("N:12Z", "N:34Z", "N:Z", "H:1AEZ"))
  }

  it should "produce an error token in case of no match" in {
    val lexer =
      Lexer(digit.times(2) |> { cs => cs.mkString },
            oneOf("ABCDEF").times(3) |> { cs => cs.mkString }) onError {
        (cs, _) => "error:" + cs.mkString
      }

    assert(lexer.spawn(source("12AAX12")).toList == List("12", "error:AAX"))
  }

  it should "be able to continue after errors if specified" in {
    val lexer =
      Lexer(digit.times(2) |> { cs => cs.mkString },
            oneOf("ABCDEF").times(3) |> { cs => cs.mkString }) onError {
        (cs, _) => "error:" + cs.mkString
      }

    assert(lexer.spawn(source("12AAX12"), false).toList == List("12", "error:AAX", "12"))
  }

  it should "produce end tokens" in {
    val lexer =
      Lexer(digit.times(2) |> { cs => cs.mkString },
            oneOf("ABCDEF").times(3) |> { cs => cs.mkString }) onError {
        (cs, _) => "error:" + cs.mkString
      } onEnd {
        _ => "eof"
      }

    assert(lexer.spawn(source("12AAB12")).toList == List("12", "AAB", "12", "eof"))
    assert(lexer.spawn(source("12AAX12"), true).toList == List("12", "error:AAX"))
    assert(lexer.spawn(source("12AAX12"), false).toList == List("12", "error:AAX", "12", "eof"))
  }

  it should "correctly produce positions" in {
    val lexer =
      Lexer(many(digit) |> { (cs, r) => "N:" + r._1 + "-" + r._2 },
            many(hex) |> { (cs, r) => "H:" + r._1 + "-" + r._2 },
            many(whiteSpace) |> { (cs, r) => "S:" + r._1 + "-" + r._2 }) onError {
        (cs, r) => "E:" + r._1 + "-" + r._2
      }

    assert(lexer.spawn(source("1234 ABC\n  \n42?")).toList ==
      List("N:(0,0)-(0,4)", "S:(0,4)-(0,5)", "H:(0,5)-(0,8)", "S:(0,8)-(2,0)", "N:(2,0)-(2,2)", "E:(2,2)-(2,3)"))
  }
}