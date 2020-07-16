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

package example.roman

import scallion._

/* In this example, we show a parser and pretty printer for roman numerals. */

// Symbols used by roman numerals.
sealed trait Symbol
case object I extends Symbol
case object V extends Symbol
case object X extends Symbol
case object L extends Symbol
case object C extends Symbol
case object D extends Symbol
case object M extends Symbol

// The following describes the syntax of roman numerals.
object RomanSyntax extends Parsers {

  // Tokens and kinds coincide in this example.
  type Token = Symbol
  type Kind = Symbol

  import SafeImplicits._

  override def getKind(token: Symbol): Symbol = token

  // Describes the syntax for values from 0 to 9 given symbol for
  // 1, 5, and 10. Works for units, tens and hundreds.
  def base(si: Symbol, sv: Symbol, sx: Symbol): Syntax[Int] = {

    val i = elem(si).unit(si)
    val v = elem(sv).unit(sv)
    val x = elem(sx).unit(sx)

    epsilon(0) |
    i ~>~ {
      epsilon(1) |
      i ~>~ {
        epsilon(2) |
        i ~>~ epsilon(3)
      } |
      v ~>~ epsilon(4) |
      x ~>~ epsilon(9)
    } |
    v ~>~ {
      epsilon(5) |
      i ~>~ {
        epsilon(6) |
        i ~>~ {
          epsilon(7) |
          i ~>~ epsilon(8)
        }
      }
    }
  }

  val units = base(I, V, X)
  val tens = base(X, L, C)
  val hundreds = base(C, D, M)
  val thousands: Syntax[Int] = {
    val m = elem(M).unit(M)

    epsilon(0) |
    m ~>~ {
      epsilon(1) |
      m ~>~ {
        epsilon(2) |
        m ~>~ epsilon(3)
      }
    }
  }

  val number: Syntax[Int] = (thousands ~ hundreds ~ tens ~ units).map({
    case ths ~ hus ~ tes ~ uns => ths * 1000 + hus * 100 + tes * 10 + uns
  }, {
    case n if n >= 0 => {
      val uns = (n / 1) % 10
      val tes = (n / 10) % 10
      val hus = (n / 100) % 10
      val ths = (n / 1000)

      Seq(ths ~ hus ~ tes ~ uns)
    }
    case _ => Seq()
  })

  val printer = PrettyPrinter(number)
  val parser = Parser(number)
}

object Roman {
  def main(args: Array[String]) {
    println("Parsing roman numerals: ")
    println("CXXXII => " + RomanSyntax.parser(Iterator(C, X, X, X, I, I)).getValue.get)
    println("MCDL => " + RomanSyntax.parser(Iterator(M, C, D, L)).getValue.get)
    println("Printing roman numerals: ")
    println("1234 => " + RomanSyntax.printer(1234).next().mkString(""))
    println("2020 => " + RomanSyntax.printer(2020).next().mkString(""))
    println("1515 => " + RomanSyntax.printer(1515).next().mkString(""))
  }
}