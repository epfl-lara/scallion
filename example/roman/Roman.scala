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

import scallion.syntactic._

sealed trait Symbol
case object I extends Symbol
case object V extends Symbol
case object X extends Symbol
case object L extends Symbol
case object C extends Symbol
case object D extends Symbol
case object M extends Symbol

object RomanSyntax extends Syntaxes[Symbol, Symbol] {

  override def getKind(token: Symbol): Symbol = token

  val stop: Parser[Seq[Symbol], Seq[Symbol]] = epsilon(Seq())

  def base(si: Symbol, sv: Symbol, sx: Symbol): Parser[Int, Int] = {

    def f(s: Symbol) = elem(s)

    val i = f(si)
    val v = f(sv)
    val x = f(sx)

    val fromI = (i +: (i +: (i +: stop | stop) | v +: stop | x +: stop | stop)).map {
      ss => ss.map {
        case `si` => 1
        case `sv` => 3
        case `sx` => 8
        case _ => 0
      }.sum
    }
    val fromV = (v +: (i +: (i +: (i +: stop | stop) | stop) | stop)).map {
      ss => ss.map {
        case `si` => 1
        case `sv` => 5
        case _ => 0
      }.sum
    }

    (fromI | fromV | stop.map(_ => 0)).contramap {
      case 9 => Seq(Seq(si, sx))
      case 8 => Seq(Seq(sv, si, si, si))
      case 7 => Seq(Seq(sv, si, si))
      case 6 => Seq(Seq(sv, si))
      case 5 => Seq(Seq(sv))
      case 4 => Seq(Seq(si, sv))
      case 3 => Seq(Seq(si, si, si))
      case 2 => Seq(Seq(si, si))
      case 1 => Seq(Seq(si))
      case 0 => Seq(Seq())
      case _ => Seq()
    }
  }

  val units: Parser[Int, Int] = base(I, V, X)
  val tens: Parser[Int, Int] = base(X, L, C)
  val hundreds: Parser[Int, Int] = base(C, D, M)
  val thousands: Parser[Int, Int] =
    (elem(M) +: (elem(M) +: (elem(M) +: stop | stop) | stop) | stop).map {
      case xs => xs.size
    }.contramap {
      case k if (k >= 0 && k <= 3) => Seq(Seq.fill(k)(M))
    }

  val number: Parser[Int, Int] = (thousands ~ hundreds ~ tens ~ units).map {
    case ths ~ hus ~ tes ~ uns => ths * 1000 + hus * 100 + tes * 10 + uns
  }.contramap {
    case n if n >= 0 => {
      val uns = (n / 1) % 10
      val tes = (n / 10) % 10
      val hus = (n / 100) % 10
      val ths = (n / 1000)

      Seq(ths ~ hus ~ tes ~ uns)
    }
    case _ => Seq()
  }
}