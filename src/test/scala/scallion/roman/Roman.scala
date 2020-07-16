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

import scallion._

sealed trait Symbol
case object I extends Symbol
case object V extends Symbol
case object X extends Symbol
case object L extends Symbol
case object C extends Symbol
case object D extends Symbol
case object M extends Symbol

object RomanSyntax extends Parsers {

  type Token = Symbol
  type Kind = Symbol

  import SafeImplicits._

  override def getKind(token: Symbol): Symbol = token

  def base(si: Symbol, sv: Symbol, sx: Symbol): Syntax[Int] = {

    val i = elem(si).unit(si).skip
    val v = elem(sv).unit(sv).skip
    val x = elem(sx).unit(sx).skip

    epsilon(0) |
    i ~ {
      epsilon(1) |
      i ~ {
        epsilon(2) |
        i ~ epsilon(3)
      } |
      v ~ epsilon(4) |
      x ~ epsilon(9)
    } |
    v ~ {
      epsilon(5) |
      i ~ {
        epsilon(6) |
        i ~ {
          epsilon(7) |
          i ~ epsilon(8)
        }
      }
    }
  }

  val units = base(I, V, X)
  val tens = base(X, L, C)
  val hundreds = base(C, D, M)
  val thousands: Syntax[Int] = {
    val m = elem(M).unit(M).skip

    epsilon(0) |
    m ~ {
      epsilon(1) |
      m ~ {
        epsilon(2) |
        m ~ epsilon(3)
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
}