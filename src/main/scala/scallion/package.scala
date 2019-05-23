
/** Scala LL(1) parsers. */
package object scallion {

  /** Simply a pair.
    *
    * Can be used in infix position in pattern matching.
    */ 
  case class ~[+A, +B](_1: A, _2: B)

}