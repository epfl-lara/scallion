package scallion

import collection.mutable.Queue

/** Contains definitions relating to regular expressions.
  *
  * @groupname regexp Regular Expressions
  * @groupprio regexp 1
  *
  * @groupname combinator Combinators
  * @groupprio combinator 2
  */
trait RegExps[Character] {

  import RegExp._

  /** Regular expressions over characters.
    *
    * @group regexp
    */
  sealed abstract class RegExp {

    /** Indicates if this regular expression accepts the empty word. */
    val acceptsEmpty: Boolean

    /** Returns the rest of a regular expression after consuming a character. */
    def derive(char: Character): RegExp = this match {
      case EmptyStr => EmptySet
      case EmptySet => EmptySet
      case Elem(predicate) =>
        if (predicate(char)) EmptyStr
        else EmptySet
      case Union(left, right) => left.derive(char) | right.derive(char)
      case Concat(first, second) =>
        if (first.acceptsEmpty) first.derive(char) ~ second | second.derive(char)
        else first.derive(char) ~ second
      case Star(regExp) => regExp.derive(char) ~ this
    }

    /** Union of `this` and `that` regular expression.
      *
      * Performs surface-level simplifications.
      *
      * @return A regular expression that accepts words if either `this` or `that` accepts it.
      *
      * @group combinator
      */
    def |(that: RegExp): RegExp = (this, that) match {
      case (EmptySet, _) => that
      case (_, EmptySet) => this
      case _ => Union(this, that)
    }

    /** Concatenation of `this` and `that` regular expression.
      *
      * Performs surface-level simplifications.
      *
      * @return A regular expression that accepts words
      *         if `this` accepts a prefix and `that` accepts the suffix.
      *
      * @group combinator
      */
    def ~(that: RegExp): RegExp = (this, that) match {
      case (EmptySet, _) => EmptySet
      case (_, EmptySet) => EmptySet
      case (EmptyStr, _) => that
      case (_, EmptyStr) => this
      case _ => Concat(this, that)
    }

    /** Exactly `n` instances of `this` regular expression.
      *
      * @group combinator
      */
    def times(n: Int): RegExp = {
      require(n >= 0)

      if (n == 0) {
        EmptyStr
      }
      else {
        this ~ this.times(n - 1)
      }
    }

    /** Zero or one instances of `this` regular expression.
      *
      * @group combinator
      */
    def opt: RegExp = this | EmptyStr
  }

  /** Contains primitive constructors for regular expressions.
    *
    * @group regexp
    */
  object RegExp {

    /** Accepts only the empty word. */
    case object EmptyStr extends RegExp {
      override val acceptsEmpty: Boolean = true
    }

    /** Never accepts. */
    case object EmptySet extends RegExp {
      override val acceptsEmpty: Boolean = false
    }

    /** Accepts single characters that satisfy a predicate. */
    case class Elem(predicate: Character => Boolean) extends RegExp {
      override val acceptsEmpty: Boolean = false
    }

    /** Union of two regular expressions. */
    case class Union(left: RegExp, right: RegExp) extends RegExp {
      override val acceptsEmpty: Boolean = left.acceptsEmpty || right.acceptsEmpty
    }

    /** Concatenation of two regular expressions. */
    case class Concat(first: RegExp, second: RegExp) extends RegExp {
      override val acceptsEmpty: Boolean = first.acceptsEmpty && second.acceptsEmpty
    }

    /** Kleene star of a regular expressions. */
    case class Star(regExp: RegExp) extends RegExp {
      override val acceptsEmpty: Boolean = true
    }
  }

  //---- Combinators ----//

  /** Regular expression that accepts any of the characters in `chars`.
    *
    * @group combinator
    */
  def oneOf(chars: Seq[Character]): RegExp = {
    val set = Set(chars: _*)
    elem((c: Character) => set.contains(c))
  }

  /** Regular expression that accepts single characters based on a `predicate`.
    *
    * @group combinator
    */
  def elem(predicate: Character => Boolean): RegExp = Elem(predicate)

  /** Regular expression that accepts only the single character `char`.
    *
    * @group combinator
    */
  def elem(char: Character): RegExp = Elem(_ == char)

  /** Regular expression that accepts only the sequence of characters `chars`.
    *
    * @group combinator
    */
  def word(chars: Seq[Character]): RegExp = {
    val empty: RegExp = EmptyStr
    chars.foldRight(empty) {
      case (char, rest) => elem(char) ~ rest
    }
  }

  /** Regular expression that accepts zero or more repetitions of `regExp`.
    *
    * @group combinator
    */
  def many(regExp: RegExp): RegExp = regExp match {
    case Star(_) => regExp
    case _ => Star(regExp)
  }

  /** Regular expression that accepts one or more repetitions of `regExp`.
    *
    * @group combinator
    */
  def many1(regExp: RegExp): RegExp = regExp ~ many(regExp)

  /** Regular expression that accepts zero or one instances of `regExp`.
    *
    * @group combinator
    */
  def opt(regExp: RegExp): RegExp = regExp | EmptyStr

  /** Regular expression that accepts any single character. */
  val any: RegExp = Elem((c: Character) => true)
}

/** Regular expressions on characters. Expected to be mixed-in. */
trait CharRegExps { self: RegExps[Char] =>

  /** Single digit between 0 and 9. */
  val digit = elem(_.isDigit)

  /** Single digit between 1 and 9. */
  val nonZero = elem((c: Char) => c >= '1' && c <= '9')

  /** Single digit between 0 and 9 or A and F or a and f. */
  val hex = elem((c: Char) => c.isDigit || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))

  /** Single white space character. */
  val whiteSpace = elem(_.isWhitespace)
}

trait Compiled[Character] { self: RegExps[Character] =>

  sealed trait Transition {
    val target: Int
  }
  case class Guarded(predicate: Character => Boolean, target: Int) extends Transition
  case class Epsilon(target: Int) extends Transition

  trait NFA {
    val states: IndexedSeq[Set[Transition]]
    val start: Set[Int]
    val accepting: Set[Int]

    def size = states.size

    def epsilonClosure(state: Set[Int]): Set[Int] = {
      var res = state
      val queue = new Queue[Int]()
      for (s <- state) {
        queue.enqueue(s)
      }
      
      while (queue.nonEmpty) {
        val s = queue.dequeue()
        
        states(s).foreach {
          case Epsilon(target) =>
            if (!res.contains(target)) {
              res += target
              queue.enqueue(target)
            }
          case _ => ()
        }
      }

      res
    }

    def apply(state: Set[Int], character: Character): Set[Int] = epsilonClosure {
      state.flatMap(states(_)).collect {
        case Guarded(predicate, target) if predicate(character) => target
      }
    }
  }

  import RegExp._

  def toNFA(regExp: RegExp, index: Int = 0): NFA = regExp match {
    case EmptySet => new NFA { 
      val states = Vector()
      val start = Set()
      val accepting = Set()
    }
    case EmptyStr => 
      new NFA {
      val states = Vector(Set())
      val start = Set(index)
      val accepting = Set(index)
    }
    case Elem(predicate) => new NFA {
      val states = Vector(Set(Guarded(predicate, index + 1)), Set())
      val start = Set(index)
      val accepting = Set(index + 1)
    }
    case Union(left, right) => {
      val leftNFA = toNFA(left, index)
      val rightNFA = toNFA(right, index + leftNFA.size)

      new NFA {
        val states = leftNFA.states ++ rightNFA.states
        val start = leftNFA.start union rightNFA.start
        val accepting = leftNFA.accepting union rightNFA.accepting
      }
    }
    case Concat(left, right) => {
      val leftNFA = toNFA(left, index)
      val rightNFA = toNFA(right, index + leftNFA.size)

      new NFA {
        val states = leftNFA.states.zipWithIndex.map {
          case (transitions, i) =>
            if (leftNFA.accepting.contains(i + index)) {
              transitions union rightNFA.start.map(Epsilon(_))
            }
            else {
              transitions
            }
        } ++ rightNFA.states
        val start = leftNFA.start
        val accepting = rightNFA.accepting
      }
    }
    case Star(inner) => {
      val innerNFA = toNFA(inner, index + 1)

      new NFA {
        val states = innerNFA.start.map(Epsilon(_): Transition) +: innerNFA.states.zipWithIndex.map {
          case (transitions, i) => {
            if (innerNFA.accepting.contains(i + index + 1)) {
              transitions + Epsilon(index)
            }
            else {
              transitions
            }
          }
        }
        val start = Set(index)
        val accepting = Set(index)
      }
    }
  }

  class Automaton(regExp: RegExp) {
    val nfa = toNFA(regExp)
    val start = nfa.epsilonClosure(nfa.start)
    var current = start

    def isAccepting: Boolean = (current & nfa.accepting).nonEmpty

    def next(character: Character): (Boolean, Boolean) = {
      current = nfa(current, character)
      ((current & nfa.accepting).nonEmpty, current.exists(i => nfa.states(i).exists {
        case Guarded(_, _) => true
        case _ => false
      }))
    }

    def reset(): Unit = current = start
  }

  def compile(regExp: RegExp): Automaton = new Automaton(regExp)
}