package scallion
package lexing

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

  /** Regular expression that accepts any single character.
    *
    * @group combinator
    */
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

/** Adds conversions from regular expressions to non-deterministic finite automata (NFAs)
  * and from NFAs to deterministic finite automata (DFAs).
  *
  * @groupname nfa Non-deterministic Finite Automata
  * @groupprio nfa 11
  *
  * @groupname dfa Deterministic Finite Automata
  * @groupprio dfa 12
  */
trait Automatons[Character] { self: RegExps[Character] =>

  /** Transition of an NFA.
    *
    * @group nfa
    */
  sealed trait Transition {

    /** The index of the target of the transition. */
    val target: Int
  }

  /** Transition guarded by a `predicate` on the next character.
    *
    * @group nfa
    */
  case class Guarded(predicate: Character => Boolean, target: Int) extends Transition

  /** Epsilon transition. Can be freely taken.
    *
    * @group nfa
    */
  case class Epsilon(target: Int) extends Transition

  /** Non-deterministic finite automaton.
    *
    * @group nfa
    */
  trait NFA {

    /** The transitions. */
    val transitions: IndexedSeq[Set[Transition]]

    /** Starting states. */
    val start: Set[Int]

    /** Accepting states. */
    val accepting: Set[Int]

    /** Number of states. */
    def size = transitions.size

    /** Returns the set of states reachable using any number of epsilon
      * transitions starting from any of the `states`. */
    def epsilonClosure(states: Set[Int]): Set[Int] = {
      var res = states
      val queue = new Queue[Int]()
      for (s <- states) {
        queue.enqueue(s)
      }

      while (queue.nonEmpty) {
        val s = queue.dequeue()

        transitions(s).foreach {
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
  }

  /** Builds non-deterministic finite automata.
    *
    * @group nfa
    */
  object NFA {

    import RegExp._

    /** Returns a NFA corresponding to the `regExp`. */
    def apply(regExp: RegExp): NFA = toNFA(regExp, 0)

    private def toNFA(regExp: RegExp, index: Int): NFA = regExp match {
      case EmptySet => new NFA {
        val transitions = Vector()
        val start = Set()
        val accepting = Set()
      }
      case EmptyStr =>
        new NFA {
        val transitions = Vector(Set())
        val start = Set(index)
        val accepting = Set(index)
      }
      case Elem(predicate) => new NFA {
        val transitions = Vector(Set(Guarded(predicate, index + 1)), Set())
        val start = Set(index)
        val accepting = Set(index + 1)
      }
      case Union(left, right) => {
        val leftNFA = toNFA(left, index)
        val rightNFA = toNFA(right, index + leftNFA.size)

        new NFA {
          val transitions = leftNFA.transitions ++ rightNFA.transitions
          val start = leftNFA.start union rightNFA.start
          val accepting = leftNFA.accepting union rightNFA.accepting
        }
      }
      case Concat(left, right) => {
        val leftNFA = toNFA(left, index)
        val rightNFA = toNFA(right, index + leftNFA.size)

        new NFA {
          val transitions = leftNFA.transitions.zipWithIndex.map {
            case (transitions, i) =>
              if (leftNFA.accepting.contains(i + index)) {
                transitions union rightNFA.start.map(Epsilon(_))
              }
              else {
                transitions
              }
          } ++ rightNFA.transitions
          val start = leftNFA.start
          val accepting = rightNFA.accepting
        }
      }
      case Star(inner) => {
        val innerNFA = toNFA(inner, index + 1)

        new NFA {
          val transitions =
            innerNFA.start.map(Epsilon(_): Transition) +:
            innerNFA.transitions.zipWithIndex.map {
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
  }

  /** Tree of values of type `A`, with branches predicated by a
    * test on a single character.
    *
    * @group dfa
    */
  sealed trait DecisionTree[+A] {

    /** Gets the value corresponding to the character. */
    def apply(char: Character): A

    /** Apply a function on all values. */
    def map[B](function: A => B): DecisionTree[B]

    /** All values of the tree. */
    def values: Seq[A]
  }

  /** Branching tree.
    *
    * The `predicate` decides whether the value corresponding to a character is in
    * the `trueSide` (in case `predicate` returns `true`) or
    * the `falseSide` (in case `predicate` returns `false`).
    *
    * @group dfa
    */
  case class Branch[+A](predicate: Character => Boolean,
                        trueSide: DecisionTree[A],
                        falseSide: DecisionTree[A]) extends DecisionTree[A] {
    override def apply(char: Character): A =
      if (predicate(char)) trueSide(char) else falseSide(char)
    override def map[B](function: A => B): DecisionTree[B] = {
      val ths = trueSide.map(function)
      val fhs = falseSide.map(function)
      if (ths == fhs) {
        ths
      }
      else {
        Branch(predicate, ths, fhs)
      }
    }
    override def values: Seq[A] = trueSide.values ++ falseSide.values
  }

  /** Leaf tree. Always contains the same value for all characters.
    *
    * @group dfa
    */
  case class Leaf[+A](value: A) extends DecisionTree[A] {
    override def apply(char: Character): A = value
    override def map[B](function: A => B): Leaf[B] = Leaf(function(value))
    override def values: Seq[A] = Vector(value)
  }

  /** Deterministic finite automaton.
    *
    * @group dfa
    */
  trait DFA {
    val transitions: IndexedSeq[DecisionTree[Int]]
    val start: Int
    val isAccepting: IndexedSeq[Boolean]
    val isLive: IndexedSeq[Boolean]

    def apply(state: Int, char: Character): Int = transitions(state)(char)
  }

  /** Builds deterministic finite automata.
    *
    * @group dfa
    */
  object DFA {

    /** Builds a DFA equivalent to the given regular expression. */
    def apply(regExp: RegExp): DFA = toDFA(NFA(regExp))

    /** Builds a DFA equivalent to the given NFA. */
    def apply(nfa: NFA): DFA = toDFA(nfa)

    private def toDFA(nfa: NFA) = {
      // Next free index.
      var nextIndex = 0

      // Queue of NFA states to process.
      val queue = new Queue[Set[Int]]()

      // Index of the DFA state corresponding to the NFA states.
      var indices: Map[Set[Int], Int] = Map()

      // Transitions in the DFA.
      var transitions: IndexedSeq[DecisionTree[Int]] = Vector()

      // Accepting states of the NFA.
      var isAccepting: IndexedSeq[Boolean] = Vector()

      // Returns the DFA state corresponding to an NFA state. */
      def inspect(state: Set[Int]): Int = {
        if (indices.contains(state)) {
          indices(state)
        }
        else {
          val index = nextIndex
          nextIndex += 1
          queue.enqueue(state)
          indices += state -> index
          index
        }
      }

      // Feed the starting state of the NFA.
      inspect(nfa.epsilonClosure(nfa.start))

      // While there are states in the NFA that have not been converted.
      while (queue.nonEmpty) {
        val state = queue.dequeue()

        // Get all guarded transitions from the current NFA state.
        val guardedTransitions = state.flatMap(nfa.transitions(_)).collect {
          case Guarded(predicate, target) => (predicate, target)
        }

        // Group targets by the predicate that leads there.
        val decisions = guardedTransitions.groupBy(_._1).mapValues(_.map(_._2).toSet).toList

        // Converts the guarded targets to a decision tree of NFA states.
        def toTree(pairs: List[(Character => Boolean, Set[Int])]): DecisionTree[Set[Int]] =
          pairs match {
            case Nil => Leaf(Set())
            case (predicate, values) :: rest => {
              val tree = toTree(rest)
              val ths = tree.map(_ union values)
              val fhs = tree

              if (ths == fhs) {
                ths
              }
              else {
                Branch(predicate, ths, fhs)
              }
            }
          }

        // Records the new DFA state.
        transitions :+= toTree(decisions).map(nfa.epsilonClosure(_)).map(inspect)
        isAccepting :+= (state & nfa.accepting).nonEmpty
      }

      // Compute the set of states which can reach accepting states.
      var oldSize = 0
      var reachable = (0 until transitions.size)
        .filter(transitions(_).values.exists(isAccepting(_)))
        .toSet
      var newSize = reachable.size

      while (newSize != oldSize) {
        oldSize = newSize
        var addition = (0 until transitions.size)
          .filter(transitions(_).values.exists(reachable.contains(_)))
          .toSet
        reachable = reachable union addition
        newSize = reachable.size
      }

      // Converts the different sequences to arrays for faster access.
      val isLiveArray = Array.tabulate(transitions.size)(reachable(_))
      val transitionsArray = transitions.toArray
      val isAcceptingArray = isAccepting.toArray

      new DFA {
        val start = 0
        val transitions = transitionsArray
        val isAccepting = isAcceptingArray
        val isLive = isLiveArray
      }
    }
  }
}