package scallion
package lexing

import collection.mutable.Queue

/** Adds conversions from regular expressions to non-deterministic finite automata (NFAs)
  * and from NFAs to deterministic finite automata (DFAs).
  *
  * Expected to be mixed-in [[scallion.lexing.RegExps]].
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