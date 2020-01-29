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
package lexical

import collection.mutable.Queue

/** Adds conversions from regular expressions to non-deterministic finite automata (NFAs)
  * and from NFAs to deterministic finite automata (DFAs).
  *
  * Expected to be mixed-in [[scallion.lexical.RegExps]].
  *
  * @groupname nfa Non-deterministic Finite Automata
  * @groupprio nfa 11
  *
  * @groupname dfa Deterministic Finite Automata
  * @groupprio dfa 12
  */
trait Automatons { self: RegExps =>

  /** Transition of an NFA.
    *
    * @group nfa
    */
  protected sealed trait Transition {

    /** The index of the target of the transition. */
    val target: Int
  }

  /** Transition guarded by a `predicate` on the next character.
    *
    * @group nfa
    */
  protected case class Guarded(condition: Condition, target: Int) extends Transition

  /** Epsilon transition. Can be freely taken.
    *
    * @group nfa
    */
  protected case class Epsilon(target: Int) extends Transition

  /** Non-deterministic finite automaton.
    *
    * @group nfa
    */
  protected trait NFA {

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
  protected object NFA {

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
    case Singleton(value) => new NFA {
        val transitions = Vector(Set(Guarded(Valued(value), index + 1)), Set())
        val start = Set(index)
        val accepting = Set(index + 1)
      }
      case Elem(predicate) => new NFA {
        val transitions = Vector(Set(Guarded(Predicated(predicate), index + 1)), Set())
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
  protected sealed trait DecisionTree[+A] {

    /** Gets the value corresponding to the character. */
    def apply(char: Character): A

    /** Apply a function on all values. */
    def map[B](function: A => B): DecisionTree[B]

    /** All values of the tree. */
    def values: Seq[A]

    def assume(condition: Condition): DecisionTree[A]

    def compact: DecisionTree[A]
  }

  /** Predicate on a character.
    *
    * @group dfa
    */
  protected sealed trait Condition {
    def apply(value: Character): Boolean
  }

  /** Predicate based on a function.
    *
    * @group dfa
    */
  protected case class Predicated(predicate: Character => Boolean) extends Condition {
    override def apply(value: Character): Boolean =
      predicate(value)
  }

  /** Predicate based on equality.
    *
    * @group dfa
    */
  protected case class Valued(value: Character) extends Condition {
    override def apply(other: Character): Boolean = value == other
  }

  /** Branching tree.
    *
    * The `condition` decides whether the value corresponding to a character is in
    * the `trueSide` (in case `predicate` returns `true`) or
    * the `falseSide` (in case `predicate` returns `false`).
    *
    * @group dfa
    */
  protected case class Branch[+A](condition: Condition,
                        trueSide: DecisionTree[A],
                        falseSide: DecisionTree[A]) extends DecisionTree[A] {
    override def apply(char: Character): A =
      if (condition(char)) trueSide(char) else falseSide(char)
    override def map[B](function: A => B): DecisionTree[B] = {
      val ths = trueSide.map(function)
      val fhs = falseSide.map(function)
      if (ths == fhs) {
        ths
      }
      else {
        Branch(condition, ths, fhs)
      }
    }
    override def values: Seq[A] = trueSide.values ++ falseSide.values

    override def assume(condition: Condition): DecisionTree[A] = {
      def ths = trueSide.assume(condition)
      def fhs = falseSide.assume(condition)

      (this.condition, condition) match {
        case (Valued(first), Valued(second)) => if (first == second) ths else fhs
        case (Valued(first), Predicated(pre)) => if (pre(first)) Branch(this.condition, ths, fhs).compact else fhs
        case (Predicated(pre), Valued(second)) => if (pre(second)) ths else fhs
        case _ => Branch(this.condition, ths, fhs).compact
      }
    }

    override def compact: DecisionTree[A] = {
      val ths = trueSide.compact
      val fhs = falseSide.compact

      if (ths == fhs) ths else Branch(condition, ths, fhs)
    }
  }

  /** Leaf tree. Always contains the same value for all characters.
    *
    * @group dfa
    */
  protected case class Leaf[+A](value: A) extends DecisionTree[A] {
    override def apply(char: Character): A = value
    override def map[B](function: A => B): Leaf[B] = Leaf(function(value))
    override def values: Seq[A] = Vector(value)
    override def assume(condition: Condition): DecisionTree[A] = this
    override def compact: DecisionTree[A] = this
  }

  /** Deterministic finite automaton.
    *
    * @group dfa
    */
  protected trait DFA {

    /** State transitions. */
    val transitions: IndexedSeq[DecisionTree[Int]]

    /** Index of starting state. */
    val start: Int

    /** Contains, for each state, whether it is accepting or not. */
    val isAccepting: IndexedSeq[Boolean]

    /** Contains, for each state, whether an
      * accepting state can be reached later on.
      */
    val isLive: IndexedSeq[Boolean]

    /** Returns the index of the next state.
      *
      * @param state The index of the current state.
      * @param char  The next input character.
      */
    def apply(state: Int, char: Character): Int = transitions(state)(char)
  }

  /** Builds deterministic finite automata.
    *
    * @group dfa
    */
  protected object DFA {

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
          case Guarded(condition, target) => (condition, target)
        }

        // Group targets by the condition that leads there.
        val decisions = guardedTransitions
          .groupBy(_._1)
          .iterator
          .map { case (c, ts) => c -> ts.map(_._2).toSet }
          .toList

        // Converts the guarded targets to a decision tree of NFA states.
        def toTree(pairs: List[(Condition, Set[Int])]): DecisionTree[Set[Int]] =
          pairs match {
            case Nil => Leaf(Set())
            case (condition, values) :: rest => {
              val tree = toTree(rest)
              val ths = tree.map(_ union values).assume(condition)
              val fhs = tree.assume(condition match {
                case Predicated(pre) => Predicated(!pre(_))
                case Valued(value) => Predicated(_ != value)
              })

              if (ths == fhs) {
                ths
              }
              else {
                Branch(condition, ths, fhs)
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
        val transitions = transitionsArray.toIndexedSeq
        val isAccepting = isAcceptingArray.toIndexedSeq
        val isLive = isLiveArray.toIndexedSeq
      }
    }
  }
}
