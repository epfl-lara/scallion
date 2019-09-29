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

package scallion.syntactic

import scala.annotation.tailrec
import scala.collection.mutable.{ HashMap, ListBuffer }

import scallion.util.internal._

/** Contains conversion from Syntax to Pushdown Automaton. */
trait Automatons[Token, Kind] { self: Syntaxes[Token, Kind] =>

  import Syntax._

  /** State transition.
    *
    * @param target  The target state.
    * @param actions Actions to execute while taking the transition.
    */
  private case class Transition(target: State, actions: List[Action])

  /** Action on the stacks. */
  sealed trait Action

  /** Push a value on the value stack. */
  private case class PushValue(value: Any) extends Action

  /** Apply function on top of stack. */
  private case class Apply(function: Any => Any) extends Action

  /** Pop the top two values of the stack and push their pair. */
  private case object ReducePair extends Action

  /** Pop the top two values of the stack and push their concatenation. */
  private case object ReduceConcat extends Action

  /** Push a state on the state stack. */
  private case class EnterRec(exit: State) extends Action


  /** State of the automaton. */
  private sealed trait State

  /** State with transitions to other states. */
  private class IntermediateState extends State {

    // The two fields below are initialized after the fact.

    /** Transition to the final state. Optional. */
    var epsilon: Option[List[Action]] = null

    /** Transitions to other states, */
    var transitions: Map[Kind, Transition] = null
  }

  /** Accepting state. */
  private case object AcceptingState extends State

  /** Builder for state. */
  private sealed trait StateBuilder {

    /** The state to be initialized. */
    val state: State
  }

  /** Builder for intermediate state.
    *
    * @param cells Mutable list of cells, to be updated with `this` cells.
    */
  private class IntermediateStateBuilder(cells: ListBuffer[Cell[_]]) extends StateBuilder {

    override val state: IntermediateState = new IntermediateState

    /** Cell for epsilon transition. */
    val cellEpsilon: Cell[List[Action]] = new CellOnce({
      (res: Option[List[Action]]) => state.epsilon = res
    })

    /** Cell for transitions. */
    val cellTransitions: Cell[Map[Kind, Transition]] = new CellUpgradableMap({
      (res: Map[Kind, Transition]) => state.transitions = res
    })

    // Addings cells to the mutable list of cells.
    cells += cellEpsilon
    cells += cellTransitions

    /** Adds an epsilon transition from this start to the target state,
     *  with an action performed on the transition.
     */
    def addEpsilon(target: StateBuilder, action: Action): Unit = target match {
      case state: IntermediateStateBuilder => {
        state.cellEpsilon.register {
          (actions: List[Action]) => cellEpsilon(action :: actions)
        }
        state.cellTransitions.register {
          (transitions: Map[Kind, Transition]) => {
            cellTransitions(transitions.map {
              case (kind, Transition(target, actions)) =>
                (kind, Transition(target, action :: actions))
            })
          }
        }
      }
      case AcceptingStateBuilder => {
        cellEpsilon(List(action))
      }
    }

    /** Adds an epsilon transition from this start to the target state. */
    def addEpsilon(target: StateBuilder): Unit = target match {
      case state: IntermediateStateBuilder => {
        state.cellEpsilon.register(cellEpsilon)
        state.cellTransitions.register(cellTransitions)
      }
      case AcceptingStateBuilder => {
        cellEpsilon(List())
      }
    }

    /** Adds a transition to a target state when encountering a token
      * of the specified kind.
      */
    def addTransition(target: StateBuilder, kind: Kind): Unit =
      cellTransitions(Map(kind -> Transition(target.state, List())))
  }

  /** Builder for the accepting state. */
  private object AcceptingStateBuilder extends StateBuilder {
    override val state: AcceptingState.type = AcceptingState
  }

  /* Factory for Pushdown Automata. */
  object PushdownAutomaton {

    /** Create an automaton given a syntax. */
    def create[A](syntax: Syntax[A]): PushdownAutomaton[A] = {
      val cells = new ListBuffer[Cell[_]]()
      val start = new IntermediateStateBuilder(cells)
      val recs = new HashMap[RecId, StateBuilder]()

      process(syntax, start, AcceptingStateBuilder, recs, cells)

      for (cell <- cells) {
        cell.complete()
      }

      new PushdownAutomaton(start.state)
    }

    /** Walks down the syntax and create states and transitions.
      *
      * @param syntax The syntax to process.
      * @param from   The starting state builder.
      * @param to     The final state builder.
      * @param recs   Mutable map of states associated with each recursive syntax.
      * @param cells  Mutable list of all created cells.
      */
    private def process(
        syntax: Syntax[_],
        from: IntermediateStateBuilder,
        to: StateBuilder,
        recs: HashMap[RecId, StateBuilder],
        cells: ListBuffer[Cell[_]]): Unit = syntax match {
      case Success(value, _) =>
        from.addEpsilon(to, PushValue(value))
      case Failure() =>
        ()
      case Elem(kind) =>
        from.addTransition(to, kind)
      case Transform(function, _, inner) =>
        val inter = new IntermediateStateBuilder(cells)
        process(inner, from, inter, recs, cells)
        inter.addEpsilon(to, Apply(function))
      case Disjunction(left, right) =>
        process(left, from, to, recs, cells)
        process(right, from, to, recs, cells)
      case Sequence(left, right) =>
        val inter = new IntermediateStateBuilder(cells)
        val after = new IntermediateStateBuilder(cells)
        process(left, from, inter, recs, cells)
        process(right, inter, after, recs, cells)
        after.addEpsilon(to, ReducePair)
      case Concat(left, right) =>
        val inter = new IntermediateStateBuilder(cells)
        val after = new IntermediateStateBuilder(cells)
        process(left, from, inter, recs, cells)
        process(right, inter, after, recs, cells)
        after.addEpsilon(to, ReduceConcat)
      case Recursive(recId, inner) =>
        val enter = recs.get(recId).getOrElse {
          val enter = new IntermediateStateBuilder(cells)
          recs += recId -> enter
          process(inner, enter, AcceptingStateBuilder, recs, cells)
          enter
        }
        if (to.state == AcceptingState) {
          // Leaving to the accepting state is a no-op,
          // therefore we don't bother.
          from.addEpsilon(enter)
        }
        else {
          from.addEpsilon(enter, EnterRec(to.state))
        }
    }
  }

  /** Pushdown Automaton.
    *
    * Provides fast parse.
    */
  class PushdownAutomaton[+A] private (private val start: State) {

    /** Stack of states to enter. */
    private var recsStack: List[State] = List.empty

    /** Stack of values. */
    private var valuesStack: List[Any] = List.empty

    /** Current state. */
    private var current: State = start

    /** Resets to automaton to its initial state. */
    def reset(): Unit = {
      recsStack = List.empty
      valuesStack = List.empty
      current = start
    }

    /** Parses the tokens into a value.
      *
      * The iterator will be consumed up until the end
      * or up to the first erroneous token.
      *
      * @return Some value in case parsing succeeded, or None otherwise.
      */
    def apply(tokens: Iterator[Token]): Option[A] = {

      var ok = true
      while (tokens.hasNext && ok) {
        val token = tokens.next()
        ok = next(token)
      }

      if (ok) {
        accept()
      }
      else {
        None
      }
    }

    /** Processes the next token of input.
      *
      * @return true if the token was successfully processed,
      *         false otherwise.
      */
    private def next(token: Token): Boolean = {
      val kind: Kind = getKind(token)

      @tailrec
      def go(): Boolean = current match {
        case AcceptingState => recsStack match {
          case first :: rest =>
            current = first
            recsStack = rest
            go()
          case Nil =>
            false
        }
        case inter: IntermediateState => inter.transitions.get(kind) match {
          case Some(Transition(to, actions)) =>
            perform(actions)
            valuesStack = token :: valuesStack
            current = to
            true
          case None => inter.epsilon match {
            case Some(actions) =>
              perform(actions)
              current = AcceptingState
              go()
            case None =>
              false
          }
        }
      }

      go()
    }

    /** Performs the actions on the stacks. */
    private def perform(actions: List[Action]): Unit =
      actions.foreach {
        case PushValue(value) =>
          valuesStack = value :: valuesStack
        case Apply(function) =>
          val first :: rest = valuesStack
          valuesStack = function(first) :: rest
        case ReducePair =>
          val second :: first :: rest = valuesStack
          valuesStack = scallion.syntactic.~(first, second) :: rest
        case ReduceConcat =>
          val second :: first :: rest = valuesStack
          valuesStack = (first.asInstanceOf[Seq[Any]] ++ second.asInstanceOf[Seq[Any]]) :: rest
        case EnterRec(exit) =>
          recsStack = exit :: recsStack
      }

    /** Computes the final value at the end of input. */
    private def accept(): Option[A] = {

      @tailrec
      def go(): Option[A] = current match {
        case AcceptingState => recsStack match {
          case first :: rest =>
            current = first
            recsStack = rest
            go()
          case Nil =>
            Some(valuesStack.head.asInstanceOf[A])
        }
        case inter: IntermediateState => inter.epsilon match {
          case Some(actions) =>
            perform(actions)
            current = AcceptingState
            go()
          case None =>
            None
        }
      }

      go()
    }
  }
}