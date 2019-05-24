
package scallion
package util

import scala.collection.mutable.{Queue, StringBuilder}

/** Contains utilities to vizualize parsers as grammars. */
trait Grammars[Kind] { self: Parsers[_, Kind] =>

  sealed trait Operand {
    def pretty(names: Int => String): String = this match {
      case NonTerminal(id) => names(id)
      case Terminal(kind) => kind.toString
      case Epsilon => "𝛆"
    }
  }
  case class NonTerminal(id: Int) extends Operand
  case class Terminal(kind: Kind) extends Operand
  case object Epsilon extends Operand

  case class Rule(id: Int, operands: Seq[Seq[Operand]]) {
    def pretty(names: Int => String): String = names(id) + " ::= " +
      operands.map(xs => xs.map(_.pretty(names)).mkString(" ")).mkString(" | ")
  }

  case class Grammar(rules: Seq[Rule]) {
    def pretty(names: Int => String = _.toString): String = rules.map(_.pretty(names)).mkString("\n")
  }

  import Parser._

  def getGrammar(parser: Parser[Any]): Grammar = {
    var nextId = 0
    var rules = Vector[Rule]()
    val queue = new Queue[(Parser[Any], Int)]
    var ids = Map[Parser[Any], Int]()

    def inspect(next: Parser[Any]): Int = {
      if (!ids.contains(next)) {
        val res = nextId
        nextId += 1
        ids += next -> res
        queue.enqueue(next -> res)
        res
      }
      else {
        ids(next)
      }
    }

    inspect(parser)

    def getOperands(next: Parser[Any]): Seq[Seq[Operand]] = next match {
      case Disjunction(left, right) => getOperands(left) ++ getOperands(right)
      case _ => Seq(getSequents(next))
    }

    def getSequents(next: Parser[Any]): Seq[Operand] = next match {
      case Failure => Seq()
      case Success(_) => Seq(Epsilon)
      case Elem(kind) => Seq(Terminal(kind))
      case Transform(_, inner) => getSequents(inner)
      case Sequence(left, right) => getSequents(left) ++ getSequents(right)
      case Concat(left, right) => getSequents(left) ++ getSequents(right)
      case d@Disjunction(_, _) => {
        val id = inspect(d)
        Seq(NonTerminal(id))
      }
      case r@Recursive(_) => {
        val id = inspect(r.inner)
        Seq(NonTerminal(id))
      }
    }

    while(queue.nonEmpty) {
      val (current, id) = queue.dequeue()
      rules :+= Rule(id, getOperands(current))
    }

    Grammar(rules)
  }
}