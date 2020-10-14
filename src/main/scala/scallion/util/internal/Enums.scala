package scallion.util.internal

import scala.collection.mutable.{HashSet, HashMap, ArrayBuffer, Queue}

package object enums {

  private[scallion] class Probe {
    private var changed = false
    def notifyChange(): Unit = changed = true
    def reset(): Unit = changed = false
    def hasChanged: Boolean = changed
  }

  private[scallion] class Runner[A](
      cell: Cell,
      queue: Queue[Tree[A]],
      probe: Probe) extends Iterator[Iterator[A]] {

    var ended = false

    private def updateQueue(): Unit =
      while (queue.isEmpty && !ended) {
        nextStep()
      }

    override def hasNext: Boolean = {
      updateQueue()
      !(queue.isEmpty && ended)
    }

    override def next(): Iterator[A] = {
      updateQueue()
      val tree = queue.dequeue()
      tree.values
    }

    private def nextStep(): Unit = {
      probe.reset()
      cell.increase()
      if (queue.isEmpty && !probe.hasChanged) {
        ended = true
      }
    }
  }

  private[scallion] sealed abstract class Tree[+A] {
    val size: Int

    def ++[B >: A](that: Tree[B]): Tree[B] = new Branch(this, that)

    def values: Iterator[A]

    val hash: Int
    override def hashCode(): Int = hash
    override def equals(that: Any): Boolean = {
      if (!that.isInstanceOf[Tree[_]]) {
        return false
      }
      val thisValues = values
      val thatValues = that.asInstanceOf[Tree[_]].values

      while (thisValues.hasNext) {
        if (!thatValues.hasNext) {
          return false
        }
        if (thisValues.next() != thatValues.next()) {
          return false
        }
      }

      return !thatValues.hasNext
    }
    override def toString = "Tree(" + values.mkString(", ") + ")"
  }

  private[scallion] object Empty extends Tree[Nothing] {
    override val hash: Int = 0
    override val size: Int = 0
    override def values: Iterator[Nothing] = Iterator()
  }

  private[scallion] class Node[+A](value: A) extends Tree[A] {
    override val hash: Int = value.hashCode()
    override val size = 1
    override def values: Iterator[A] = Iterator(value)
  }

  private[scallion] class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A] {
    override val hash: Int = left.hash ^ Integer.rotateLeft(right.hash, left.size)
    override val size = left.size + right.size
    override def values: Iterator[A] = left.values ++ right.values
  }

  private[scallion] sealed abstract class Cell {
    def increase(): Unit
  }

  private[scallion] object EmptyCell extends Cell {
    override def increase(): Unit = ()
  }

  private[scallion] class ElemCell[A](value: A, subscriber: Tree[A] => Unit) extends Cell {
    private var sent = false

    override def increase(): Unit =
      if (!sent) {
        subscriber(new Node(value))
        sent = true
      }
  }

  private[scallion] class EnvEntry[A](probe: Probe) {
    private var inner: Cell = EmptyCell
    private var size: Int = 0

    def setInner(cell: Cell): Unit = inner = cell

    def currentSize: Int = size

    def increase(): Unit = {
      size += 1
      inner.increase()
    }

    val subscribers: ArrayBuffer[Tree[A] => Unit] = new ArrayBuffer()

    def addVarCell(subscriber: Tree[A] => Unit): Cell = {
      val varCell = new VarCell(this, probe, subscriber)
      subscribers += {
        tree => varCell.inform(tree)
      }
      varCell
    }

    def inform(tree: Tree[A]): Unit =
      subscribers.foreach(_(tree))
  }

  private[scallion] class DisjunctionCell[A](subscriber: Tree[A] => Unit) extends Cell {

    private var leftCell: Cell = EmptyCell
    private var rightCell: Cell = EmptyCell

    private var seen: HashSet[Tree[A]] = new HashSet()

    def setLeftCell(cell: Cell): Unit = leftCell = cell
    def setRightCell(cell: Cell): Unit = rightCell = cell

    def increase(): Unit = {
      seen = new HashSet()
      leftCell.increase()
      rightCell.increase()
    }

    def informLeft(tree: Tree[A]): Unit = {
      if (!seen.contains(tree)) {
        seen += tree
        subscriber(tree)
      }
    }

    def informRight(tree: Tree[A]): Unit = {
      if (!seen.contains(tree)) {
        seen += tree
        assert(seen.contains(tree))
        subscriber(tree)
      }
    }
  }

  private[scallion] class SharedCell[A](subscriber: Tree[A] => Unit) extends Cell {

    private val cells: ArrayBuffer[Cell] = new ArrayBuffer()

    private var seen: HashSet[Tree[A]] = new HashSet()

    def addCell(cell: Cell): Unit = cells += cell

    def increase(): Unit = {
      seen = new HashSet()
      for (cell <- cells) {
        cell.increase()
      }
    }

    def inform(tree: Tree[A]): Unit = {
      if (!seen.contains(tree)) {
        seen += tree
        subscriber(tree)
      }
    }
  }

  private[scallion] class SequenceCell[A](
      nullLeft: Boolean,
      nullRight: Boolean,
      probe: Probe,
      subscriber: Tree[A] => Unit) extends Cell {

    private var leftSize: Int = 0
    private var rightSize: Int = 0

    private var maxLeft: Option[Int] = if (nullLeft) Some(0) else None
    private var maxRight: Option[Int] = if (nullRight) Some(0) else None

    private var size: Int = 0

    private var leftCell: Cell = EmptyCell
    private var rightCell: Cell = EmptyCell

    private var seen: HashSet[Tree[A]] = new HashSet

    private val leftValues: ArrayBuffer[ArrayBuffer[Tree[A]]] = new ArrayBuffer()
    leftValues += new ArrayBuffer()
    if (nullLeft) {
      leftValues(0) += Empty
    }
    private val rightValues: ArrayBuffer[ArrayBuffer[Tree[A]]] = new ArrayBuffer()
    rightValues += new ArrayBuffer()
    if (nullRight) {
      rightValues(0) += Empty
    }

    def fetchLeft(): Unit = {
      leftSize += 1
      leftValues += new ArrayBuffer()
      leftCell.increase()
    }

    def fetchRight(): Unit = {
      rightSize += 1
      rightValues += new ArrayBuffer()
      rightCell.increase()
    }

    def setLeftCell(cell: Cell): Unit = leftCell = cell
    def setRightCell(cell: Cell): Unit = rightCell = cell

    def increase(): Unit = {
      size += 1
      seen = new HashSet()

      if (maxLeft.nonEmpty && maxRight.nonEmpty) {
        for (left <- (size - rightSize) to leftSize) {
          val right = size - left

          val ls = leftValues(left)
          val rs = rightValues(right)
          for (l <- ls; r <- rs) {
            val tree = l ++ r
            if (!seen.contains(tree)) {
              seen += tree
              subscriber(l ++ r)
            }
          }
        }
        if (maxLeft.get + maxRight.get > size) {
          probe.notifyChange()
        }
      }
      else {
        probe.notifyChange()
      }


      if (maxLeft.nonEmpty && maxRight.nonEmpty) {
        fetchLeft()
        fetchRight()
      }
      else {
        if (maxLeft.isEmpty) {
          fetchLeft()
        }
        else {
          fetchRight()
        }
      }
    }

    def informLeft(tree: Tree[A]): Unit = {
      maxLeft = Some(leftSize)
      leftValues(leftSize) += tree
      val right = size - leftSize
      rightValues(right).foreach(r => {
        val combined = tree ++ r
        if (!seen.contains(combined)) {
          seen += combined
          subscriber(combined)
        }
      })
      probe.notifyChange()
    }

    def informRight(tree: Tree[A]): Unit = {
      maxRight = Some(rightSize)
      rightValues(rightSize) += tree
      val left = size - rightSize
      leftValues(left).foreach(l => {
        val combined = l ++ tree
        if (!seen.contains(combined)) {
          seen += combined
          subscriber(combined)
        }
      })
      probe.notifyChange()
    }
  }

  private[scallion] class VarCell[A](
      envEntry: EnvEntry[A],
      probe: Probe,
      subscriber: Tree[A] => Unit) extends Cell {

    private val entries: HashMap[Int, ArrayBuffer[Tree[A]]] = new HashMap()

    private var size: Int = 0

    def inform(tree: Tree[A]): Unit = {
      val n = tree.size
      if (n > size) {
        val xs = entries.get(n).getOrElse {
          val xs = new ArrayBuffer[Tree[A]]()
          entries += n -> xs
          xs
        }

        xs += tree
      }
      else {
        assert(n == size)
        subscriber(tree)
      }
    }

    override def increase(): Unit = {
      size += 1
      if (size > envEntry.currentSize) {
        envEntry.increase()
        assert(size == envEntry.currentSize)
      }
      else {
        entries.get(size) match {
          case Some(trees) => {
            entries -= size
            trees.foreach(tree => {
              subscriber(tree)
            })
          }
          case None => if (entries.nonEmpty) {
            probe.notifyChange()
          }
        }
      }
    }
  }
}