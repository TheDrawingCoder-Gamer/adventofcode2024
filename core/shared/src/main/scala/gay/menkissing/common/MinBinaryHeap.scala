package gay.menkissing.common

import collection.mutable
import cats.*
import cats.syntax.all.*

import annotation.tailrec

final class MinBinaryHeap[A, S](using ord: Order[S]):
  val backing: mutable.ArrayBuffer[(A, S)] = mutable.ArrayBuffer()

  @tailrec
  private def bubbleUp(p: Int): Unit =
    val parent = (p - 1) / 2
    if backing(parent)._2 > backing(p)._2 then
      val v = backing(parent)
      backing(parent) = backing(p)
      backing(p) = v
      if parent != 0 then bubbleUp(parent)

  @tailrec
  private def siftDown(p: Int): Unit =
    val left = 2 * p + 1
    val right = 2 * p + 2

    var smallest = p

    if left < backing.length && backing(left)._2 < backing(smallest)._2 then
      smallest = left
    if right < backing.length && backing(right)._2 < backing(smallest)._2 then
      smallest = right

    if smallest != p then
      val v = backing(smallest)
      backing(smallest) = backing(p)
      backing(p) = v
      siftDown(smallest)

  def insert(n: A, priority: S): this.type =
    // first, append that thang :joy:
    backing.append((n, priority))
    // then bubble up that thang :joy:
    bubbleUp(backing.length - 1)
    this

  def extractWithPriority(): (A, S) =
    // for degenerate case of backing.length == 1
    if backing.length == 1 then
      val r = backing.remove(0)
      r
    else
      // first, get the last element & the head
      val last = backing.remove(backing.length - 1)
      val head = backing.remove(0)
      // and prepend the last element
      backing.prepend(last)
      // then sift down to preserve the heap property
      siftDown(0)
      head
  def extract(): A = extractWithPriority()._1
  def head: A = backing(0)._1
  def headOption: Option[A] = Option.when(backing.nonEmpty)(backing(0)._1)
  def updatePriority(n: A, value: S)(using Eq[A]): this.type =
    val p = backing.indexWhere(_._1 === n)
    if p < 0 then return insert(n, value)
    val (q, realScore) = backing(p)
    if value === realScore then ()
    else if value < realScore then
      // in a min heap, when decreasing a key, we bubble up
      backing(p) = (q, value)
      bubbleUp(p)
    else
      backing(p) = (q, value)
      siftDown(p)

    this

  def isEmpty: Boolean = backing.isEmpty
  def nonEmpty: Boolean = backing.nonEmpty
