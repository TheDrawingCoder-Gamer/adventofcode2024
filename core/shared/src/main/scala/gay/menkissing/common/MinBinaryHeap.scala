package gay.menkissing.common

import collection.mutable
import cats.*
import cats.syntax.all.*

import annotation.tailrec

sealed class BinaryHeap[A](using ord: Order[A]):
  protected val backing: mutable.ArrayBuffer[A] = mutable.ArrayBuffer()

  @tailrec
  protected final def bubbleUp(p: Int): Unit =
    val parent = (p - 1) / 2
    if backing(parent) > backing(p) then
      val v = backing(parent)
      backing(parent) = backing(p)
      backing(p) = v
      if parent != 0 then bubbleUp(parent)
  @tailrec
  protected final def siftDown(p: Int): Unit =
    val left = 2 * p + 1
    val right = 2 * p + 2

    var smallest = p

    if left < backing.length && backing(left) < backing(smallest) then
      smallest = left
    if right < backing.length && backing(right) < backing(smallest) then
      smallest = right

    if smallest != p then
      val v = backing(smallest)
      backing(smallest) = backing(p)
      backing(p) = v
      siftDown(smallest)
  def insert(n: A): this.type =
    backing.append(n)
    bubbleUp(backing.length - 1)
    this
  def extract(): A =
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
  def peek: A = backing(0)
  def peekOption: Option[A] = Option.when(backing.nonEmpty)(peek)

  def isEmpty: Boolean = backing.isEmpty
  def nonEmpty: Boolean = backing.nonEmpty

final class MinBinaryHeap[A, S]
  (using ord: Order[S])
    extends BinaryHeap[(A, S)](using ord.contramap(_._2)):

  inline def insert(n: A, priority: S): this.type = insert((n, priority))

  def extractWithoutPriority(): A = extract()._1
  def head: A = peek._1
  def headOption: Option[A] = peekOption.map(_._1)
  // very slow, if possible to use a different one do it
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
