package gay.menkissing.common

import scala.annotation.tailrec
import scala.collection.mutable as mut
import math.Numeric

import cats.*
import cats.implicits.*
import cats.data.Kleisli


class ForeverIterator[A](val underlying: Iterator[A]) extends Iterator[A]:
  private var memoizedAll: Boolean = false
  private val memoizedValues = mut.ArrayBuffer[A]()
  private var currentIterator = underlying

  override def hasNext: Boolean = memoizedAll || currentIterator.hasNext

  override def next(): A =
    if currentIterator.hasNext then
      val v = currentIterator.next()
      if !memoizedAll then
        memoizedValues.append(v)
        if !currentIterator.hasNext then
          memoizedAll = true
      v
    else if memoizedAll then
      currentIterator = memoizedValues.iterator
      currentIterator.next()
    else
      throw new NoSuchElementException("Input iterator was empty")


def topologicalSort[A](a: List[A])(using pord: PartialOrder[A]): Option[List[A]] =
  var unsorted = a.toBuffer
  val sorted = mut.Buffer[A]()
  
  def hasIncomingNode(n: A): Boolean = {
    unsorted.exists(it => pord.lt(it, n))
  }
  def nodeEnters(inc: A, n: A): Boolean = {
    pord.lt(inc, n)
  }
  
  val startNodes: mut.Set[A] = a.filterNot(hasIncomingNode).iterator.to(mut.Set.iterableFactory)
  
  while (startNodes.nonEmpty) do
    val n = startNodes.head
    startNodes.remove(n)
    sorted.append(n)
    unsorted = unsorted.filterNot(_ == n)
    unsorted.withFilter(it => nodeEnters(n, it)).foreach: m =>
      if !hasIncomingNode(m) then
        startNodes.add(m)

  Option.when(unsorted.isEmpty)(sorted.toList)


def debugTiming[A](func: => A): A =
  val start = System.nanoTime()
  val res = func
  val end = System.nanoTime()
  println(s"Elapsed: ${(end - start).toDouble / 1000000.0}ms")
  res

extension[A](f: A => A)
  def repeated(n: Int): A => A =
    Iterator.iterate(_)(f).drop(n).next()

def repeat(times: Int)(block: => Unit): Unit =
  (0 until times).foreach(_ => block)

extension[A, G[_]](f: A => G[A])(using Monad[G])
  def flatRepeated(n: Int): A => G[A] =
    Kleisli.endoMonoidK[G].algebra[A].combineN(Kleisli[G, A, A](f), n).run

// may not be unique
def prettyCharForNum(num: Int): Char =
  if num < 10 then ('0' + num).toChar
  else if num < 10 + 26 then ('a' + num - 10).toChar
  else if num < 10 + 26 + 26 then ('A' + num - 10 - 26).toChar
  else '?'

def logBaseN(n: Double, base: Double): Double = math.log(n) / math.log(base)

extension[A, B] (map: mut.HashMap[A, B])
  def memo(in: A)(func: => B): B = map.getOrElseUpdate(in, func)

private def bfsImpl[A, B, C](a: A, z: C, append: (C, B) => C)(f: A => Either[Iterable[A], B]): C =
  var result = z
  val queue = mut.Queue(a)
  while queue.nonEmpty do
    f(queue.dequeue()) match
      case Right(r) => result = append(result, r)
      case Left(states) => queue.enqueueAll(states)
  result

def bfsFoldl[A, B](a: A)(f: A => Either[Iterable[A], B])(using M: Monoid[B]): B =
  bfsImpl(a, M.empty, M.combine)(f)
  
extension[A] (set: scala.collection.Set[A]) {
  def ⊆(that: scala.collection.Set[A]): Boolean = set.subsetOf(that)
}

extension[A] (self: Iterable[A])
  def findMap[B](f: A => Option[B]): Option[B] = self.collectFirst(f.unlift)

extension[A, B] (self: Either[A, B])
  def leftOrDie: A = self.swap.getOrElse(throw new Exception("Right.leftOrDie"))

  def rightOrDie: B = self.getOrElse(throw new Exception("Left.rightOrDie"))

extension[A] (self: Iterator[A])
  def findMap[B](f: A => Option[B]): Option[B] =
    val i = self.flatMap(f)
    Option.when(i.hasNext)(i.next())


extension[A] (self: Vector[A])
  def unsnoc: (Vector[A], A) = (self.init, self.last)

object Unsnoc:
  def unapply[A](vec: Vector[A]): Option[(Vector[A], A)] =
    Option.when(vec.nonEmpty)(vec.unsnoc)

@tailrec
def unfoldedMap[A, S](init: S)(f: S => Either[A, S]): A =
  f(init) match
    case Left(a) => a
    case Right(s) => unfoldedMap(s)(f)
    
def unfolded[A, S](init: S)(f: S => Option[(A, S)]): A =
  @tailrec
  def go(state: S, acc: Option[A]): Option[A] =
    f(state) match
      case None => acc
      case Some((a, v)) => go(v, Some(a))
  go(init, None).get

object ListExt:
  def segmentBy[A](ls: List[A])(f: (A, A) => Boolean): List[List[A]] =
    @tailrec def go(cur: List[A], acc: List[List[A]]): List[List[A]] =
      cur match
        case Nil => acc.reverse
        case x :: xs =>
          val (ys,zs) = xs.span(it => f(x, it))
          go(zs, (x :: ys) :: acc)
    go(ls, Nil)

extension[A] (self: List[A])
  // haskell's `groupBy`
  inline def segmentBy(f: (A, A) => Boolean): List[List[A]] = ListExt.segmentBy(self)(f)
  // haskell's `group`
  def segmented(using eq: Eq[A]): List[List[A]] =
    segmentBy(eq.eqv)

extension[A] (self: Array[A])
  def collectFirstSome[B](f: A => Option[B]): Option[B] =
    self.collectFirst(Function.unlift(f))

object whatTheScallop:
  def ! : Nothing = throw AssertionError("WHAT THE SCALLOP?!?")