package gay.menkissing.common

import scala.annotation.tailrec
import scala.collection.mutable

import cats.*
import cats.implicits.*
import cats.data.Kleisli
import cats.data.NonEmptyList

def topologicalSort[A]
  (a: List[A])
  (using pord: PartialOrder[A]): Option[List[A]] =
  var unsorted = a.toBuffer
  val sorted = mutable.Buffer[A]()

  def hasIncomingNode(n: A): Boolean = unsorted.exists(it => pord.lt(it, n))
  def nodeEnters(inc: A, n: A): Boolean = pord.lt(inc, n)

  val startNodes: mutable.Set[A] =
    a.filterNot(hasIncomingNode).iterator.to(mutable.Set.iterableFactory)

  while startNodes.nonEmpty do
    val n = startNodes.head
    startNodes.remove(n)
    sorted.append(n)
    unsorted = unsorted.filterNot(_ == n)
    unsorted.withFilter(it => nodeEnters(n, it)).foreach: m =>
      if !hasIncomingNode(m) then startNodes.add(m)

  Option.when(unsorted.isEmpty)(sorted.toList)

def debugTiming[A](func: => A): A =
  val start = System.nanoTime()
  val res = func
  val end = System.nanoTime()
  println(s"Elapsed: ${(end - start).toDouble / 1000000.0}ms")
  res

extension [A](f: A => A)
  def repeated(n: Int): A => A = Iterator.iterate(_)(f).drop(n).next()

def repeat(times: Int)(block: => Unit): Unit = (0 until times)
  .foreach(_ => block)

extension [A, G[_]](f: A => G[A])(using Monad[G])
  def flatRepeated(n: Int): A => G[A] =
    Kleisli.endoMonoidK[G].algebra[A].combineN(Kleisli[G, A, A](f), n).run

// may not be unique
def prettyCharForNum(num: Int): Char =
  if num < 10 then ('0' + num).toChar
  else if num < 10 + 26 then ('a' + num - 10).toChar
  else if num < 10 + 26 + 26 then ('A' + num - 10 - 26).toChar
  else '?'

def logBaseN(n: Double, base: Double): Double = math.log(n) / math.log(base)

extension [A, B](map: mutable.Map[A, B])
  def memo(in: A)(func: => B): B = map.getOrElseUpdate(in, func)

def bfsFoldlG[A, B](a: A)(f: A => Either[Iterable[A], B]): Option[B] =
  var result: Option[B] = None
  val queue = mutable.Queue(a)
  while queue.nonEmpty do
    f(queue.dequeue()) match
      case Right(r)     => result = Some(r)
      case Left(states) => queue.enqueueAll(states)
  result

private def bfsImpl[A, B, C]
  (a: A, z: C, append: (C, B) => C)
  (f: A => Either[Iterable[A], B]): C =
  var result = z
  val queue = mutable.Queue(a)
  while queue.nonEmpty do
    f(queue.dequeue()) match
      case Right(r)     => result = append(result, r)
      case Left(states) => queue.enqueueAll(states)
  result

def bfsFoldl[A, B]
  (a: A)
  (f: A => Either[Iterable[A], B])
  (using M: Monoid[B]): B = bfsImpl(a, M.empty, M.combine)(f)

extension [A](set: scala.collection.Set[A])
  def ⊆(that: scala.collection.Set[A]): Boolean = set.subsetOf(that)

extension [A](self: Iterable[A])
  def findMap[B](f: A => Option[B]): Option[B] = self.collectFirst(f.unlift)

extension [A, B](self: Either[A, B])
  def leftOrDie: A = self.swap.getOrElse(throw new Exception("Right.leftOrDie"))

  def rightOrDie: B = self.getOrElse(throw new Exception("Left.rightOrDie"))

extension [A](self: Iterator[A])
  def findMap[B](f: A => Option[B]): Option[B] =
    val i = self.flatMap(f)
    Option.when(i.hasNext)(i.next())

extension [A](self: Vector[A])
  def unsnoc: (Vector[A], A) = (self.init, self.last)

object Unsnoc:
  def unapply[A](vec: Vector[A]): Option[(Vector[A], A)] =
    Option.when(vec.nonEmpty)(vec.unsnoc)

@tailrec
def unfoldedMap[A, S](init: S)(f: S => Either[A, S]): A =
  f(init) match
    case Left(a)  => a
    case Right(s) => unfoldedMap(s)(f)

def unfolded[A, S](init: S)(f: S => Option[(A, S)]): A =
  @tailrec
  def go(state: S, acc: Option[A]): Option[A] =
    f(state) match
      case None         => acc
      case Some((a, v)) => go(v, Some(a))
  go(init, None).get

object segmentFuncs:
  def segmentBy[F[_], A]
    (fa: F[A])
    (f: (A, A) => Boolean)
    (using fold: Foldable[F]): List[NonEmptyList[A]] =
    val ls = fold.toList(fa)
    @tailrec def go
      (cur: List[A], acc: List[NonEmptyList[A]]): List[NonEmptyList[A]] =
      cur match
        case Nil     => acc.reverse
        case x :: xs =>
          val (ys, zs) = xs.span(it => f(x, it))
          go(zs, NonEmptyList.of(x, ys*) :: acc)
    go(ls, Nil)

extension [F[_], A](self: F[A])(using fold: Foldable[F])
  // haskell's `groupBy`
  inline def segmentBy(f: (A, A) => Boolean): List[NonEmptyList[A]] =
    segmentFuncs.segmentBy(self)(f)
  // haskell's `group`
  def segmented(using eq: Eq[A]): List[NonEmptyList[A]] = segmentBy(eq.eqv)

  def foldString
    (start: String, sep: String, end: String)
    (using show: Show[A]): String =
    val first =
      self.foldLeft(start): (acc, a) =>
        acc + show.show(a) + sep
    first.dropRight(sep.length) + end

  def foldString(sep: String)(using show: Show[A]): String =
    foldString("", sep, "")
  def foldString(using show: Show[A]): String = foldString("", "", "")

  def countWhile(f: A => Boolean): Long =
    fold.foldM[Either[Long, _], A, Long](self, 0L): (i, a) =>
      if !f(a) then Left(i) else Right(i + 1L)
    .merge

extension [F[_]](self: Monad[F])
  def whenM[A](cond: F[Boolean])(ifTrue: => F[A]): F[Unit] =
    self.ifM(cond)(self.as(ifTrue, ()), self.pure(()))

extension [F[_], A](self: F[A])(using monad: Monad[F])
  def whenM(cond: F[Boolean]): F[Unit] = monad.whenM(cond)(self)

extension [F[_]](self: F[Boolean])(using monad: Monad[F])
  def ifMHalf[A](fa: F[A]): F[Unit] = monad.whenM(self)(fa)

extension [A](self: Array[A])
  def collectFirstSome[B](f: A => Option[B]): Option[B] =
    self.collectFirst(Function.unlift(f))

final class ThisShouldntHappenError(val why: String) extends RuntimeException

def !!! : Nothing = throw ThisShouldntHappenError("This shouldn't happen")

opaque type MonadPApp[F[_]] = Monad[F]

extension [F[_]](self: MonadPApp[F])
  def tailRecM[A](a: A)[B](f: A => F[Either[A, B]]): F[B] = self.tailRecM(a)(f)

object MonadPApp:
  def apply[F[_]](using m: Monad[F]) = m

object MonadExt:
  def tailRecM[F[_]: Monad]
    (using DummyImplicit)[A](a: A)[B](f: A => F[Either[A, B]]): F[B] =
    Monad[F].tailRecM(a)(f)
