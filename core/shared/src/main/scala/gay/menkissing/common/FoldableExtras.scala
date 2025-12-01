package gay.menkissing.common

import cats.*
import cats.data.*
import cats.syntax.all.*

import scala.annotation.tailrec

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
    self.shortCircuitFoldLeft[Long](0L): (i, a) =>
      if !f(a) then Done(i) else Continue(i + 1L)

  /**
   * A fold that can short circuit when returning a `Done`.
   */
  def shortCircuitFoldLeft[B](b: B)(f: (B, A) => StepResult[B]): B =
    self.foldShortCircuitGen(b)(f).merge

  private def foldShortCircuitGen[B]
    (b: B)
    (f: (B, A) => StepResult[B]): StepResult[B] =
    fold.foldM[StepResult, A, B](self, b): (acc, v) =>
      f(acc, v)

  def foldFind[B](b: B)(f: (B, A) => StepResult[B]): Option[B] =
    foldShortCircuitGen(b)(f).left.toOption

  // Fold and automatically collect the results for each fold operation...
  def accumFoldLeft[B](b: B)(f: (B, A) => B): List[B] =
    fold.foldLeft(self, (b, List.empty[B])):
      case ((acc, accum), a) =>
        val r = f(acc, a)
        (r, r :: accum)
    ._2.reverse
