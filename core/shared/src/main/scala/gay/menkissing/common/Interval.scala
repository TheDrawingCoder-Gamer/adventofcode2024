package gay.menkissing.common

import cats.*
import cats.syntax.all.*
import algebra.ring.*
import cats.collections.{Discrete, Range}

final case class Interval[A] private (min: A, max: A):
  infix def intersect
    (
      t: Interval[A]
    )
    (using ord: Order[A]): Option[Interval[A]] =
    Option.when(max >= t.min && min <= t.max):
      Interval(min max t.min, max min t.max)(using ord)

  infix def -
    (
      t: Interval[A]
    )
    (using ord: Order[A], dis: Discrete[A]): Set[Interval[A]] =
    this intersect t match
      case Some(h) =>
        var daSet = Set.empty[Interval[A]]
        if min != h.min then daSet += Interval(min, dis.pred(h.min))(using ord)
        if max != h.max then daSet += Interval(dis.succ(h.max), max)(using ord)
        daSet
      case _ => Set(this)

  infix def -(n: A)(using ord: Order[A], dis: Discrete[A]): Set[Interval[A]] =
    if this.contains(n) then
      var daSet = Set.empty[Interval[A]]
      if min != n then daSet += Interval(min, dis.pred(n))(using ord)
      if max != n then daSet += Interval(dis.succ(n), max)(using ord)
      daSet
    else Set(this)

  infix def --
    (
      that: Iterable[A]
    )
    (using ord: Order[A], dis: Discrete[A]): Set[Interval[A]] =
    that.foldLeft(Set(this)): (acc, n) =>
      acc.flatMap(_ - n)

  def fitsIn(d: Interval[A])(using ord: Order[A]): Boolean =
    min >= d.min && max <= d.max

  def contains(n: A)(using ord: Order[A]): Boolean = n >= min && n <= max

  def length(using ring: Ring[A]): A = ring.minus(max, min) + ring.fromInt(1)

  def combine(t: Interval[A])(using Order[A], Discrete[A]): Set[Interval[A]] =
    this intersect t match
      case Some(v) => (t - v) ++ (this - v) + t
      case _       => Set(this, t)

  def values(using Order[A], Discrete[A]): Seq[A] = Range(min, max).toList

object Interval:
  def apply[A](min: A, max: A)(using ord: Order[A]): Interval[A] =
    new Interval(ord.min(min, max), ord.max(min, max))

  given showInterval[A](using Show[A]): Show[Interval[A]] =
    dim => show"${dim.min}..${dim.max}"
