package gay.menkissing.common

import cats.*
import cats.syntax.all.*
import algebra.ring.*
import cats.collections.{Discrete, Range}

final case class Dimension[@specialized(Int, Long) A] private (min: A, max: A):
  infix def intersect(t: Dimension[A])(using ord: Order[A]): Option[Dimension[A]] =
    Option.when(max >= t.min && min <= t.max):
      (min max t.min) dimBy (max min t.max)

  infix def -(t: Dimension[A])(using ord: Order[A], dis: Discrete[A]): Set[Dimension[A]] =
    this intersect t match
      case Some(h) =>
        var daSet = Set.empty[Dimension[A]]
        if min != h.min then
          daSet += (min dimBy dis.pred(h.min))
        if max != h.max then
          daSet += (dis.succ(h.max) dimBy max)
        daSet
      case _ => Set(this)

  infix def -(n: A)(using ord: Order[A], dis: Discrete[A]): Set[Dimension[A]] =
    if this.contains(n) then
      var daSet = Set.empty[Dimension[A]]
      if min != n then
        daSet += (min dimBy dis.pred(n))
      if max != n then
        daSet += (dis.succ(n) dimBy max)
      daSet
    else
      Set(this)

  infix def --(that: Iterable[A])(using ord: Order[A], dis: Discrete[A]): Set[Dimension[A]] =
    that.foldLeft(Set(this)): (acc, n) =>
      acc.flatMap(_ - n)

  def fitsIn(d: Dimension[A])(using ord: Order[A]): Boolean =
    min >= d.min && max <= d.max

  def contains(n: A)(using ord: Order[A]): Boolean =
    n >= min && n <= max

  def length(using rng: Rng[A], discrete: Discrete[A]): A = discrete.succ(rng.minus(max, min))

  def combine(t: Dimension[A])(using Order[A], Discrete[A]): Set[Dimension[A]] =
    this intersect t match
      case Some(v) => (t - v) ++ (this - v) + t
      case _ => Set(this, t)

  def values(using Order[A], Discrete[A]): Seq[A] =
    Range(min, max).toList

object Dimension:
  def apply[A](min: A, max: A)(using ord: Order[A]): Dimension[A] =
    new Dimension(ord.min(min, max), ord.max(min, max))

  given showDimension[A](using Show[A]): Show[Dimension[A]] = dim =>
    show"${dim.min}..${dim.max}"

extension[A] (self: A)(using Order[A])
  infix def dimBy(that: A): Dimension[A] =
    Dimension(self, that)
