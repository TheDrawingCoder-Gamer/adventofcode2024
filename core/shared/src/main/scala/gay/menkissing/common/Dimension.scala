package gay.menkissing.common

import cats.*
import cats.implicits.*
import algebra.ring.*
import cats.collections.Discrete

final case class Dimension[@specialized(Int, Long) A] private (min: A, max: A):
  infix def intersect(t: Dimension[A])(using ord: Order[A]): Option[Dimension[A]] =
    Option.when(max >= t.min && min <= t.max):
      (min max t.min) dimBy (max min t.max)

  def fitsIn(d: Dimension[A])(using ord: Order[A]): Boolean =
    min >= d.min && max <= d.max

  def length(using rng: Rng[A], discrete: Discrete[A]): A = discrete.succ(rng.minus(max, min))

object Dimension:
  def apply[A](min: A, max: A)(using ord: Order[A]): Dimension[A] =
    new Dimension(ord.min(min, max), ord.max(min, max))

  given showDimension[A](using Show[A]): Show[Dimension[A]] = dim =>
    show"${dim.min}..${dim.max}"

extension[A] (self: A)(using Order[A])
  infix def dimBy(that: A): Dimension[A] =
    Dimension(self, that)
