package gay.menkissing.common

import algebra.ring.*
import algebra.instances.all.*
import cats.*
import cats.syntax.all.*

trait VecN[V[_]]:
  def dimensions: Int

  def axis[A](i: Int)(using ring: Ring[A]): V[A]

  extension [A](self: V[A])
    // equivilant to constructing from axes.map(f)
    def map(f: A => A): V[A]
    def zip(that: V[A])(f: (A, A) => A): V[A]

    // Forall V[_], axes.length will be the same
    // (no Vecs that can be any length)
    def axes: List[A]

    def coord(i: Int): A

    def withCoord(i: Int, v: A): V[A]

    final infix def dot(that: V[A])(using ring: Ring[A]): A =
      self.axes.zip(that.axes).map((l, r) => ring.times(l, r)).reduce(ring.plus)

    final infix def +(that: V[A])(using addsg: AdditiveSemigroup[A]): V[A] =
      self.zip(that)(addsg.plus)

    final infix def -(that: V[A])(using addg: AdditiveGroup[A]): V[A] =
      self.zip(that)(addg.minus)

    final infix def *(that: A)(using mulsg: MultiplicativeSemigroup[A]): V[A] =
      self.map(mulsg.times(_, that))

    final infix def taxiDistance
      (that: V[A])
      (using addg: AdditiveGroup[A], signed: Signed[A]): A =
      self.axes.zip(that.axes).map((l, r) => signed.abs(addg.minus(l, r)))
        .reduce(addg.plus)

    final def unary_-(using addg: AdditiveGroup[A]): V[A] =
      self.map(addg.negate)

    final infix def min(that: V[A])(using ord: Order[A]): V[A] =
      self.zip(that)(ord.min)

    final infix def max(that: V[A])(using ord: Order[A]): V[A] =
      self.zip(that)(ord.max)

object VecN:
  def axis[V[_]](using vn: VecN[V])[A](n: Int)(using ring: Ring[A]): V[A] =
    vn.axis(n)
