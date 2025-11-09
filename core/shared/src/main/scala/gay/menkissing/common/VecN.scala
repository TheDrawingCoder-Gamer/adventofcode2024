package gay.menkissing.common

import algebra.ring.*
import algebra.instances.all.*
import cats.*
import cats.syntax.all.*
import scala.deriving.*

trait BareVecN[V[_]]:
  def dimensions: Int

trait VecN[V[_]] extends BareVecN[V]:
  def axis[A](i: Int)(using ring: Ring[A]): V[A] =
    if i >= dimensions then whatTheScallop.!
    else
      construct(
        Vector.tabulate[A](dimensions)(it =>
          if it == i then ring.one else ring.zero
        )
      )
  // If ls.length != dimensions then error : (
  def construct[A](ls: Vector[A]): V[A]

  extension [A](self: V[A])
    // equivilant to constructing from axes.map(f)
    // but if want to go fast override this
    def map(f: A => A): V[A] = construct(axes.map(f))
    def zip(that: V[A])(f: (A, A) => A): V[A] =
      construct(self.axes.zip(that.axes).map(f.tupled))

    // Forall V[_], axes.length will be the same
    // (no Vecs that can be any length)
    def axes: Vector[A]

    def coord(i: Int): A = self.axes(i)

    def withCoord(i: Int, v: A): V[A] = construct(self.axes.updated(i, v))

    def offset[D]
      (d: D, n: A)
      (using dirN: IsDirectionN[D] { type Vec = V }, ring: Ring[A]): V[A] =
      self + d.digitalDir * n

    def allNeighbors(using ring: Ring[A], eq: Eq[A]): List[V[A]] =
      (0 until dimensions).map: dim =>
        List(
          ring.minus(self.coord(dim), ring.one),
          self.coord(dim),
          ring.plus(self.coord(dim), ring.one)
        )
      .toVector.sequence
        .filter(_.zipWithIndex.exists((v, i) => self.coord(i) =!= v))
        .map(construct)

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
