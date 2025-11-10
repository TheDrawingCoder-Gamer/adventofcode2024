package gay.menkissing.common

import cats.collections.Discrete
import cats.collections.Range
import cats.collections.syntax.range.*
import cats.*
import cats.syntax.all.*
import algebra.ring.*
import algebra.instances.*
import cats.derived.*

final case class Vec2[A](x: A, y: A) derives VecN, Eq:
  def stepsTowards(that: Vec2[A])(using PartialOrder[A]): Vector[Direction2D] =
    Option.when(this.x =!= that.x)(
      if this.x > that.x then Direction2D.Left else Direction2D.Right
    ).toVector ++ Option.when(this.y =!= that.y)(
      if this.y > that.y then Direction2D.Up else Direction2D.Down
    )

  def straightLine
    (that: Vec2[A])
    (using ord: Order[A], dis: Discrete[A], ring: Ring[A]): List[Vec2[A]] =
    require(this.x === that.x || this.y === that.y)
    val shouldReverse =
      (this.x - that.x > ring.zero) || (this.y - that.y > ring.zero)
    def maybeReverse[A](ls: List[A]): List[A] =
      if shouldReverse then ls.reverse
      else ls
    if this.x === that.x then
      val minY = this.y `min` that.y
      val maxY = this.y `max` that.y
      maybeReverse(Range[A](minY, maxY).toList.map(yy => Vec2(x, yy)))
    else
      val minX = this.x `min` that.x
      val maxX = this.x `max` that.x
      maybeReverse(Range(minX, maxX).toList.map(xx => Vec2(xx, y)))

object Vec2:
  given showVec2[A](using Show[A]): Show[Vec2[A]] with
    def show(t: Vec2[A]): String = show"Vec2(${t.x}, ${t.y})"

  given readVec2[A](using ra: Read[A]): Read[Vec2[A]] with
    def read(str: String): Option[Vec2[A]] =
      str match
        case s"$x,$y" =>
          (ra.read(x.trim), ra.read(y.trim)).mapN((xx, yy) => Vec2(xx, yy))
        case s"Vec2($x,$y)" =>
          (ra.read(x.trim), ra.read(y.trim)).mapN(Vec2.apply)
        case _ => None

  def unapply[A](str: String)(using Read[A]): Option[Vec2[A]] =
    readVec2[A].read(str)
