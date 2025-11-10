package gay.menkissing.common

import cats.collections.Discrete
import cats.collections.Range
import cats.collections.syntax.range.*
import cats.*
import cats.syntax.all.*
import algebra.ring.*
import algebra.instances.*

final case class Vec2[A](x: A, y: A):
  def cardinalNeighbors(using ring: Ring[A]): List[Vec2[A]] =
    List(
      Vec2[A](x - ring.one, y),
      Vec2[A](x + ring.one, y),
      Vec2[A](x, y - ring.one),
      Vec2[A](x, y + ring.one)
    )

  def offset(dir: Direction2D, n: A)(using AdditiveGroup[A]): Vec2[A] =
    dir match
      case Direction2D.Up    => this.copy(y = y - n)
      case Direction2D.Down  => this.copy(y = y + n)
      case Direction2D.Left  => this.copy(x = x - n)
      case Direction2D.Right => this.copy(x = x + n)

  def offset(dir: Direction2D)(using ring: Ring[A]): Vec2[A] =
    dir match
      case Direction2D.Up    => this.copy(y = y - ring.one)
      case Direction2D.Down  => this.copy(y = y + ring.one)
      case Direction2D.Left  => this.copy(x = x - ring.one)
      case Direction2D.Right => this.copy(x = x + ring.one)

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
  given eqVec2[A](using Eq[A]): Eq[Vec2[A]] with
    def eqv(x: Vec2[A], y: Vec2[A]): Boolean = x.x === y.x && x.y === y.y

  given showVec2[A](using Show[A]): Show[Vec2[A]] with
    def show(t: Vec2[A]): String = show"Vec2(${t.x}, ${t.y})"

  given vecNVec2: VecN[Vec2] with
    def dimensions: Int = 2

    override def axis[A](i: Int)(using ring: Ring[A]): Vec2[A] =
      i match
        case 0 => Vec2(ring.one, ring.zero)
        case 1 => Vec2(ring.zero, ring.one)
        case _ => whatTheScallop.!

    extension [A](self: Vec2[A])

      override def coord(i: Int): A =
        i match
          case 0 => self.x
          case 1 => self.y
          case _ => whatTheScallop.!

      override def withCoord(i: Int, v: A): Vec2[A] =
        i match
          case 0 => self.copy(x = v)
          case 1 => self.copy(y = v)
          case _ => whatTheScallop.!

      override def allNeighbors(using ring: Ring[A], eq: Eq[A]): List[Vec2[A]] =
        VecN.defaultAllNeighbors[Vec2](self)

      def axes: Vector[A] = Vector(self.x, self.y)
      override def map(f: A => A): Vec2[A] = Vec2(f(self.x), f(self.y))
      override def zip(that: Vec2[A])(f: (A, A) => A): Vec2[A] =
        Vec2(f(self.x, that.x), f(self.y, that.y))
