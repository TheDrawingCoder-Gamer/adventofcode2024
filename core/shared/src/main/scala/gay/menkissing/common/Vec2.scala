package gay.menkissing.common

import spire.algebra.*
import spire.implicits.*
import cats.collections.Discrete
import cats.collections.syntax.range.*
import cats.kernel.UnboundedEnumerable

final case class Vec2[@specialized(Specializable.Bits32AndUp) A](x: A, y: A)(using ring: Ring[A], signed: Signed[A]):
  final def +(that: Vec2[A]): Vec2[A] =
    Vec2(ring.plus(x, that.x), ring.plus(y, that.y))

  final def -(that: Vec2[A]): Vec2[A] =
    Vec2(ring.minus(this.x, that.x), ring.minus(this.x, that.x))
  
  final def *(that: A): Vec2[A] =
    Vec2(this.x * that, this.y * that)
  
  final def taxiDistance(that: Vec2[A]): A =
    signed.abs((this.x - that.x))

  def cardinalNeighbors(using dis: Discrete[A]): List[Vec2[A]] =
      List(
        Vec2[A](dis.pred(x), y),
        Vec2[A](dis.succ(x), y),
        Vec2[A](x, dis.pred(y)),
        Vec2[A](x, dis.succ(y))
      )
  def allNeighbors(using eq: Eq[A], dis: Discrete[A]): List[Vec2[A]] =
    for
      x <- List(dis.pred(this.x), this.x, dis.succ(this.x))
      y <- List(dis.pred(this.y), this.y, dis.succ(this.y))
      if x =!= this.x || y =!= this.y
    yield Vec2(x, y)
  
  def offset(dir: Direction2D, n: A): Vec2[A] =
    dir match
      case Direction2D.Up => this.copy(y = y - n)
      case Direction2D.Down => this.copy(y = y + n)
      case Direction2D.Left => this.copy(x = x - n)
      case Direction2D.Right => this.copy(x = x + n)
  
  def offset(dir: Direction2D)(using dis: Discrete[A]): Vec2[A] =
    dir match
      case Direction2D.Up => this.copy(y = dis.pred(y))
      case Direction2D.Down => this.copy(y = dis.succ(y))
      case Direction2D.Left => this.copy(x = dis.pred(x))
      case Direction2D.Right => this.copy(x = dis.succ(x))
  
  def stepsTowards(that: Vec2[A])(using PartialOrder[A]): Vector[Direction2D] =
      Option.when(this.x =!= that.x)(if this.x > that.x  then Direction2D.Left else Direction2D.Right).toVector
        ++ Option.when(this.y =!= that.y)(if this.y > that.y then Direction2D.Up else Direction2D.Down)

object Vec2:
  given eqVec2[@specialized(Specializable.Bits32AndUp) A](using Eq[A]): Eq[Vec2[A]] with
    def eqv(x: Vec2[A], y: Vec2[A]): Boolean =
      x.x === y.x && x.y === y.y

    