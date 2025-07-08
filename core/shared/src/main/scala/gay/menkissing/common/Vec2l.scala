package gay.menkissing.common

import scala.annotation.targetName
import scala.math.Ordering.Implicits.infixOrderingOps

// THIS SHIT IS NOT ORDERED!!
case class Vec2l(x: Long, y: Long) {
  def offset(dir: Direction2D, n: Int = 1): Vec2l =
    dir match
      case Direction2D.Up => this.copy(y = y + n)
      case Direction2D.Down => this.copy(y = y - n)
      case Direction2D.Left => this.copy(x = x - n)
      case Direction2D.Right => this.copy(x = x + n)
  @targetName("add")
  final def +(that: Vec2l): Vec2l = {
    Vec2l(this.x + that.x, this.y + that.y)
  }
  final def taxiDistance(that: Vec2l): Long = {
    Math.abs(this.x - that.x) + Math.abs(this.y - that.y)
  }
  def straightLine(that: Vec2l): List[Vec2l] = {
    require(this.x == that.x || this.y == that.y)
    val shouldReverse = (this.x - that.x > 0) || (this.y - that.y > 0)
    def maybeReverse[A](ls: List[A]): List[A] = {
      if (shouldReverse)
        ls.reverse 
      else 
        ls
    }
    if (this.x == that.x) {
      val minY = this.y `min` that.y 
      val maxY = this.y `max` that.y 
      maybeReverse((minY to maxY).map(yy => Vec2l(this.x, yy)).toList)
    } else { 
      val minX = this.x `min` that.x 
      val maxX = this.x `max` that.x 
      maybeReverse((minX to maxX).map(xx => Vec2l(xx, this.y)).toList)
    }
  }
}

