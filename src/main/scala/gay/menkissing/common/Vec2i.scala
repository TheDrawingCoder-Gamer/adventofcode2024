package gay.menkissing.common
import scala.annotation.targetName
import scala.math.Ordering.Implicits.infixOrderingOps
// THIS SHIT IS NOT ORDERED!!
case class Vec2i(x: Int, y: Int) extends Ordered[Vec2i] {
  // I flipped this vertically BUT I TESTED THIS SHIT!!!!
  def offset(dir: Direction2D, n: Int = 1): Vec2i =
    dir match
      case Direction2D.Up => this.copy(y = y - n)
      case Direction2D.Down => this.copy(y = y + n)
      case Direction2D.Left => this.copy(x = x - n)
      case Direction2D.Right => this.copy(x = x + n)
  override def compare(that: Vec2i): Int = {
    if (this.x - that.x != 0) 
      this.x - that.x 
    else 
      this.y - that.y
  }
  @targetName("add")
  final def +(that: Vec2i): Vec2i = {
    Vec2i(this.x + that.x, this.y + that.y)
  }
  
  @targetName("diff")
  final def -(that: Vec2i): Vec2i = {
    Vec2i(this.x - that.x, this.y - that.y)
  }
  
  final def taxiDistance(that: Vec2i): Int = {
    Math.abs(this.x - that.x) + Math.abs(this.y - that.y)
  }
  def straightLine(that: Vec2i): List[Vec2i] = {
    require(this.x == that.x || this.y == that.y)
    val shouldReverse = (this `max` that) == this 
    def maybeReverse[A](ls: List[A]): List[A] = {
      if (shouldReverse)
        ls.reverse 
      else 
        ls
    }
    if (this.x == that.x) {
      val minY = this.y `min` that.y 
      val maxY = this.y `max` that.y 
      maybeReverse((minY to maxY).map(yy => Vec2i(this.x, yy)).toList)
    } else { 
      val minX = this.x `min` that.x 
      val maxX = this.x `max` that.x 
      maybeReverse((minX to maxX).map(xx => Vec2i(xx, this.y)).toList)
    }
  }
  
  def cardinalNeighbors: List[Vec2i] =
    List(Vec2i(x - 1, y), Vec2i(x + 1, y), Vec2i(x, y - 1), Vec2i(x, y + 1))
  
  def toLong: Vec2l = Vec2l(x.toLong, y.toLong)

  def stepsTowards(that: Vec2i): Vector[Direction2D] =
    val mag = that - this
    Option.when(mag.x != 0)(if mag.x < 0 then Direction2D.Left else Direction2D.Right).toVector
      ++ Option.when(mag.y != 0)(if mag.y < 0 then Direction2D.Up else Direction2D.Down)
}

