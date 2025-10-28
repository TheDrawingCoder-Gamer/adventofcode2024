package gay.menkissing.common

import spire.std.any.*



type Vec2i = Vec2[Int]

object Vec2i:
  def apply(x: Int, y: Int) = new Vec2(x, y)

  extension (self: Vec2i)
    def straightLine(that: Vec2i): List[Vec2i] = {
      require(self.x == that.x || self.y == that.y)
      val shouldReverse = (self.x - that.x > 0) || (self.y - that.y > 0) 
      def maybeReverse[A](ls: List[A]): List[A] = {
        if (shouldReverse)
          ls.reverse 
        else 
          ls
      }
      if (self.x == that.x) {
        val minY = self.y `min` that.y 
        val maxY = self.y `max` that.y 
        maybeReverse((minY to maxY).map(yy => Vec2i(self.x, yy)).toList)
      } else { 
        val minX = self.x `min` that.x 
        val maxX = self.x `max` that.x 
        maybeReverse((minX to maxX).map(xx => Vec2i(xx, self.y)).toList)
      }
    }

    

