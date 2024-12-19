import gay.menkissing.advent.ProblemAdv
import scala.io.Source
import scala.collection.mutable as mut



object Day14 extends ProblemAdv[List[Day14.Robot], Int, Int] {
  extension (self: Int) {
    infix def rem(that: Int): Int = {
      // these don't cancel out due to floor div
      self - that.abs * (self.toDouble / that.toDouble.abs).floor.toInt
    }
  }
  case class Vec2i(x: Int, y: Int)
  case class GridSize(x: Int, y: Int)

  case class Robot(pos: Vec2i, velocity: Vec2i) {
    def stepN(n: Int = 1)(using size: GridSize): Robot = {
      copy(pos = pos.copy(x = (pos.x + n * velocity.x) rem size.x, y = (pos.y + n * velocity.y) rem size.y))
    }
  }

  def longestSubseq[A](seq: List[A], item: A): Int =
    seq.foldLeft((0, 0)):
      case ((max, count), a) =>
        if item == a then
          (math.max(max, count + 1), count + 1)
        else
          (max, 0)
   ._1

  extension (robots: List[Robot])(using size: GridSize) {

    def stepN(n: Int = 1): List[Robot] = robots.map(_.stepN(n))
    def safety: Int = {
      val middleX = (size.x / 2)
      val middleY = (size.y / 2)

      robots.groupBy { robot =>
        if (robot.pos.x == middleX || robot.pos.y == middleY) {
          -1
        } else if (robot.pos.x < middleX) {
          if (robot.pos.y < middleY) {
            0
          } else {
            2
          }
        } else {
          if (robot.pos.y < middleY) {
            1
          } else {
            3
          }
        }
      }.removed(-1).values.map(_.length).product
    }
    def pretty: String = {
      val grid = robots.robotMap

      grid.map(_.map {
        case 0 => '.'
        case _ => '#'
      }.mkString("", "", "")).mkString("", "\n", "")
    }

    def findEasterEgg: Int =
      (0 to 10000).find { i =>
        val newRobots = robots.stepN(i)
        if (newRobots.groupBy(_.pos.x).map(_._2.length).max > 15 && newRobots.groupBy(_.pos.y).map(_._2.length).max > 15) {

          val xLineMembers = newRobots.groupBy(_.pos.x).maxBy(_._2.length)._2.map(_.pos.y).toSet
          val yLineMembers = newRobots.groupBy(_.pos.y).maxBy(_._2.length)._2.map(_.pos.x).toSet
          val xLine = (0 until size.y).map(it => xLineMembers.contains(it)).toList
          val yLine = (0 until size.x).map(it => yLineMembers.contains(it)).toList
          longestSubseq(xLine, true) > 15 && longestSubseq(yLine, true) > 15
        } else false
      }.getOrElse(-1)

    def robotMap: Vector[Vector[Int]] = {
      val goodGrid = mut.ArrayBuffer.fill(size.y, size.x)(0)

      robots.foreach { robot =>
        goodGrid(robot.pos.y)(robot.pos.x) = goodGrid(robot.pos.y)(robot.pos.x) + 1
      }

      goodGrid.map(_.toVector).toVector
    }

  }
  override def parse(str: String): List[Robot] = {
    str.linesIterator.map { case s"p=$px,$py v=$vx,$vy" => Robot(Vec2i(px.toInt, py.toInt), Vec2i(vx.toInt, vy.toInt)) }.toList
  }

  // given size: GridSize = GridSize(11, 7)
  given size: GridSize = GridSize(101, 103)
  override def part1(input: List[Robot]): Int =  {

    val r = input.stepN(100)
    // println(r.pretty)
    r.safety
  }

  override def part2(input: List[Robot]): Int = {
    input.findEasterEgg
  }

  lazy val input: String = Source.fromResource("day14.txt").mkString.trim
}

/*
@main def main(): Unit = {
  println(Day14.fullPart1)
  println(Day14.fullPart2)
}
 */
