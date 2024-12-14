import gay.menkissing.common.*
import gay.menkissing.advent.Problem

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.io.Source
import scala.collection.mutable as mut

case class GridSize(x: Int, y: Int)

case class Robot(pos: Vec2i, velocity: Vec2i) {
  def stepN(n: Int = 1)(using size: GridSize): Robot = {
    copy(pos = pos.copy(x = (pos.x + n * velocity.x) rem size.x, y = (pos.y + n * velocity.y) rem size.y))
  }
}

extension (robots: List[Robot])(using size: GridSize) {
  def hasRect: Boolean = {
    val grid = robots.asGrid

    grid.zipWithIndices.filterNot(_._1 == 0).exists { (_, p) =>
      val rightPos = Iterator.iterate(p)(_.offset(Direction2D.Right)).takeWhile(p1 => grid.get(p1.x, p1.y).exists(_ > 0)).toList.last
      val bottomPos = Iterator.iterate(p)(_.offset(Direction2D.Down)).takeWhile(p1 => grid.get(p1.x, p1.y).exists(_ > 0)).toList.last

      if (p != rightPos && p != bottomPos && grid.isDefinedAt(rightPos.x, rightPos.y) && grid.isDefinedAt(bottomPos.x, bottomPos.y)) {
        assert(grid(rightPos.x, rightPos.y) > 0)
        assert(grid(bottomPos.x, bottomPos.y) > 0)

        val bottomRight = Vec2i(rightPos.x, bottomPos.y)
        val sizeX = rightPos.x - p.x
        val sizeY = rightPos.y - p.y

        sizeX > 5 &&
          sizeY > 5 &&
          grid(rightPos.x, bottomPos.y) > 0 &&
          p.straightLine(rightPos).forall(it => grid(it.x, it.y) > 0) &&
          p.straightLine(bottomPos).forall(it => grid(it.x, it.y) > 0) &&
          rightPos.straightLine(bottomRight).forall(it => grid(it.x, it.y) > 0) &&
          bottomPos.straightLine(bottomRight).forall(it => grid(it.x, it.y) > 0)
      } else false
    }
  }
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

    grid.map(_.map(prettyCharForNum.andThen { case '0' => '.' case i => i }).mkString("", "", "")).mkString("", "\n", "")
  }
  def robotMap: Vector[Vector[Int]] = {
    val goodGrid = mut.ArrayBuffer.fill(size.y, size.x)(0)

    robots.foreach { robot =>
      goodGrid(robot.pos.y)(robot.pos.x) = goodGrid(robot.pos.y)(robot.pos.x) + 1
    }

    goodGrid.map(_.toVector).toVector
  }
  def asGrid: Grid[Int] = Grid(robotMap)

  def asImage: BufferedImage =
    val image = BufferedImage(size.x, size.y, BufferedImage.TYPE_INT_RGB)
    val g = image.createGraphics()
    val rset = robots.map(_.pos).toSet
    for
      x <- 0 until size.x
      y <- 0 until size.y
    do
      g.setColor(
        if rset(Vec2i(x, y)) then java.awt.Color.BLACK else java.awt.Color.WHITE,
      )
      g.fillRect(x, y, 1, 1)
    g.dispose()

    image
}

object Day14 extends Problem[List[Robot], Int] {
  override def parse(str: String): List[Robot] = {
    str.linesIterator.map { case s"p=$px,$py v=$vx,$vy" => Robot(Vec2i(px.toInt, py.toInt), Vec2i(vx.toInt, vy.toInt)) }.toList
  }

  // given size: GridSize = GridSize(11, 7)
  given size: GridSize = GridSize(101, 103)
  override def part1(input: List[Robot]): Int =  {

    val r = input.map(_.stepN(100))
    // println(r.pretty)
    r.safety
  }

  override def part2(input: List[Robot]): Int = {
    val startX = 7990
    Iterator.iterate(input.stepN(startX))(_.stepN()).zipWithIndex.take(100).foreach { (rs, p) =>
      ImageIO.write(rs.asImage, "png", new File(f"img/2024/14/${p + startX}%05d.png"))
    }

    -1

  }

  lazy val input: String = Source.fromResource("day14.txt").mkString.trim
}

@main def main(): Unit = {
  println(-2 rem 6)
  println(Day14.fullPart1)
  println(Day14.fullPart2)
}