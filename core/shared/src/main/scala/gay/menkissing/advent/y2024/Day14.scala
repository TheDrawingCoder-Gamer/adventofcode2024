package gay.menkissing.advent.y2024

import gay.menkissing.advent.{FileIO, ProblemAdv}

import scala.collection.mutable as mut
import scala.io.Source



object Day14 extends ProblemAdv[List[Day14.Robot], Int, Int] {
  extension (self: Int)
    infix def rem(that: Int): Int =
      val m = math.abs(self) % that
      if self < 0 then
        that - m
      else
        m

  case class Vec2i(x: Int, y: Int)

  case class Robot(pos: Vec2i, velocity: Vec2i):
    def stepN(n: Int = 1): Robot =
      copy(pos = pos.copy(x = (pos.x + n * velocity.x) rem size.x, y = (pos.y + n * velocity.y) rem size.y))

  def longestSubseq[A](seq: List[A], item: A): Int =
    seq.foldLeft((0, 0)):
      case ((max, count), a) =>
        if item == a then
          (math.max(max, count + 1), count + 1)
        else
          (max, 0)
   ._1

  extension (robots: List[Robot]) {

    def stepN(n: Int = 1): List[Robot] = robots.map(_.stepN(n))
    def safety: Int =
      val middleX = (size.x / 2)
      val middleY = (size.y / 2)

      robots.groupBy: robot =>
        (robot.pos.x.compareTo(middleX), robot.pos.y.compareTo(middleY)) match
          case (0, _) | (_, 0) => -1
          case ( 1, -1) => 0
          case (-1, -1) => 1
          case (-1,  1) => 2
          case ( 1,  1) => 3
      .removed(-1).values.map(_.length).product
    def pretty: String = {
      val grid = robots.robotMap

      grid.map(_.map {
        case 0 => '.'
        case _ => '#'
      }.mkString("", "", "")).mkString("", "\n", "")
    }

    def findEasterEgg: Int =
      (0 to 10403).find: i =>
        val newRobots = robots.stepN(i)
        newRobots.groupBy(_.pos.y).count(_._2.length >= 10) > 15 && newRobots.groupBy(_.pos.x).count(_._2.length >= 15) >= 3
      .getOrElse(-1)

    def robotMap: Vector[Vector[Int]] = {
      val goodGrid = mut.ArrayBuffer.fill(size.y + 1, size.x + 1)(0)

      robots.foreach { robot =>
        goodGrid(robot.pos.y)(robot.pos.x) = goodGrid(robot.pos.y)(robot.pos.x) + 1
      }

      goodGrid.map(_.toVector).toVector
    }

  }
  override def parse(str: String): List[Robot] =
    str.linesIterator.map:
      case s"p=$px,$py v=$vx,$vy" =>
        Robot(Vec2i(px.toInt, py.toInt), Vec2i(vx.toInt, vy.toInt))
    .toList


  // given size: GridSize = GridSize(11, 7)
  val size: Vec2i = Vec2i(101, 103)
  override def part1(input: List[Robot]): Int =  {

    val r = input.stepN(100)
    // println(r.pretty)
    r.safety
  }

  override def part2(input: List[Robot]): Int = {
    val r = input.findEasterEgg
    // println(input.stepN(r).pretty)
    r
  }

  lazy val input: String = FileIO.getInput(2024, 14).trim
}
