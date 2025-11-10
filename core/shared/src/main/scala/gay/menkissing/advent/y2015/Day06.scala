package gay.menkissing.advent
package y2015

import gay.menkissing.common.Vec2
object Day06 extends Problem:
  type Input = List[Instruction]
  type Output = Int

  enum Op:
    case On, Toggle, Off

    def apply(b: Boolean): Boolean =
      this match
        case On     => true
        case Off    => false
        case Toggle => !b

    def apply(i: Int): Int =
      this match
        case On     => i + 1
        case Off    => math.max(0, i - 1)
        case Toggle => i + 2

  case class Instruction(op: Op, start: Vec2[Int], stop: Vec2[Int]):
    def advance(arr: Array[Boolean]): Unit =
      val minX = start.x `min` stop.x
      val maxX = start.x `max` stop.x
      val minY = start.y `min` stop.y
      val maxY = start.y `max` stop.y
      for
        x <- minX to maxX
        y <- minY to maxY
      do arr(y * 1000 + x) = op(arr(y * 1000 + x))
    def advance(arr: Array[Int]): Unit =
      val minX = start.x `min` stop.x
      val maxX = start.x `max` stop.x
      val minY = start.y `min` stop.y
      val maxY = start.y `max` stop.y
      for
        x <- minX to maxX
        y <- minY to maxY
      do arr(y * 1000 + x) = op(arr(y * 1000 + x))

  def parse(str: String): List[Instruction] =
    str.linesIterator.map:
      // matching kind of sucks, sort of a hack
      case s"toggle $x1,$y1 through $x2,$y2" =>
        Instruction(
          Op.Toggle,
          Vec2(x1.toInt, y1.toInt),
          Vec2(x2.toInt, y2.toInt)
        )
      case s"turn $o $x1,$y1 through $x2,$y2" =>
        Instruction(
          if o == "off" then Op.Off else Op.On,
          Vec2(x1.toInt, y1.toInt),
          Vec2(x2.toInt, y2.toInt)
        )
    .toList

  def part1(input: List[Instruction]): Int =
    // as learned with day 15 2020, it's perfectly acceptable to have a primitive array of length 1,000,000
    val arr = Array.fill(1000 * 1000)(false)
    input.foreach(_.advance(arr))
    arr.count(identity)

  def part2(input: List[Instruction]): Int =
    val arr = Array.fill(1000 * 1000)(0)
    input.foreach(_.advance(arr))
    arr.sum

  lazy val input: String = FileIO.getInput(2015, 6)
