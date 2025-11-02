package gay.menkissing.advent
package y2020

import cats.Show

object Day03 extends Problem:
  type Input = Vector[Vector[Boolean]]
  type Output = Long

  override def parse(str: String): Vector[Vector[Boolean]] =
    str.linesIterator.map(_.map(_ == '#').toVector).toVector

  extension (self: Vector[Vector[Boolean]])
    def testSlope(slopeX: Int, slopeY: Int): Int =
      Iterator.iterate((0, 0))((x, y) =>
        ((x + slopeX) % self.head.length, y + slopeY)
      ).takeWhile((_, y) => y < self.length).count((x, y) => self(y)(x))

  override def part1(input: Vector[Vector[Boolean]]): Long =
    input.testSlope(3, 1)

  override def part2(input: Vector[Vector[Boolean]]): Long =
    List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)).foldLeft(1L):
      case (acc, (x, y)) => acc * input.testSlope(x, y)

  override lazy val input: String = FileIO.getInput(2020, 3)
