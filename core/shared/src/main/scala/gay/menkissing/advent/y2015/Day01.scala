package gay.menkissing.advent
package y2015


object Day01 extends Problem:
  type Input = String

  type Output = Int

  def parse(str: String): String = str

  def part1(input: String): Int = input.count(_ == '(') - input.count(_ == ')')

  def part2(input: String): Int =
    input.zipWithIndex.foldLeft((0, -1)):
      case ((acc, -1), ('(', _)) => (acc + 1, -1)
      case ((0, -1), (')', i))   => (-1, i)
      case ((acc, -1), (')', _)) => (acc - 1, -1)
      case ((a, b), (_, _))      => (-1, b)
    ._2 + 1

  lazy val input: String = FileIO.getInput(2015, 1)
