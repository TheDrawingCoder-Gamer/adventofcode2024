package gay.menkissing.advent
package y2020

object Day05 extends Problem[List[Int], Int]:
  override def parse(str: String): List[Int] =
    str.linesIterator.map: line =>
      line.foldLeft(0):
        case (acc, c) =>
          c match
            case 'F' | 'L' => acc << 1
            case 'B' | 'R' => (acc << 1) + 1
    .toList

  override def part1(input: List[Int]): Int = input.max

  override def part2(input: List[Int]): Int =
    input.sorted.sliding(2).collectFirst:
      Function.unlift:
        case List(l, r) =>
          Option.when(r - l > 1)(r - 1)
    .get

  override lazy val input: String = FileIO.getInput(2020, 5)

