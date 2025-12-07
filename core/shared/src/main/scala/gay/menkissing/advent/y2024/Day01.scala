package gay.menkissing.advent
package y2024

object Day01 extends Problem:
  type Input = (List[Int], List[Int])
  type Output = Int

  override def input = FileIO.getInput(2024, 1)

  override def parse(str: String): (List[Int], List[Int]) =
    str.linesIterator.map:
      case s"$a   $b" => (a.toInt, b.toInt)
    .toList.unzip

  override def part1(input: (List[Int], List[Int])): Int =
    val (fst, snd) = input

    val fstSorted = fst.sorted
    val sndSorted = snd.sorted

    fstSorted.zip(sndSorted).map((a, b) => Math.abs(a - b)).sum

  override def part2(input: (List[Int], List[Int])): Int =
    val (fst, snd) = input
    val sndCount =
      snd.foldLeft(Map.empty[Int, Int].withDefaultValue(0)):
        case (map, a) => map.updated(a, map(a) + 1)

    fst.map(it => sndCount.getOrElse(it, 0) * it).sum
