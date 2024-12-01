import scala.io.Source

val input = Source.fromResource("day1.txt").mkString

val sides = input.linesIterator.map { case s"$a   $b" => (a.toInt, b.toInt) }

val (fst, snd) = sides.toList.unzip

val fstSorted = fst.sorted
val sndSorted = snd.sorted

val part1 = fstSorted.zip(sndSorted).map((a, b) => Math.abs(a - b)).sum

val sndCount = snd.foldLeft(Map[Int, Int]()) {
  case (map, a) =>
    map.updatedWith(a) {
      case Some(value) => Some(value + 1)
      case _ => Some(1)
    }
}

val part2 = fst.map(it => sndCount.getOrElse(it, 0) * it).sum