import scala.io.Source

val data = Source.fromResource("day3.txt").mkString

val regex = raw"mul\(([0-9]{1,3}),([0-9]{1,3})\)".r
val doRegex = raw"do\(\)".r
val dontRegex = raw"don't\(\)".r


val multMap = regex.findAllMatchIn(data).map { it =>
  (it.start, it.group(1).toInt * it.group(2).toInt)
}

val doIdxs = doRegex.findAllMatchIn(data).map(_.start)
val dontIdxs = dontRegex.findAllMatchIn(data).map(_.start)

val idxes = (doIdxs.map(it => (it, true)) ++ dontIdxs.map(it => (it, false))).toList.prepended((0, true)).sortBy(_._1)

multMap.map { case (start, res) =>
  if (idxes.findLast(_._1 < start).get._2) {
    res
  } else 0
}.sum
// egg


