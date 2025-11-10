package gay.menkissing.advent
package y2024

object Day03 extends Problem:
  type Input = String
  type Output = Int

  lazy val input: String = FileIO.getInput(2024, 3)

  override def parse(str: String): String = str

  def getMultMap(str: String): List[(Int, Int)] =
    val regex = raw"mul\(([0-9]{1,3}),([0-9]{1,3})\)".r

    regex.findAllMatchIn(str).map: it =>
      (it.start, it.group(1).toInt * it.group(2).toInt)
    .toList

  override def part1(input: String): Int = getMultMap(input).map(_._2).sum

  override def part2(input: String): Int =
    val multMap = getMultMap(input)

    val doRegex = raw"do\(\)".r
    val dontRegex = raw"don't\(\)".r

    val doIdxs = doRegex.findAllMatchIn(input).map(_.start)
    val dontIdxs = dontRegex.findAllMatchIn(input).map(_.start)

    val idxes =
      (doIdxs.map(it => (it, true)) ++ dontIdxs.map(it => (it, false))).toList
        .prepended((0, true)).sortBy(_._1)

    multMap.map:
      case (start, res) =>
        if idxes.findLast(_._1 < start).get._2 then res
        else 0
    .sum
