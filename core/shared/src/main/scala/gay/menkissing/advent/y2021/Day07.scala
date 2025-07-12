package gay.menkissing.advent
package y2021

object Day07 extends Problem[List[Int], Int]:
  lazy val input = FileIO.getInput(2021, 7)

  def parse(input: String): List[Int] =
    input.trim.split(",").map(_.toInt).toList

  def median(input: List[Int]): Int =
    val len = input.length
    val l = len.toDouble / 2.0
    val ll = l.floor.toInt
    val lu = l.ceil.toInt
    val sorted = input.sorted
    if ll == lu then
      sorted(ll)
    else
      math.max(sorted(ll), sorted(lu))

  def average(ls: List[Int]): Double =
    ls.sum.toDouble / ls.length.toDouble

  def sumDistance(crabs: List[Int], pt: Int): Int =
    crabs.map(it => math.abs(it - pt)).sum

  def part1(input: List[Int]): Int =
    sumDistance(input, median(input))

  final def sumtorial(p: Int): Int =
    if p < 0 then
      0
    else
      (1 to p).sum

  def distanceP2(crabs: List[Int], pt: Int): Int =
    crabs.map(it => sumtorial(math.abs(it - pt))).sum

  def part2(input: List[Int]): Int =
    val pt = average(input)
    // We don't know what the best out of the two is so lets just test both
    math.min(distanceP2(input, pt.floor.toInt), distanceP2(input, pt.ceil.toInt))