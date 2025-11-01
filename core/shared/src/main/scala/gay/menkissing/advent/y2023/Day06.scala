package gay.menkissing.advent
package y2023

object Day06
    extends ProblemUniqueInputs[List[(Long, Long)], (Long, Long), Long]:

  override lazy val input: String = FileIO.getInput(2023, 6)

  override def parseP1(str: String): List[(Long, Long)] =
    str.linesIterator.toList match
      case s"Time: $times" :: s"Distance: $ds" :: _ =>
        val regex = raw"\s+".r
        regex.split(times.trim).map(_.toLong).toList
          .zip(regex.split(ds.trim).map(_.toLong))
      case _ => ???

  override def parseP2(str: String): (Long, Long) =
    str.linesIterator.toList match
      case s"Time: $times" :: s"Distance: $ds" :: _ =>
        val regex = raw"\s+".r
        (
          regex.split(times.trim).mkString("").toLong,
          regex.split(ds.trim).mkString("").toLong
        )
      case _ => ???

  // distance = (maxTime - timeHeld) * timeHeld
  def calc(time: Long, max: Long): Long = (max - time) * time

  def naiveFindBounds(time: Long, distance: Long): Long =
    val minBound = (1L until time).find(i => calc(i, time) > distance)
    val maxBound = (1L until time).findLast(i => calc(i, time) > distance)
    maxBound.get - minBound.get + 1

  override def part1(input: List[(Long, Long)]): Long =
    // Lets be naive!!!!
    val bounds = input.map((t, d) => naiveFindBounds(t, d))
    bounds.product

  override def part2(input: (Long, Long)): Long =
    val (time, distance) = input
    // IT JUST WORKS:tm:
    naiveFindBounds(time, distance)
