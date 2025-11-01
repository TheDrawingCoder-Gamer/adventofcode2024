package gay.menkissing.advent
package y2022

import gay.menkissing.common.*

object Day20 extends Problem[Vector[Long], Long]:
  def mix(pos: Int, n: Long, dataSize: Int): Int =
    val r =
      if n == 0 then pos
      else
        val rawTo = pos + n
        val to = rawTo.rem(dataSize - 1).toInt
        if to == 0 then dataSize - 1 else to

    if dataSize < 20 then println(s"$n moved from $pos to $r")

    r

  // Using this reduces performance LOL
  // when the programming is functional....
  extension [A](vec: Vector[A])
    def move(from: Int, to: Int): Vector[A] =
      if from == to then vec
      else if from < to then
        val (before, at) = vec.splitAt(from)
        val after = at.drop(1)
        val (beforeMove, afterMove) = after.splitAt(to - from)
        before ++ beforeMove ++ Vector(vec(from)) ++ afterMove
      else
        val (before, at) = vec.splitAt(from)
        val after = at.drop(1)
        val (beforeMove, afterMove) = before.splitAt(to)
        beforeMove ++ Vector(vec(from)) ++ afterMove ++ after

  extension (vec: Vector[(Long, Int)])
    def mixNumbersReal: Vector[(Long, Int)] =
      val dataSize = vec.size
      vec.sortBy(_._2).foldLeft(vec):
        case (result, (item, idx)) =>
          val pos = result.indexWhere(_._2 == idx)
          val newPos = mix(pos, item, dataSize)
          val r = result.move(pos, newPos)
          if dataSize < 20 then println(r.map(_._1))
          r

  extension (vec: Vector[Long])
    def mixNumbersImm: Vector[Long] = vec.zipWithIndex.mixNumbersReal.map(_._1)

  lazy val input = FileIO.getInput(2022, 20)
  def parse(input: String): Vector[Long] =
    input.linesIterator.map(_.toLong).toVector
  def part1(input: Vector[Long]): Long =
    val dataSize = input.size
    val goodData = input.mixNumbersImm
    val zeroIndex = goodData.indexOf(0L)
    if dataSize < 20 then println(goodData)
    val xIdx = (zeroIndex + 1000) % dataSize
    val yIdx = (zeroIndex + 2000) % dataSize
    val zIdx = (zeroIndex + 3000) % dataSize

    goodData(xIdx) + goodData(yIdx) + goodData(zIdx)
  def part2(input: Vector[Long]): Long =
    val data = input.map(_ * 811589153L)
    val dataSize = data.size
    val goodData = mixNumbersReal.repeated(10)(data.zipWithIndex).map(_._1)

    val zeroIndex = goodData.indexOf(0L)
    if dataSize < 20 then println(goodData)
    val xIdx = (zeroIndex + 1000) % dataSize
    val yIdx = (zeroIndex + 2000) % dataSize
    val zIdx = (zeroIndex + 3000) % dataSize

    goodData(xIdx) + goodData(yIdx) + goodData(zIdx)
