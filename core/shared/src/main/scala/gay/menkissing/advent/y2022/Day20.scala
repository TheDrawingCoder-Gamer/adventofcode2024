package gay.menkissing.advent
package y2022

import gay.menkissing.common.*
import scala.reflect.ClassTag

object Day20 extends Problem:
  type Input = Vector[Long]
  type Output = Long

  def mix(pos: Int, n: Long, dataSize: Int): Int =
    val r =
      if n == 0 then pos
      else
        val rawTo = pos + n
        val to = rawTo.rem(dataSize - 1).toInt
        if to == 0 then dataSize - 1 else to

    if dataSize < 20 then println(s"$n moved from $pos to $r")

    r

  extension [A: ClassTag](vec: IArray[A])
    def move(from: Int, to: Int): IArray[A] =
      if from == to then vec
      else if from < to then
        val arr = vec.toArray
        val arr2 = Array.ofDim[A](arr.length)
        Array.copy(arr, 0, arr2, 0, from)
        Array.copy(arr, from + 1, arr2, from, to - from)
        arr2(to) = arr(from)
        Array.copy(arr, to + 1, arr2, to + 1, arr2.length - (to + 1))
        IArray.unsafeFromArray(arr2)
      else
        val arr = vec.toArray
        val arr2 = Array.ofDim[A](arr.length)
        Array.copy(arr, 0, arr2, 0, to)
        arr2(to) = arr(from)
        Array.copy(arr, to, arr2, to + 1, from - to)
        Array.copy(arr, from + 1, arr2, from + 1, arr2.length - (from + 1))
        IArray.unsafeFromArray(arr2)
  extension (vec: IArray[(Long, Int)])
    def mixNumbersReal: IArray[(Long, Int)] =
      val dataSize = vec.size
      vec.sortBy(_._2).foldLeft(vec):
        case (result, (item, idx)) =>
          val pos = result.indexWhere(_._2 == idx)
          val newPos = mix(pos, item, dataSize)
          val r = result.move(pos, newPos)
          if dataSize < 20 then println(r.map(_._1))
          r

  extension (vec: IArray[Long])
    def mixNumbersImm: IArray[Long] = vec.zipWithIndex.mixNumbersReal.map(_._1)

  def input = FileIO.getInput(2022, 20)
  def parse(input: String): Vector[Long] =
    input.linesIterator.map(_.toLong).toVector
  def part1(input: Vector[Long]): Long =
    val dataSize = input.size
    val goodData = IArray.from(input).mixNumbersImm
    val zeroIndex = goodData.indexOf(0L)
    if dataSize < 20 then println(goodData)
    val xIdx = (zeroIndex + 1000) % dataSize
    val yIdx = (zeroIndex + 2000) % dataSize
    val zIdx = (zeroIndex + 3000) % dataSize

    goodData(xIdx) + goodData(yIdx) + goodData(zIdx)

  // swapping vector for iarray - ~2500ms to ~350ms!!!
  // this is so awesome sauce
  def part2(input: Vector[Long]): Long =
    val data = input.map(_ * 811589153L)
    val dataSize = data.size
    val goodData =
      mixNumbersReal.repeated(10)(IArray.from(data.zipWithIndex)).map(_._1)

    val zeroIndex = goodData.indexOf(0L)
    if dataSize < 20 then println(goodData)
    val xIdx = (zeroIndex + 1000) % dataSize
    val yIdx = (zeroIndex + 2000) % dataSize
    val zIdx = (zeroIndex + 3000) % dataSize

    goodData(xIdx) + goodData(yIdx) + goodData(zIdx)
