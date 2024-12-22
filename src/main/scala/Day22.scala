import gay.menkissing.advent.Problem
import gay.menkissing.common.*

import scala.io.Source
import scala.collection.parallel.CollectionConverters.*

object Day22 extends Problem[List[Int], Long]:
  override def parse(str: String): List[Int] =
    str.linesIterator.map(_.toInt).toList

  // 16777216 == 2 ^ 24
  val pow2to24minus1 = 16777216 - 1
  def stepRand(i: Int): Int =
    var x = i
    // starting with 0b1111111111111111
    // 64 is 6 zero bits
    x ^= (x << 6)
    // upp
    // upper bits 1, 0b0000000000111111
    x &= pow2to24minus1
    // 32 is 5 zero bits
    x ^= (x >> 5)
    x &= pow2to24minus1
    // 2048 is 11 zero bits
    x ^= (x << 11)
    x &= pow2to24minus1

    x

  override def part1(input: List[Int]): Long =
    input.map: l =>
      stepRand.repeated(2000)(l).toLong
    .sum

  override def part2(input: List[Int]): Long =
    val sequences = input.map: l =>
      Iterator.iterate(l)(stepRand).take(2000).map(_ % 10).toList
    val validDiffSequences =
      (-9 to 9).flatMap: x =>
        (-9 to 9).flatMap: y =>
          (-9 to 9).flatMap: z =>
            (-9 to 9).map: w =>
              Vector(x, y, z, w)
      .toVector
    val diffMap =
      sequences.map: seq =>
        seq.sliding(5).foldLeft(Map[Vector[Int], Int]()):
          case (acc, Seq(x, y, z, w, v)) =>
            acc.updatedWith(Vector(y - x, z - y, w - z, v - w)):
              case Some(value) => Some(value)
              case None => Some(v)
      .foldLeft(Map[Vector[Int], Long]()): (acc, oldMap) =>
        oldMap.foldLeft(acc):
          case (c, (k, v)) =>
            c.updatedWith(k):
              case Some(value) => Some(value + v.toLong)
              case _ => Some(v.toLong)



    validDiffSequences.par.flatMap: i =>
      diffMap.get(i).map(v => (i, v))
    .maxBy(_._2)._2






  override def input: String = Source.fromResource("day22.txt").mkString

@main def main(): Unit =
  Day22.debugAndTimeP1()
  Day22.debugAndTimeP2()