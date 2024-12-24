package gay.menkissing.advent

import gay.menkissing.advent.Problem
import gay.menkissing.common.*

import scala.collection.parallel.CollectionConverters.*
import scala.io.Source

object Day22 extends Problem[List[Int], Long]:
  override def parse(str: String): List[Int] =
    str.linesIterator.map(_.toInt).toList

  // 16777216 == 2 ^ 24
  val pow2to24minus1 = 16777216 - 1
  def advance(i: Int): Int =
    var x = i
    x ^= (x << 6)
    x &= pow2to24minus1

    x ^= (x >> 5)
    x &= pow2to24minus1

    x ^= (x << 11)
    x &= pow2to24minus1

    x

  override def part1(input: List[Int]): Long =
    input.map: l =>
      advance.repeated(2000)(l).toLong
    .sum

  override def part2(input: List[Int]): Long =
    val sequences = input.map: l =>
      Iterator.iterate(l)(advance).take(2000).map(_ % 10).toList
    val validDiffSequences =
      for {
        x <- -9 to 9
        y <- -9 to 9
        z <- -9 to 9
        w <- -9 to 9
      } yield Vector(x, y, z, w)
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



    validDiffSequences.flatMap: i =>
      diffMap.get(i).map(v => (i, v))
    .maxBy(_._2)._2






  override val input: String = Source.fromResource("day22.txt").mkString
