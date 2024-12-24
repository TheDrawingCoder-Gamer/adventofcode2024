package gay.menkissing.advent

import gay.menkissing.advent.Problem

import scala.io.Source

object Day3 extends Problem[String, Int]:
  
  val input: String = Source.fromResource("day3.txt").mkString

  override def parse(str: String): String = str
  
  def getMultMap(str: String): List[(Int, Int)] =
    val regex = raw"mul\(([0-9]{1,3}),([0-9]{1,3})\)".r

    
    regex.findAllMatchIn(str).map { it =>
      (it.start, it.group(1).toInt * it.group(2).toInt)
    }.toList

  override def part1(input: String): Int = getMultMap(input).map(_._2).sum

  override def part2(input: String): Int =
    val multMap = getMultMap(input)

    val doRegex = raw"do\(\)".r
    val dontRegex = raw"don't\(\)".r

    val doIdxs = doRegex.findAllMatchIn(input).map(_.start)
    val dontIdxs = dontRegex.findAllMatchIn(input).map(_.start)

    val idxes = (doIdxs.map(it => (it, true)) ++ dontIdxs.map(it => (it, false))).toList.prepended((0, true)).sortBy(_._1)

    multMap.map { case (start, res) =>
      if (idxes.findLast(_._1 < start).get._2) {
        res
      } else 0
    }.sum

