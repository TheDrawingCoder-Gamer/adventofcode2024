package gay.menkissing.advent

import gay.menkissing.advent.Problem

import scala.io.Source


object Day1 extends Problem[(List[Int], List[Int]), Int]:
  override val input = Source.fromResource("day1.txt").mkString

  override def parse(str: String): (List[Int], List[Int]) = 
    input.linesIterator.map { case s"$a   $b" => (a.toInt, b.toInt) }.toList.unzip

  override def part1(input: (List[Int], List[Int])): Int =
    val (fst, snd) = input

    val fstSorted = fst.sorted
    val sndSorted = snd.sorted
    
    fstSorted.zip(sndSorted).map((a, b) => Math.abs(a - b)).sum

  override def part2(input: (List[Int], List[Int])): Int =
    val (fst, snd) = input
    val sndCount = snd.foldLeft(Map[Int, Int]()) {
      case (map, a) =>
        map.updatedWith(a) {
          case Some(value) => Some(value + 1)
          case _ => Some(1)
        }
    }
    
    fst.map(it => sndCount.getOrElse(it, 0) * it).sum