package gay.menkissing.advent
package y2021

import gay.menkissing.common.*

object Day6y2021 extends Problem[Vector[Int], Long]:
  type FishMap = Map[Int, Long]

  def naiveAdvance(fish: Vector[Int]): Vector[Int] =
    fish.map {
      case 0 => 6
      case 1 => 0
      case 2 => 1
      case 3 => 2
      case 4 => 3
      case 5 => 4
      case 6 => 5
      case 7 => 6
      case 8 => 7
      case _ => assert(false)
    }.prependedAll(List.fill(fish.count(_ == 0))(8))

  override def parse(str: String): Vector[Int] =
    str.split(",").map(_.toInt).toVector

  def advance(map: FishMap): FishMap =
    Map(
      0 -> map.getOrElse(1, 0L),
      1 -> map.getOrElse(2, 0L),
      2 -> map.getOrElse(3, 0L),
      3 -> map.getOrElse(4, 0L),
      4 -> map.getOrElse(5, 0L),
      5 -> map.getOrElse(6, 0L),
      6 -> (map.getOrElse(0, 0L) + map.getOrElse(7, 0L)),
      7 -> map.getOrElse(8, 0L),
      8 -> map.getOrElse(0, 0L)
    )

  override def part1(input: Vector[Int]): Long =
    naiveAdvance.repeated(80)(input).size


  override def part2(input: Vector[Int]): Long =
    val fishMap = input.groupBy(identity).map(it => it._1 -> it._2.length.toLong)
    advance.repeated(256)(fishMap).foldLeft(0L):
      case (acc, (_, v)) => acc + v

  override val input: String = FileIO.getContentsOf("y2021/day6.txt")





