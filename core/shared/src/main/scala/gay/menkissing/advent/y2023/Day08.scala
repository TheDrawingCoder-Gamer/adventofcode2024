package gay.menkissing.advent
package y2023

import gay.menkissing.common.*, algebras.given
import algebra.ring.EuclideanRing

object Day08 extends Problem:
  type Input = (String, Map[String, (String, String)])
  type Output = Long
  def input: String = FileIO.getInput(2023, 8)

  def parse(str: String): (String, Map[String, (String, String)]) =
    val List(path, conns) = str.split("\n\n").toList: @unchecked
    val connMap =
      conns.linesIterator.map:
        case s"$key = ($l, $r)" => (key, (l, r))
      .toMap
    (path, connMap)

  def calcTuahThatThang
    (
      path: String,
      connMap: Map[String, (String, String)],
      start: String,
      pred: String => Boolean
    ): Long =
    var cur = start
    1L + ForeverIterator(path.iterator).indexWhere: c =>
      val left = c == 'L'
      cur = if left then connMap(cur)._1 else connMap(cur)._2
      pred(cur)
    .toLong

  def part1(input: (String, Map[String, (String, String)])): Long =
    val (path, connMap) = input
    calcTuahThatThang(path, connMap, "AAA", _ == "ZZZ")

  // yes WE all came up with using LCM on our own
  // even though WE thought that because the cycle didnt encompass the entire thing, it didnt matter
  def part2(input: (String, Map[String, (String, String)])): Long =
    val (path, connMap) = input

    connMap.keys.filter(_.endsWith("A")).map: key =>
      calcTuahThatThang(path, connMap, key, _.endsWith("Z"))
    .reduce(EuclideanRing[Long].lcm).toLong
