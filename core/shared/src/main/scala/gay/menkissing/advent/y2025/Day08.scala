package gay.menkissing.advent
package y2025

import gay.menkissing.common.*
import Sys3D.*
import collection.mutable
object Day08 extends Problem:
  type Input = Set[Vec3[Int]]
  type Output = Long

  def input: String = FileIO.getInput(2025, 8)

  def parse(str: String): Set[Vec3[Int]] =
    str.linesIterator.map:
      case s"$x,$y,$z" => Vec3(x.toInt, y.toInt, z.toInt)
    .toSet

  // Need to do it here because
  // I depend on Double's properties and I cant make it generic
  def straightLineDistance(x: Vec3[Int], y: Vec3[Int]): Double =
    val xDist = math.pow((x.x - y.x).toDouble, 2.0)
    val yDist = math.pow((x.y - y.y).toDouble, 2.0)
    val zDist = math.pow((x.z - y.z).toDouble, 2.0)
    math.sqrt(xDist + yDist + zDist)

  val takeAmount: Int = 1000

  def distanceList
    (input: Set[Vec3[Int]]): List[((Vec3[Int], Vec3[Int]), Double)] =
    input.subsets(2).map: set =>
      val x :: y :: Nil = set.toList.runtimeChecked
      val distance = straightLineDistance(x, y)
      ((x, y), distance)
    .toList.sortBy(_._2)

  def part1(input: Set[Vec3[Int]]): Long =
    val distList = distanceList(input)

    val connections =
      distList.take(takeAmount).flatMap:
        case ((v1, v2), _) => List(v1 -> v2, v2 -> v1)
      .groupMapReduce(_._1)(it => Set(it._2))(_ ++ _)
    def dfs(cur: Vec3[Int], visited: mutable.Set[Vec3[Int]]): Int =
      visited += cur

      1 + connections.getOrElse(cur, Set.empty).iterator.map: x =>
        if !visited(x) then dfs(x, visited)
        else 0
      .sum

    val visited = mutable.Set.empty[Vec3[Int]]
    val map = mutable.ListBuffer.empty[Int]
    input.foreach: p =>
      if !visited(p) then map += dfs(p, visited)
    assert(visited.size == input.size)
    assert(map.sum == input.size)
    map.sorted(using Ordering[Int].reverse).take(3).product

  def part2(input: Set[Vec3[Int]]): Long =
    val distList = distanceList(input).map(_._1)
    def fullyLinked(conns: List[(Vec3[Int], Vec3[Int])]): Boolean =
      val v = input.head
      val map =
        conns.flatMap((x, y) => List(x -> y, y -> x))
          .groupMapReduce(_._1)(it => Set(it._2))(_ ++ _)
      val visited = mutable.Set.empty[Vec3[Int]]
      def dfs(cur: Vec3[Int]): Unit =
        visited += cur
        map.getOrElse(cur, Set.empty).foreach: x =>
          if !visited(x) then dfs(x)
      dfs(v)
      visited.size == input.size

    val idx = (1 until distList.size).find: p =>
      val z = distList.take(p)
      fullyLinked(z)
    .get
    val p = distList(idx - 1)
    p._1.x * p._2.x
