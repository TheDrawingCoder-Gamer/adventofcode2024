package gay.menkissing.advent
package y2021

import gay.menkissing.common.*
import Sys3D.*
import algebra.instances.all.*

object Day22 extends Problem:
  type Input = List[Step]
  type Output = BigInt

  final case class Step(command: Command, cuboid: AABB3D[Int])

  enum Command:
    case On, Off

  object Command:
    def parse(v: String): Command =
      v match
        case "on"  => On
        case "off" => Off

  def input = FileIO.getInput(2021, 22)

  def tryAddhole(obj: AABB3D[Int], hole: AABB3D[Int]): Set[AABB3D[Int]] =
    obj intersect hole match
      case Some(realHole) => addHole(obj, realHole)
      case None           => Set(obj)

  def addHole(obj: AABB3D[Int], hole: AABB3D[Int]): Set[AABB3D[Int]] =
    var daSet = Set.empty[AABB3D[Int]]
    if obj.xs.start != hole.xs.start then
      daSet += AABB3D(obj.xs.start safeToIncl hole.xs.start - 1, obj.ys, obj.zs)
    if obj.xs.end != hole.xs.end then
      daSet += AABB3D(hole.xs.end + 1 safeToIncl obj.xs.end, obj.ys, obj.zs)
    if obj.ys.start != hole.ys.start then
      daSet +=
        AABB3D(hole.xs, obj.ys.start safeToIncl hole.ys.start - 1, obj.zs)
    if obj.ys.end != hole.ys.end then
      daSet += AABB3D(hole.xs, hole.ys.end + 1 safeToIncl obj.ys.end, obj.zs)
    if obj.zs.start != hole.zs.start then
      daSet +=
        AABB3D(hole.xs, hole.ys, obj.zs.start safeToIncl hole.zs.start - 1)
    if obj.zs.end != hole.zs.end then
      daSet += AABB3D(hole.xs, hole.ys, hole.zs.end + 1 safeToIncl obj.zs.end)

    daSet
  def parse(str: String): List[Step] =
    str.linesIterator.map:
      case s"$cmd x=$lx..$rx,y=$ly..$ry,z=$lz..$rz" =>
        val command = Command.parse(cmd)
        Step(
          command,
          AABB3D(
            lx.toInt safeToIncl rx.toInt,
            ly.toInt safeToIncl ry.toInt,
            lz.toInt safeToIncl rz.toInt
          )
        )
    .toList

  def fitsBound(min: Int, max: Int)(v: Int): Boolean = v >= min && v <= max

  def run(steps: List[Step]): Set[AABB3D[Int]] =
    steps.foldLeft(Set.empty[AABB3D[Int]]): (daSet, step) =>
      val toAdd =
        if step.command == Command.On then Set(step.cuboid) else Set.empty
      val r = daSet.flatMap(it => tryAddhole(it, step.cuboid)) ++ toAdd
      // println(r.map(_.show))
      // println(score(r))
      r

  def score(s: Set[AABB3D[Int]]): BigInt = s.toList.map(_.volume).sum
  def part1(input: List[Step]): BigInt =
    val rangeBound =
      AABB3D(-50 safeToIncl 50, -50 safeToIncl 50, -50 safeToIncl 50)
    score(run(input.takeWhile(_.cuboid.fitsIn(rangeBound))))

  def part2(input: List[Step]): BigInt = score(run(input))
