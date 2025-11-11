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

  lazy val input = FileIO.getInput(2021, 22)

  def tryAddhole(obj: AABB3D[Int], hole: AABB3D[Int]): Set[AABB3D[Int]] =
    obj intersect hole match
      case Some(realHole) => addHole(obj, realHole)
      case None           => Set(obj)

  def addHole(obj: AABB3D[Int], hole: AABB3D[Int]): Set[AABB3D[Int]] =
    var daSet = Set.empty[AABB3D[Int]]
    if obj.xs.min != hole.xs.min then
      daSet += AABB3D(obj.xs.min dimBy hole.xs.min - 1, obj.ys, obj.zs)
    if obj.xs.max != hole.xs.max then
      daSet += AABB3D(hole.xs.max + 1 dimBy obj.xs.max, obj.ys, obj.zs)
    if obj.ys.min != hole.ys.min then
      daSet += AABB3D(hole.xs, obj.ys.min dimBy hole.ys.min - 1, obj.zs)
    if obj.ys.max != hole.ys.max then
      daSet += AABB3D(hole.xs, hole.ys.max + 1 dimBy obj.ys.max, obj.zs)
    if obj.zs.min != hole.zs.min then
      daSet += AABB3D(hole.xs, hole.ys, obj.zs.min dimBy hole.zs.min - 1)
    if obj.zs.max != hole.zs.max then
      daSet += AABB3D(hole.xs, hole.ys, hole.zs.max + 1 dimBy obj.zs.max)

    daSet
  def parse(str: String): List[Step] =
    str.linesIterator.map:
      case s"$cmd x=$lx..$rx,y=$ly..$ry,z=$lz..$rz" =>
        val command = Command.parse(cmd)
        Step(
          command,
          AABB3D(
            lx.toInt dimBy rx.toInt,
            ly.toInt dimBy ry.toInt,
            lz.toInt dimBy rz.toInt
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
    val rangeBound = AABB3D(-50 dimBy 50, -50 dimBy 50, -50 dimBy 50)
    score(run(input.takeWhile(_.cuboid.fitsIn(rangeBound))))

  def part2(input: List[Step]): BigInt = score(run(input))
