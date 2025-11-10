package gay.menkissing.advent
package y2022

import gay.menkissing.common.*, Sys3D.*
import collection.mutable
import algebra.instances.all.*

object Day18 extends NewProblem:
  type Input = Map[Vec3[Int], Cube]
  type Output = Int

  def parse(input: String, default: Boolean): Map[Vec3[Int], Cube] =
    input.linesIterator.map:
      case s"$x,$y,$z" =>
        val pos = Vec3(x.toInt, y.toInt, z.toInt)
        pos -> Cube(pos, default, default, default, default, default, default)
    .toMap

  def parseP1(str: String): Map[Vec3[Int], Cube] = parse(str, true)
  def parseP2(str: String): Map[Vec3[Int], Cube] = parse(str, false)
  def neighbors(pos: Vec3[Int]): Iterable[Vec3[Int]] =
    Direction3D.values.toList.map(v => pos.offset(v))
  case class Cube
    (
      pos: Vec3[Int],
      north: Boolean,
      east: Boolean,
      south: Boolean,
      west: Boolean,
      up: Boolean,
      down: Boolean
    ):
    def surfaceArea: Int =
      (if north then 1 else 0) + (if east then 1 else 0) +
        (if south then 1 else 0) + (if west then 1 else 0) +
        (if up then 1 else 0) + (if down then 1 else 0)
    def updated(dir: Direction3D, v: Boolean): Cube =
      dir match
        case Direction3D.North => copy(north = v)
        case Direction3D.East  => copy(east = v)
        case Direction3D.South => copy(south = v)
        case Direction3D.West  => copy(west = v)
        case Direction3D.Up    => copy(up = v)
        case Direction3D.Down  => copy(down = v)

    def updateFaces(map: Map[Vec3[Int], Cube]) =
      val (cube, resMap) =
        Direction3D.values.foldLeft((this, map)):
          case ((curCube, map), dir) =>
            val daPos = pos.offset(dir, 1)
            map.get(daPos) match
              case Some(daCube) =>
                (
                  curCube.updated(dir, false),
                  map.updated(daPos, daCube.updated(dir.reverse, false))
                )
              case None => (curCube, map)
      resMap.updated(pos, cube)

  lazy val input = FileIO.getInput(2022, 18)

  def part1(input: Map[Vec3[Int], Cube]): Int =
    val goodInput =
      input.foldLeft(input):
        case (m, (_, c)) => c.updateFaces(m)
    goodInput.map((_, c) => c.surfaceArea).sum

  def spotIsAir(map: Map[Vec3[Int], Cube], v: Vec3[Int]): Boolean =
    !map.contains(v)
  // Part 2: calculating the outer surface area
  // it will be less than part 1
  //

  def floodFill
    (
      start: Vec3[Int],
      map: Map[Vec3[Int], Cube],
      bounds: AABB3D[Int]
    ): Set[Vec3[Int]] =
    assert(!map.contains(start))
    val q = mutable.Queue[Vec3[Int]](start)
    val items = mutable.Set[Vec3[Int]]()
    while q.nonEmpty do
      val n = q.removeHead()
      if spotIsAir(map, n) && !items.contains(n) then
        q.addAll(neighbors(n).filter(bounds.contains))
        items += n

    items.toSet

  def part2(input: Map[Vec3[Int], Cube]): Int =
    // fixed, was some shenanagins in AABB3D.grow
    val daBounds = AABB3D.containingAll(input.keySet).grow(1)
    val freeAir = floodFill(daBounds.start, input, daBounds)

    val resMap =
      freeAir.foldLeft(input): (map, it) =>
        Direction3D.values.foldLeft(map): (m2, dir) =>
          val p = it.offset(dir)
          m2.updatedWith(p):
            case None    => None
            case Some(c) => Some(c.updated(dir.reverse, true))
    // println(resMap)
    resMap.map(_._2.surfaceArea).sum
