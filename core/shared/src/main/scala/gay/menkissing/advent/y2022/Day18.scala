package gay.menkissing.advent
package y2022

import gay.menkissing.common.*, Sys3D.*
import collection.mutable

object Day18 extends NewProblem[Map[Vec3i, Day18.Cube], Int]:
  def parse(input: String, default: Boolean): Map[Vec3i, Cube] = {
    input.linesIterator.map {
      case s"$x,$y,$z" => {
        val pos = Vec3i(x.toInt, y.toInt, z.toInt)
        pos -> Cube(pos, default, default, default, default, default, default)
      }
    }.toMap
  }

  def parseP1(str: String): Map[Vec3i, Cube] = parse(str, true)
  def parseP2(str: String): Map[Vec3i, Cube] = parse(str, false)
  def neighbors(pos: Vec3i): Iterable[Vec3i] = {
    Direction3D.values.toList.map(v => pos.offset(v))
  }
  case class Cube(pos: Vec3i, north: Boolean, east: Boolean, south: Boolean, west: Boolean, up: Boolean, down: Boolean) {
    def surfaceArea: Int = {
      (if (north) 1 else 0)
      + (if (east) 1 else 0)
      + (if (south) 1 else 0)
      + (if (west) 1 else 0)
      + (if (up) 1 else 0)
      + (if (down) 1 else 0)
    }
    def updated(dir: Direction3D, v: Boolean): Cube =
      dir match
        case Direction3D.North => copy(north = v)
        case Direction3D.East => copy(east = v)
        case Direction3D.South => copy(south = v)
        case Direction3D.West => copy(west = v)
        case Direction3D.Up => copy(up = v)
        case Direction3D.Down => copy(down = v)
      
    def updateFaces(map: Map[Vec3i, Cube]) = {
      val (cube, resMap) =
        Direction3D.values.foldLeft((this, map)):
          case ((curCube, map), dir) =>
            val daPos = pos.offset(dir)
            map.get(daPos) match
              case Some(daCube) =>
                (curCube.updated(dir, false), map.updated(daPos, daCube.updated(dir.reverse, false)))
              case None =>
                (curCube, map)
      resMap.updated(pos, cube)
            
    }
  }

  lazy val input = FileIO.getInput(2022, 18)






  def part1(input: Map[Vec3i, Cube]): Int =
    val goodInput = 
      input.foldLeft(input) { case (m, (_, c)) =>
        c.updateFaces(m)
      }
    goodInput.map((_, c) => c.surfaceArea).sum


  def spotIsAir(map: Map[Vec3i, Cube], v: Vec3i): Boolean =
    !map.contains(v)
  // Part 2: calculating the outer surface area
  // it will be less than part 1
  //

  def floodFill(start: Vec3i, map: Map[Vec3i, Cube], bounds: AABB3D): Set[Vec3i] = {
    assert(!map.contains(start))
    val q = mutable.Queue[Vec3i](start);
    val items = mutable.Set[Vec3i]();
    while (q.nonEmpty) {
      val n = q.removeHead();
      if (spotIsAir(map, n) && !items.contains(n)) {
        q.addAll(neighbors(n).filter(bounds.contains))
        items += n
      }
    }
    items.toSet
  }

  def part2(input: Map[Vec3i, Cube]): Int =
    // TODO: why do i need to grow this so much to get the correct answer? what is wrong with my flood fill implementation???
    // fix this later, it got ME the correct answer :troll:
    val daBounds = AABB3D.containingAll(input.keySet).grow(10)
    val freeAir = floodFill(daBounds.start, input, daBounds)

    val resMap =
      freeAir.foldLeft(input): (map, it) =>
        Direction3D.values.foldLeft(map): (m2, dir) =>
          val p = it.offset(dir)
          m2.updatedWith(p):
            case None => None
            case Some(c) => Some(c.updated(dir.reverse, true))
    // println(resMap)
    resMap.map(_._2.surfaceArea).sum
