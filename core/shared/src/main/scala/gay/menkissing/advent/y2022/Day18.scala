package gay.menkissing.advent
package y2022

import gay.menkissing.common.Sys3D.*

object Day18 extends HalfDay[Map[Vec3i, Day18.Cube], Int]:
  def parse(input: String): Map[Vec3i, Cube] = {
    val data = input.linesIterator.map {
      case s"$x,$y,$z" => {
        val pos = Vec3i(x.toInt, y.toInt, z.toInt)
        pos -> Cube(pos, true, true, true, true, true, true, None)
      }
    }.toMap
    data.foldLeft(data) { case (m, (_, c)) =>
      c.updateFaces(m)
    }
  }
  def neighbors(pos: Vec3i): Iterable[Vec3i] = {
    val xPoses = for {
      x <- List(pos.x - 1, pos.x + 1)
    } yield (Vec3i(x, pos.y, pos.z))
    val yPoses = for {
      y <- List(pos.y - 1, pos.y + 1)
    } yield (Vec3i(pos.x, y, pos.z))
    val zPoses = for {
      z <- List(pos.z - 1, pos.z + 1)
    } yield (Vec3i(pos.x, pos.y, z))
    xPoses ++ yPoses ++ zPoses
  }
  enum AirType {
    case Suffocated, Free, Unknown

  }
  case class Cube(pos: Vec3i, north: Boolean, east: Boolean, south: Boolean, west: Boolean, up: Boolean, down: Boolean, airType: Option[AirType]) {
    def surfaceArea: Int = {
      (if (north) 1 else 0)
      + (if (east) 1 else 0)
      + (if (south) 1 else 0)
      + (if (west) 1 else 0)
      + (if (up) 1 else 0)
      + (if (down) 1 else 0)
    }
    def isAir = airType.isDefined
    def updateFaces(map: Map[Vec3i, Cube]) = {
      var currentCube = this
      var currentMap = map
      for {
        daPos <- neighbors(pos)
      } {
        currentMap.get(daPos).foreach { daCube =>
          if (daPos.x != pos.x) {
            val daX = pos.x - daPos.x
            daX.sign match {
              case -1 => {
                // to the west
                currentCube = currentCube.copy(west = false)
                currentMap = currentMap.updated(daPos, daCube.copy(east = false))
              }
              case 1 => {
                currentCube = currentCube.copy(east = false)
                currentMap = currentMap.updated(daPos, daCube.copy(west = false))
              }
              case _ => ()
            }
          } else if (daPos.y != pos.y) {
            val daY = pos.y - daPos.y
            daY.sign match {
              case -1 => {
                // to the down
                currentCube = currentCube.copy(down = false)
                currentMap = currentMap.updated(daPos, daCube.copy(up = false))
              }
              case 1 => {
                currentCube = currentCube.copy(up = false)
                currentMap = currentMap.updated(daPos, daCube.copy(down = false))
              }
              case _ => ()
            }
          } else {
            assert(daPos.z != pos.z)
            val daZ = pos.z - daPos.z
            daZ.sign match {
              case -1 => {
                // to the south
                currentCube = currentCube.copy(south = false)
                currentMap = currentMap.updated(daPos, daCube.copy(north = false))
              }
              case 1 => {
                currentCube = currentCube.copy(north = false)
                currentMap = currentMap.updated(daPos, daCube.copy(south = false))
              }
            }
         }
        }
      }
      currentMap.updated(pos, currentCube)
    }
  }

  lazy val input = FileIO.getInput(2022, 18)



  case class Bounds(
                   west: Int,
                   east: Int,
                   north: Int,
                   south: Int,
                   up: Int,
                   down: Int
                   ):
    def inBounds(pos: Vec3i): Boolean =
      pos.x >= west
        && pos.x <= east
        && pos.y >= down
        && pos.y <= up
        && pos.z >= south
        && pos.z <= north

  def bounds(data: Map[Vec3i, Cube]): Bounds =
    val westBound = data.minBy((p, _) => p.x)._1.x
    val eastBound = data.maxBy((p, _) => p.x)._1.x
    val northBound = data.maxBy((p, _) => p.z)._1.z
    val southBound = data.minBy((p, _) => p.z)._1.z
    val upBound = data.maxBy((p, _) => p.y)._1.y
    val downBound = data.minBy((p, _) => p.y)._1.y
    Bounds(westBound, eastBound, northBound, southBound, upBound, downBound)


  def offsetDir(x: Vec3i, y: Vec3i): Option[Direction3D] = {
    if (x.x != y.x) {
      val daX = x.x - y.x
      daX.sign match {
        case 1 => Some(Direction3D.East)
        case -1 => Some(Direction3D.West)
        case _ => None
      }
    } else if (x.y != y.y) {
      val daY = x.y - y.y
      daY.sign match {
        case 1 => Some(Direction3D.Up)
        case -1 => Some(Direction3D.Down)
        case _ => None
      }
    } else if (x.z != y.z) {
      val daZ = x.z - y.z
      daZ.sign match {
        case 1 => Some(Direction3D.North)
        case -1 => Some(Direction3D.South)
        case _ => None
      }
    } else None
  }

  def part1(input: Map[Vec3i, Cube]): Int = input.filter(!_._2.isAir).map((_, c) => c.surfaceArea).sum

  // Part 2: calculating the outer surface area
  // it will be less than part 1
  //

  def floodFill(start: Vec3i, map: Map[Vec3i, Cube], bounds: Bounds): List[Vec3i] = {
    assert(!(map.contains(start) && !map(start).isAir))
    val stack = scala.collection.mutable.Stack[Vec3i](start);
    val items = scala.collection.mutable.Set[Vec3i]();
    while (stack.nonEmpty) {
      val n = stack.pop();
      if ((!map.contains(n) || map(n).isAir) && !items.contains(n)) {
        for (neighbor <- neighbors(n).filter(bounds.inBounds)) {
          stack.push(neighbor)
          items.add(neighbor)
        }
        items.add(n)
      }
    }
    items.toList
  }

  def part2(input: Map[Vec3i, Cube]): Int =
    val daBounds = bounds(input)
    val freeAir = floodFill(Vec3i(daBounds.west, daBounds.down, daBounds.south), input, daBounds)

    println(freeAir)
    freeAir.length
