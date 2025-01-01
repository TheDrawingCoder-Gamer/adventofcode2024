import cats.*
import cats.implicits.*
import cats.data.*
import scala.io.Source
import gay.menkissing.common.Sys3D.*

def parse(input: String) = {
  input.linesIterator.map {
    case s"$x,$y,$z" => {
      val pos = Vec3i(x.toInt, y.toInt, z.toInt)
      pos -> Cube(pos, true, true, true, true, true, true, None)
    }
  }.toMap
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
      currentMap.get(daPos).map { daCube => 
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

val input = Source.fromResource("day18tst.txt").mkString 

val data = parse(input)

val goodData = data.foldLeft(data) { case (m, (_, c)) => 
  c.updateFaces(m)
}

val westBound = goodData.minBy((p, _) => p.x)._1.x
val eastBound = goodData.maxBy((p, _) => p.x)._1.x 
val northBound = goodData.maxBy((p, _) => p.z)._1.z
val southBound = goodData.minBy((p, _) => p.z)._1.z
val upBound = goodData.maxBy((p, _) => p.y)._1.y 
val downBound = goodData.minBy((p, _) => p.y)._1.y

var daData = goodData

daData.filter(!_._2.isAir).map((_, c) => c.surfaceArea).sum
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
/* 
for {
  x <- westBound to eastBound 
  y <- downBound to upBound 
  z <- southBound to northBound 
} {
  val daPos = Vec3i(x, y, z)
  if (!daData.contains(daPos) || daData(daPos).isAir) {
    if (neighbors(daPos).forall(daData.contains)) {
      for {
        neighbor <- neighbors(daPos) 
      } {
        daData.get(neighbor).filter(!_.isAir).map { daCube =>
          offsetDir(daPos, neighbor).get match {
            case Direction.Up => daData = daData.updated(neighbor, daCube.copy(down = false))
            case Direction.Down => daData = daData.updated(neighbor, daCube.copy(up = false))
            case Direction.East => daData = daData.updated(neighbor, daCube.copy(west = false))
            case Direction.West => daData = daData.updated(neighbor, daCube.copy(east = false))
            case Direction.North => daData = daData.updated(neighbor, daCube.copy(south = false))
            case Direction.South => daData = daData.updated(neighbor, daCube.copy(north = false))
          } 

        }
      }

    }
  }
}
*/
// Part 2: calculating the outer surface area
// it will be less than part 1
//

daData.filter(!_._2.isAir).map((_, c) => c.surfaceArea).sum

def inBounds(pos: Vec3i): Boolean = {
  pos.x >= westBound 
  && pos.x <= eastBound 
  && pos.y >= downBound
  && pos.y <= upBound 
  && pos.z >= southBound
  && pos.z <= northBound
}
def floodFill(start: Vec3i, map: Map[Vec3i, Cube]): List[Vec3i] = {
  if (map.contains(start) && !map(start).isAir) 
    ???
  val stack = scala.collection.mutable.Stack[Vec3i](start);
  val items = scala.collection.mutable.Set[Vec3i]();
  while (!stack.isEmpty) {
    val n = stack.pop();
    if ((!map.contains(n) || map(n).isAir) && !items.contains(n)) {
      for (neighbor <- neighbors(n).filter(inBounds)) {
        stack.push(neighbor)
        items.add(neighbor)
      }
      items.add(n)
    }
  }
  items.toList
}

val freeAir = floodFill(Vec3i(westBound, downBound, southBound), daData)

println(freeAir)
