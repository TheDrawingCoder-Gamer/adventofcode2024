import scala.io.Source
import scala.collection.mutable as mut

// use just the STD challenge

def cardinalPositions(x: Int, y: Int): List[(Int, Int)] = {
  List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
}

def neighborPositions(ix: Int, iy: Int): List[(Int, Int)] = {
  (ix - 1 to ix + 1).flatMap { x =>
    (iy - 1 to iy + 1).flatMap { y =>
      Option.when(x != ix || y != iy)((x, y))
    }
  }.toList


}


extension (b: Boolean) {
  def asBit: Int = if (b) 1 else 0
}

extension (region: Vector[(Int, Int)]) {
  def isolated: Vector[(Int, Int)] = {
    val minX = region.minBy(_._1)._1
    val minY = region.minBy(_._2)._2
    region.map((x, y) => (x - minX, y - minY))
  }
  def pretty: String = {
    val minX = region.minBy(_._1)._1
    val minY = region.minBy(_._2)._2
    val maxX = region.maxBy(_._1)._1
    val maxY = region.maxBy(_._2)._2
    val res = mut.ArrayBuffer.fill(maxY - minY + 1, maxX - minX + 1)('.')
    region.foreach { (x, y) =>
      res(y - minY)(x - minX) = '#'
    }
    res.map(_.mkString("", "", "")).mkString("", "\n", "")
  }
  def inflate: Vector[(Int, Int)] = {
    region.flatMap((x, y) => List((x * 2, y * 2), (x * 2 + 1, y * 2), (x * 2, y * 2 + 1), (x * 2 + 1, y * 2 + 1)))
  }

}


enum Direction {
  case North, South, East, West

  def clockwise: Direction = this match {
    case North => East
    case East => South
    case South => West
    case West => North
  }

  // lazy : )
  def counterClockwise: Direction = clockwise.clockwise.clockwise

  def digitalDir: (Int, Int) = this match {
    case North => (0, -1)
    case East => (1, 0)
    case South => (0, 1)
    case West => (-1, 0)
  }
}


extension (p: (Int, Int)) {
  def offset(dir: Direction, n: Int = 1): (Int, Int) = {
    val added = dir.digitalDir
    (p._1 + added._1 * n, p._2 + added._2 * n)
  }
}

object PlantMap {
  def fromRegion(region: Vector[(Int, Int)]): PlantMap = {
    PlantMap(region.pretty.linesIterator.toVector)
  }
}

case class PlantMap(map: Vector[String]) {

  val height = map.size
  val width = map.head.length
  // Length should be equal
  assert(map.forall(_.length == width))

  def apply(x: Int, y: Int): Char = {
    map(y)(x)
  }
  def get(x: Int, y: Int): Option[Char] = {
    Option.when(isDefinedAt(x, y))(apply(x, y))
  }
  def isDefinedAt(x: Int, y: Int): Boolean = {
    x >= 0 && x < width && y >= 0 && y < height
  }

  def flatten: Vector[Char] = map.flatten
  def indices: Vector[(Int, Int)] = {
    (for {
      y <- 0 until height
      x <- 0 until width
    } yield (x, y)).toVector
  }
  def flatZipWithIndex: Vector[(Char, (Int, Int))] = {
    (for {
      y <- 0 until height
      x <- 0 until width
    } yield (apply(x, y), (x, y))).toVector
  }
  def cardinalNeighbors(x: Int, y: Int): List[Char] = {
    optionalCardinalNeighbors(x, y).flatten
  }
  def optionalCardinalNeighbors(x: Int, y: Int): List[Option[Char]] = {
    List(get(x - 1, y), get(x + 1, y), get(x, y - 1), get(x, y + 1))
  }

  def optionalNeighbors(ix: Int, iy: Int): List[Option[Char]] = {
    neighborPositions(ix, iy).map(get)
  }

  def priceRegion(char: Char, region: Vector[(Int, Int)]): Int = {
    val perimeter = region.map((x, y) => optionalCardinalNeighbors(x, y).count(_.forall(_ != char))).sum
    region.size * perimeter
  }
  def priceRegionP2(region: Vector[(Int, Int)]): Int = {
    val sides = regionSides(region)
    region.size * sides
  }

  def countAreaPerimeter(char: Char): (Int, Int) = {
    val points = flatZipWithIndex.withFilter(_._1 == char).map(_._2)
    val perimeter = points.map {
      (x, y) => optionalCardinalNeighbors(x, y).count(_.forall(_ != char))
    }.sum
    (points.size, perimeter)
  }

  def countEqualNeighbors(x: Int, y: Int): Int = {
    val c = apply(x, y)
    cardinalPositions(x, y).count((x, y) => get(x, y).contains(c))
  }
  def regionSides(region: Vector[(Int, Int)]): Int = {
    // ???
    // inflate the region so that 1 block gaps dont cause sadness
    // this rules out certain edge cases when counting corners

    val bigRegion = region.isolated.inflate
    val regionMap = PlantMap.fromRegion(bigRegion)
    val corners = bigRegion.filter { (x, y) =>
      val neighborCount = regionMap.optionalNeighbors(x, y).count(_.contains('#'))
      neighborCount match {
        case 3 | 4 | 7 => true
        case _ => false
      }
    }
    /*
    println(corners.foldLeft(regionMap.map) { case (map, (x, y)) =>
      map.updated(y, map(y).updated(x, 'X'))
    }.mkString("", "\n", ""))
     */
    corners.size
  }

  def uniqueChars: Set[Char] = {
    flatten.toSet
  }

  def floodFill(x: Int, y: Int): (Char, Vector[(Int, Int)]) = {
    val q = mut.Queue[(Int, Int)]()
    val char = apply(x, y)
    val res = mut.ListBuffer[(Int, Int)]()
    q.addOne((x, y))
    while (q.nonEmpty) {
      val n = q.removeHead()
      if (get(n._1, n._2).contains(char) && !res.contains(n)) {
        res.prepend(n)
        q.addAll(cardinalPositions(n._1, n._2))
      }
    }
    (char, res.toVector.reverse)
  }

  def regions: List[(Char, Vector[(Int, Int)])] = {
    List.unfold[(Char, Vector[(Int, Int)]), Vector[(Int, Int)]](this.indices) { v =>
      v.headOption.map { head =>
        val points = floodFill(head._1, head._2)
        (points, v.diff(points._2))
      }
    }
  }

  def sumPrices: Int = {
    regions.map((c, vec) => priceRegion(c, vec)).sum
  }

  def part2Price: Int = {
    regions.map((_, vec) => priceRegionP2(vec)).sum
  }
}



def parse(str: String): PlantMap = {
  PlantMap(str.linesIterator.toVector)
}

def part1(input: String): Int = {
  val plants = parse(input)

  plants.sumPrices
}

def part2(input: String): Int = {
  val plants = parse(input)

  plants.part2Price
}


val input = Source.fromResource("day12.txt").mkString


part1(input)
part2(input)