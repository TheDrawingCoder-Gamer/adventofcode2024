package gay.menkissing.advent
package y2024

import scala.collection.mutable

object Day12 extends Problem:
  type Input = PlantMap
  type Output = Int

  type Region = Vector[(Int, Int)]
  def cardinalPositions(x: Int, y: Int): List[(Int, Int)] =
    List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))

  def neighborPositions(ix: Int, iy: Int): List[(Int, Int)] = (ix - 1 to ix + 1)
    .flatMap: x =>
      (iy - 1 to iy + 1).flatMap: y =>
        Option.when(x != ix || y != iy)((x, y))
    .toList

  extension (region: Region)
    def pretty: String =
      val minX = region.minBy(_._1)._1
      val minY = region.minBy(_._2)._2
      val maxX = region.maxBy(_._1)._1
      val maxY = region.maxBy(_._2)._2
      val res = mutable.ArrayBuffer.fill(maxY - minY + 1, maxX - minX + 1)('.')
      region.foreach: (x, y) =>
        res(y - minY)(x - minX) = '#'
      res.map(_.mkString("", "", "")).mkString("", "\n", "")

    def asPlantMap: PlantMap =
      val maxX = region.maxBy(_._1)._1
      val maxY = region.maxBy(_._2)._2
      val res = mutable.ArrayBuffer.fill(maxY + 1, maxX + 1)('.')
      region.foreach: (x, y) =>
        res(y)(x) = '#'
      PlantMap(res.map(_.mkString("", "", "")).toVector)

    def inflate: Region =
      region.flatMap((x, y) =>
        List(
          (x * 2, y * 2),
          (x * 2 + 1, y * 2),
          (x * 2, y * 2 + 1),
          (x * 2 + 1, y * 2 + 1)
        )
      )

    def sides: Int =
      val bigRegion = region.inflate
      val regionMap = bigRegion.asPlantMap
      bigRegion.count: (x, y) =>
        val neighborCount =
          regionMap.optionalNeighbors(x, y).count(_.contains('#'))
        neighborCount match
          case 3 | 4 | 7 => true
          case _         => false

    def area: Int = region.size
    def perimeter: Int =
      val regionMap = region.asPlantMap
      region.map((x, y) =>
        regionMap.optionalCardinalNeighbors(x, y).count(_.forall(_ != '#'))
      ).sum

  final case class PlantMap(plants: Vector[String]):
    val height: Int = plants.size
    val width: Int = plants.head.length
    // Length should be equal
    assert(plants.forall(_.length == width))

    def apply(x: Int, y: Int): Char = plants(y)(x)

    def get(x: Int, y: Int): Option[Char] =
      Option.when(isDefinedAt(x, y))(apply(x, y))

    def isDefinedAt(x: Int, y: Int): Boolean =
      x >= 0 && x < width && y >= 0 && y < height

    def indices: Vector[(Int, Int)] = (0 until height)
      .flatMap(y => (0 until width).map(x => (x, y))).toVector

    def optionalCardinalNeighbors(x: Int, y: Int): List[Option[Char]] =
      cardinalPositions(x, y).map(get)

    def optionalNeighbors(x: Int, y: Int): List[Option[Char]] =
      neighborPositions(x, y).map(get)

    def floodFill(x: Int, y: Int): Region =
      val q = mutable.Queue[(Int, Int)]()
      val char = apply(x, y)
      val res = mutable.Set[(Int, Int)]()
      q.addOne((x, y))
      while q.nonEmpty do
        val n = q.removeHead()
        if get(n._1, n._2).contains(char) && !res.contains(n) then
          res += n
          q.addAll(cardinalPositions(n._1, n._2))

      res.toVector

    def regions: List[Region] =
      List.unfold[Region, Vector[(Int, Int)]](this.indices): acc =>
        acc.headOption.map: head =>
          val points = floodFill(head._1, head._2)
          (points, acc.diff(points))

  def parse(str: String): PlantMap = PlantMap(str.linesIterator.toVector)

  def part1(plants: PlantMap): Int =
    plants.regions.map(r => r.area * r.perimeter).sum

  def part2(plants: PlantMap): Int =
    plants.regions.map(r => r.area * r.sides).sum

  override def input: String = FileIO.getInput(2024, 12)
