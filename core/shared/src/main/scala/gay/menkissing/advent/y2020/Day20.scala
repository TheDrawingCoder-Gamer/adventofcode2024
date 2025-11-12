package gay.menkissing.advent
package y2020

import gay.menkissing.common.*, algebras.given
import collection.mutable
import cats.syntax.all.*

object Day20 extends Problem:
  type Input = List[Tile]
  type Output = Long

  val debugger = Debuginator(active = true)

  def flipBits(n: Int): Int =
    var res = 0
    var n2 = n

    (0 until 10).foreach: _ =>
      res = res << 1

      if (n2 & 1) == 1 then res = res | 1

      n2 = n2 >> 1

    res

  def eqEdge(edge1: Int, edge2: Int): Boolean =
    edge1 == edge2 || flipBits(edge1) == edge2

  def seqToInt(seq: Seq[Boolean]): Int =
    seq.foldLeft(0): (a, b) =>
      (a << 1) + (if b then 1 else 0)

  extension (self: Grid[Boolean])
    def flip: Grid[Boolean] = Grid(self.values.map(_.reverse))
    def edges: List[Int] =
      val rows = self.rows
      val cols = self.columns
      val firstRow = seqToInt(rows.head)
      val lastRow = seqToInt(rows.last)
      val firstCol = seqToInt(cols.head)
      val lastCol = seqToInt(cols.last)
      val unflipped =
        List(
          firstRow,
          lastCol,
          lastRow,
          firstCol
        )
      val flipped = unflipped.map(flipBits)
      unflipped ++ flipped

  lazy val input = FileIO.getInput(2020, 20)

  def parse(str: String): Input =
    str.trim.split("\n\n").map: group =>
      val ls = group.linesIterator
      val s"Tile ${Read[Int](i)}:" = ls.next().runtimeChecked
      Tile(i, Grid.fromString(ls.mkString("\n"))(_ == '#'))
    .toList

  def numToDir(num: Int): Direction2D =
    num match
      case 0 => Direction2D.Up
      case 1 => Direction2D.Down
      case 2 => Direction2D.Left
      case 3 => Direction2D.Right
  def dirToRotateCount(dir: Direction2D): Int =
    dir match
      case Direction2D.Up    => 0
      case Direction2D.Down  => 2
      case Direction2D.Left  => 3
      case Direction2D.Right => 1

  def unary[A](ls: List[A]): Boolean =
    ls match
      case _ :: Nil => true
      case _        => false

  final case class EdgeInfo(bits: Int, dir: Direction2D, flipped: Boolean):
    def flip: EdgeInfo = EdgeInfo(flipBits(bits), dir, !flipped)

    def rotate(n: Int): EdgeInfo =
      val newDir = dir.rotate(n)
      // TODO: flip?
      copy(dir = newDir)

  final case class Placement
    (id: Int, rotation: Int, flipX: Boolean, flipY: Boolean)

  final class Tile
    (
      val id: Int,
      val tile: Grid[Boolean],
      val rotation: Int = 0,
      val flipped: Boolean = false
    ):
    lazy val edges: Vector[Int] = tile.edges.toVector

    override def equals(that: Any): Boolean =
      that match
        case x: Tile => this.id == x.id
        case _       => false

    override def hashCode(): Int = Integer.hashCode(id)

    override def toString(): String =
      s"Tile($id, rotation = $rotation, flipped = $flipped)"

    private def edgeFromEdge(edge: Edge): Int = edges(edge.ordinal)

    def selectEdge(dir: Direction2D): Int =
      val edgeIndex =
        if flipped then dirToRotateCount(dir) + 4 else dirToRotateCount(dir)
      edgeFromEdge(Edge.edges(rotation)(edgeIndex))

    def sharedEdges(that: Tile): Set[Int] =
      (this.edges, that.edges).tupled.collect:
        case (l, r) if l == r => l
      .toSet

    def neighborOf(that: Tile): Boolean =
      this != that && (this.edges, that.edges).tupled.exists:
        case (thisEdge, thatEdge) => thisEdge == thatEdge

    def flip: Tile = Tile(id, tile, rotation, !flipped)
    def rotate: Tile = Tile(id, tile, (rotation + 1) % 4, flipped)

    def hasEdge(edge: Int): Boolean = edges.contains(edge)

    def arranged(dir: Direction2D, edge: Int): Option[Tile] = (0 until 8).toList
      .foldM[Either[Tile, _], Tile](this):
        case (acc, i) =>
          if acc.selectEdge(dir) == edge then Left(acc)
          else if i == 3 then Right(acc.flip)
          else Right(acc.rotate)
      .match
        case Left(value) => Some(value)
        case Right(_)    => None

    def forceImage: Grid[Boolean] =
      val cropped =
        tile.slice(Vec2(1, 1), Vec2(tile.width - 2, tile.height - 2))
      val f = ((grid: Grid[Boolean]) => grid.rotate).repeated(rotation)
      val rotated = f(cropped)
      val flipTuah = if flipped then rotated.flip else rotated
      flipTuah

  final case class Image(tiles: List[Tile]):
    def neighborsOf(tile: Tile): List[Tile] =
      tiles.collect:
        case t if tile.neighborOf(t) => t

    lazy val neighbors = tiles.map(t => t -> neighborsOf(t)).toMap

    def corners: List[Tile] = neighbors.filter(_._2.length == 2).keys.toList

  def part1(input: List[Tile]): Long =
    val image = Image(input)

    image.corners.map(_.id.toLong).product

  // AWFUL. GOD IS DEAD
  enum Edge:
    case FrontUp
    case FrontRight
    case FrontDown
    case FrontLeft
    case BackUp
    case BackRight
    case BackDown
    case BackLeft

  object Edge:
    // This represents how many edgers we edge when we ede
    // We only flip horizontal, but we can still represent flip tuahing the other way with rotation
    val rot0Edges =
      Vector(
        FrontUp,
        FrontRight,
        FrontDown,
        FrontLeft,
        BackUp,
        FrontLeft,
        BackDown,
        FrontRight
      )
    val rot90Edges =
      Vector(
        BackLeft,
        FrontUp,
        BackRight,
        FrontDown,
        FrontLeft,
        FrontDown,
        FrontRight,
        FrontUp
      )
    val rot180Edges =
      Vector(
        BackDown,
        BackLeft,
        BackUp,
        BackRight,
        FrontDown,
        BackRight,
        FrontUp,
        BackLeft
      )
    val rot270Edges =
      Vector(
        FrontRight,
        BackDown,
        FrontLeft,
        BackUp,
        BackRight,
        BackUp,
        BackLeft,
        BackDown
      )

    val edges =
      Vector(
        rot0Edges,
        rot90Edges,
        rot180Edges,
        rot270Edges
      )

  def rotateN(grid: Grid[Boolean], n: Int): Grid[Boolean] =
    ((grid: Grid[Boolean]) => grid.rotate).repeated(n)(grid)
  def part2(input: List[Tile]): Long =
    val image = Image(input)
    // We know what is what neighbor, we just need to FLIP TUAH! FLIP THAT THANG!!!
    val neighbors = image.neighbors

    val corner = neighbors.find(_._2.length == 2).get._1

    val seen = mutable.Set.empty[Tile]
    val dim = math.sqrt(input.size).toInt
    val imageGrid = Array.fill[Tile](dim, dim)(null)

    def placeTile(tile: Tile, row: Int, col: Int): Unit =
      seen += tile
      imageGrid(row)(col) = tile

    val List(se1, se2) = neighbors(corner).map(corner.sharedEdges(_))
    val newCorner = (0 until 8).toList.foldM[Either[Tile, _], Tile](corner):
      case (acc, i) =>
        if se1(acc.selectEdge(Direction2D.Right)) &&
          se2(acc.selectEdge(Direction2D.Down))
        then Left(acc)
        else if i == 3 then Right(acc.flip)
        else Right(acc.rotate)
    .merge

    placeTile(newCorner, 0, 0)

    def assembleRec(tile: Tile, row: Int, col: Int): Unit =
      if row >= dim || col >= dim then return

      neighbors(tile).foreach: t =>
        if !seen(t) then
          if t.hasEdge(tile.selectEdge(Direction2D.Right)) then
            val newT =
              t.arranged(Direction2D.Left, tile.selectEdge(Direction2D.Right))
                .get
            placeTile(newT, row, col + 1)
            assembleRec(newT, row, col + 1)
          else if t.hasEdge(tile.selectEdge(Direction2D.Down)) then
            val newT =
              t.arranged(Direction2D.Up, tile.selectEdge(Direction2D.Down)).get
            placeTile(newT, row + 1, col)
            assembleRec(newT, row + 1, col)

    assembleRec(newCorner, 0, 0)

    // println(imageGrid.map(_.mkString(",")).mkString("\n"))

    val updatedTiles = imageGrid.map(_.map(_.forceImage))
    val newGrid =
      updatedTiles.map(_.reduce((l, r) => l.combineHorizontal(r)))
        .reduce((u, d) => u.combineVertical(d))

    val freakMonster =
      """|..................#.
         |#....##....##....###
         |.#..#..#..#..#..#...""".stripMargin
    val freakySet =
      Grid.fromString(freakMonster)(_ == '#').zipWithIndices
        .flatMap((b, p) => Option.when(b)(p)).toSet

    val seaMonsterNs = (0 until 4).map(i => rotateN(newGrid, i))
      .flatMap(i => List(i, i.flip)).map: g =>
        g.indices.count: (x, y) =>
          val p = Vec2(x, y)
          freakySet.forall(v => g.getOrElse(p + v, false))

    val freakMonsterN = seaMonsterNs.max

    newGrid.flatten.count(identity) - (freakySet.size * freakMonsterN)
