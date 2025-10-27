package gay.menkissing.advent
package y2022

import gay.menkissing.common.*
import scala.collection.mutable as mut
import cats.syntax.all.*
import Vec2i.*
import spire.implicits.IntAlgebra

object Day22 extends Problem[(Day22.ForbiddenGrid, List[Day22.Instruction]), Int] {
  enum GridPos {
    case Solid, Open, GNil 
  }
  import GridPos.* 
  extension (s: Seq[GridPos]) {
    def trimPos: Seq[GridPos] = {
      s.dropWhile(_ == GNil).takeWhile(_ != GNil)
    }
  }
  extension (regionMap: Map[Int, Vec2i])
    def pretty: String =
      val buff = Array.fill(5, 5)('.')
      regionMap.foreach:
        case (region, Vec2i(x, y)) =>
          buff(y)(x) = region.toString.head
      buff.map(_.mkString).mkString("\n")
  val netConnections: List[(Map[Int, Vec2i], List[ConnectionInfo])] =
    List(
      // Shape
      // .12
      // .3.
      // 45.
      // 6..
      // Net 8
      ConnectionParser.parse(
        List(
          ".12..",
          ".3...",
          "45...",
          "6....",
          "....."
        ),
        List(
          "16UR-", "14LRf",
          "26UU-", "25RLf", "23DL-",
          "34LD-", "32RU-",
          "43UR-", "41LRf",
          "52RLf", "56DL-",
          "61LD-", "62DD-", "65RU-"
        )
      ),
      // "Offset cross" net
      ConnectionParser.parse(
        List(
          "..1..",
          "234..",
          "..56.",
          ".....",
          "....."
        ),
        List(
          "12UDf", "13LD-", "16RLf",
          "21UDf", "26LUf", "25DUf",
          "31UR-", "35DRf",
          "46RDf",
          "53LUf", "52DUf",
          "64ULf", "61RLf", "62DRf"
        )
      ),
      // "T" Net
      ConnectionParser.parse(
        List(
          "123..",
          ".4...",
          ".5...",
          ".6..."
        ),
        List(
          "14DRf", "15LRf", "16UR-",
          "26UU-",
          "34DL-", "35RLf", "36ULf",
          "41LUf", "43RU-",
          "51LRf", "53RLf",
          "61LD-", "63RDf", "62DD-"
        )
      ),
      // "Hi F" net
      ConnectionParser.parse(
        List(
          ".12..",
          "34...",
          ".5...",
          ".6..."
        ),
        List(
          "13LD-", "16UU-",
          "24DL-", "25RLf", "26ULf",
          "31UR-", "36LRf", "35DRf",
          "42RU-",
          "52RLf", "53LUf",
          "61DD-", "62RDf", "63RLf"
        )
      ),
      // "Lo F" Net
      ConnectionParser.parse(
        List(
          ".12..",
          ".3...",
          "45...",
          ".6..."
        ),
        List(
          "14LRf", "16UU-",
          "23DL-", "25RLf", "26ULf",
          "32RU-", "34LD-",
          "41LRf", "43UR-", "46DRf",
          "52RLf",
          "61DD-", "62RDf", "64LUf"
        )
      ),
      // "Long" Net
      ConnectionParser.parse(
        List(
          "1....",
          "2....",
          "34...",
          ".5...",
          ".6..."
        ),
        List(
          "14RLf", "15ULf", "16LL-",
          "26LUf", "24RDf",
          "36LRf", "35DRf",
          "41RLf", "42ULf",
          "51RDf", "53LUf",
          "61RR-", "62DRf", "63LRf"
        )
      ),
      // "Cross" net
      ConnectionParser.parse(
        List(
          ".1...",
          "234..",
          ".5...",
          ".6..."
        ),
        List(
          "12LD-", "14RDf", "16UU-",
          "21UR-", "25DRf", "26LRf",
          // No 3
          "41ULf", "45DL-", "46RLf",
          "52LUf", "54RU-",
          "61DD-", "62LRf", "64RLf"
        )
      ),
      // "Short S" Net
      ConnectionParser.parse(
        List(
          ".1...",
          ".23..",
          "45...",
          ".6..."
        ),
        List(
          "13RDf", "14LRf", "16UU-",
          "24LD-",
          "31ULf", "35DL-", "36RLf",
          "41LRf", "42UR-", "46DRf",
          "53RU-",
          "61DD-", "63RLf", "64LUf"
        )
      ),
      // "Long S" Net
      ConnectionParser.parse(
        List(
          ".12..",
          ".3...",
          ".4...",
          "56..."
        ),
        List(
          "15LUf", "16UU-",
          "23DL-", "24RLf", "26ULf",
          "32RU-", "35LRf",
          "42RLf", "45LD-",
          "51DRf", "53LRf", "54UR-",
          "61DD-", "62RDf"
        )
      ),
      // Net 7
      ConnectionParser.parse(
        List(
          "..1..",
          ".23..",
          "45...",
          ".6..."
        ),
        List(
          "12LD-", "14UR-", "16RU-",
          "21UR-", "24LD-",
          "35DL-", "36RLf",
          "41LD-", "42UR-", "46DRf",
          "53RU-",
          "61DL-", "63RLf", "64LUf"
        )
      ),
      // Net 10 (zigzag)
      ConnectionParser.parse(
        List(
          "1....",
          "23...",
          ".45..",
          "..6.."
        ),
        List(
          "13RDf", "15ULf", "16LL-",
          "24DRf", "26LUf",
          "31ULf", "35RDf",
          "42LUf", "46DRf",
          "51RDf", "53ULf",
          "61RR-", "62DRf", "64LUf"
        )
      )
    ).flatten


  case class ConnectionInfo(thisDir: Direction2D, thatDir: Direction2D, thisPos: Int, thatPos: Int, flipOffset: Boolean):
    def traverse(startOffset: Int, maxOffset: Int): Int =
      if flipOffset then
        maxOffset - 1 - startOffset
      else
        startOffset

  object ConnectionParser:
    def directionFromChar(c: Char): Direction2D =
      c match
        case 'L' => Direction2D.Left
        case 'R' => Direction2D.Right
        case 'D' => Direction2D.Down
        case 'U' => Direction2D.Up


    def rotated(net: List[String], list: List[ConnectionInfo]): (List[String], List[ConnectionInfo]) =
      // Rotating??
      // 0, 0, to 4, 0
      val newNet = net.padTo(5, ".....").zipWithIndex.flatMap((r, y) => r.zipWithIndex.map((c, x) => c -> (4 - y, x)))
                      .sortBy((_,  t) => (t._2, t._1)).map(_._1).grouped(5).map(_.mkString).toList.dropWhile(_.forall(_ == '.'))
      val newList = list.map:
        case ConnectionInfo(sd, ed, s, e, f) =>
          ConnectionInfo(sd.clockwise, ed.clockwise, s, e, f)
      (newNet, newList)
    def fullParsed(net: List[String], list: List[ConnectionInfo]): (Map[Int, Vec2i], List[ConnectionInfo]) =
      val regionMap = net.zipWithIndex.flatMap: (line, y) =>
        line.zipWithIndex.flatMap: (c, x) =>
          Option.when(c.isDigit)(c.asDigit -> Vec2i(x, y))
      (regionMap.toMap, list)
    def parse(net: List[String], list: List[String]): List[(Map[Int, Vec2i], List[ConnectionInfo])] =
      val baseList =
        list.map: str =>
          val start = str(0).asDigit
          val end = str(1).asDigit
          val startDir = directionFromChar(str(2))
          val endDir = directionFromChar(str(3))
          ConnectionInfo(startDir, endDir, start, end, str(4) == 'f')
      val baseNet = net
      val (net1, list1) = rotated(baseNet, baseList)
      val (net2, list2) = rotated(net1, list1)
      val (net3, list3) = rotated(net2, list2)
      val (net4, list4) = (baseNet.map(_.reverse).transpose.dropWhile(_.forall(it => it == '.')).transpose.map(_.mkString), baseList.map(it => it.copy(thisDir = it.thisDir.flipHorizontal, thatDir = it.thatDir.flipHorizontal)))
      val (net5, list5) = rotated(net4, list4)
      val (net6, list6) = rotated(net5, list5)
      val (net7, list7) = rotated(net6, list6)

      List(
        fullParsed(baseNet, baseList),
        fullParsed(net1, list1),
        fullParsed(net2, list2),
        fullParsed(net3, list3),
        fullParsed(net4, list4),
        fullParsed(net5, list5),
        fullParsed(net6, list6),
        fullParsed(net7, list7)
      )




  case class CompleteGrid(regionMap: Map[Int, Vec2i], connections: List[ConnectionInfo], grid: Grid[GridPos]) {
    val faceWidth = math.min(grid.rows.map(_.trimPos.size).min, grid.columns.map(_.trimPos.size).min)
    val reverseRegionMap = regionMap.map((x, y) => (y, x))
    val connMap: Map[(Int, Direction2D), (Int, Direction2D, Boolean)] =
      connections.map:
        case ConnectionInfo(exitDir, arriveDir, startPos, endPos, flipOffset) =>
          (startPos, exitDir) -> (endPos, arriveDir, flipOffset)
      .toMap

    def move(pos: Vec2i, dir: Direction2D, n: Int): (Vec2i, Direction2D) =
      moveHelper(pos, dir, n) { (poses, lastPos) =>
        val lastRegionPos = Vec2i(lastPos.x / faceWidth, lastPos.y / faceWidth)
        val lastRegion = reverseRegionMap(lastRegionPos)
        val (nextRegion, arrivalDir, flipOffset) = connMap(lastRegion, dir)


        val regionPos = regionMap(nextRegion)

        val startOffset =
          dir.axis match
            case Axis2D.X => lastPos.y % faceWidth
            case Axis2D.Y => lastPos.x % faceWidth
        val newOffset =
          if flipOffset then
            faceWidth - 1 - startOffset
          else
            startOffset

        val newPos =
          arrivalDir match
            case Direction2D.Up => Vec2i((regionPos.x * faceWidth) + newOffset, (regionPos.y + 1) * faceWidth - 1)
            case Direction2D.Down => Vec2i((regionPos.x * faceWidth) + newOffset, regionPos.y * faceWidth)
            case Direction2D.Left => Vec2i((regionPos.x + 1) * faceWidth - 1, (regionPos.y * faceWidth) + newOffset)
            case Direction2D.Right => Vec2i(regionPos.x * faceWidth, (regionPos.y * faceWidth) + newOffset)
        assert(newPos != GNil)
        if grid(newPos) != Open then
          (lastPos, dir)
        else
          val countUntilEdge = poses.indexWhere(it => grid.getOrElse(it, GNil) == GNil)
          val restCount = n - countUntilEdge
          move(newPos, arrivalDir, restCount)

      }

    final def moveHelper(pos: Vec2i, dir: Direction2D, n: Int)(handleOff: (Seq[Vec2i], Vec2i) => (Vec2i, Direction2D)): (Vec2i, Direction2D) = {
      require(grid.getOrElse(pos, GNil) == Open)
      if (n == 0) return (pos, dir)
      val newPos = pos.offset(dir, n)
      val line = pos `straightLine` newPos

      if (line.forall(it => grid.getOrElse(it, GNil) != Solid)) {
        if (line.forall(it => grid.getOrElse(it, GNil) != GNil)) {
          // simply return new pos
          (newPos, dir)
        } else {
          handleOff(line, line.findLast(it => grid.getOrElse(it, GNil) != GNil).get)
        }
      } else {
        assert(grid.getOrElse(line.head, GNil) != Solid)
        val solidIdx = line.indexWhere(it => grid.getOrElse(it, GNil) == Solid)
        val openIdx = solidIdx - 1
        val openPos = line(openIdx)
        assert(grid.get(openPos).isDefined && grid(openPos) == Open)
        (openPos, dir)
      }
    }
  }


  case class ForbiddenGrid(grid: Grid[GridPos]) extends AnyVal {
    def regions: List[Vec2i] = {
      val faceWidth = math.min(grid.rows.map(_.trimPos.size).min, grid.columns.map(_.trimPos.size).min)
      val height = grid.height / faceWidth
      val width = grid.width / faceWidth
      (0 until width).flatMap: x =>
        (0 until height).flatMap: y =>
          Option.when(grid(x * faceWidth, y * faceWidth) != GNil)(Vec2i(x, y))
      .toList
    }

    def completeGrid: CompleteGrid =
      val regions = this.regions.toSet
      val (regionMap, conns) = netConnections.find: (regionMap, _) =>
        regions == regionMap.values.toSet
      .get
      // println(regionMap.pretty)
      CompleteGrid(regionMap, conns, grid)


   // helper means no recursion is accessable
    // @annotation.tailrec 
    final def move(pos: Vec2i, dir: Direction2D, n: Int): Vec2i = {
      def getGood(rowOrCol: Seq[GridPos]): Option[GridPos] = {
        dir.axisDirection match {
          case Axis2D.Direction.Positive => rowOrCol.find(_ != GNil)
          case Axis2D.Direction.Negative => rowOrCol.findLast(_ != GNil)
        }
      }
      def getPos(rowOrCol: Seq[GridPos]): Int = {
        dir.axisDirection match {
          case Axis2D.Direction.Positive => rowOrCol.indexWhere(_ != GNil)
          case Axis2D.Direction.Negative => rowOrCol.lastIndexWhere(_ != GNil)
        }
      }
      moveHelper(pos, dir, n) { (line, lastPos) =>
        val countUntilEdge = line.indexWhere(it => grid.getOrElse(it, GNil) == GNil)
        val restCount = n - countUntilEdge
        lazy val solidEnd = line.findLast(it => grid.getOrElse(it, GNil) != GNil)
        
        dir.axis match {
            case Axis2D.X => 
              val row = grid.extractRow(pos.y)
              val goodX = getGood(row) 
              goodX match
                // End movement
                case Some(Solid) => solidEnd.get
                // Start movement from that side 
                case Some(Open) =>

                  val x = getPos(row)
                  assert(x != -1)
                  move(pos.copy(x = x), dir, restCount)
                // Can't be correct
                case _ => assert(false)
              
            case Axis2D.Y => 
              val col = grid.extractColumn(pos.x)
              val goodY = getGood(col)
              goodY match {
                case Some(Solid) => solidEnd.get
                case Some(Open) => 
                  val y = getPos(col)
                  assert(y != -1)
                  move(pos.copy(y = y), dir, restCount)
                case _ => assert(false)
              }
          }
      }
    }
    final def moveHelper(pos: Vec2i, dir: Direction2D, n: Int)(handleOff: (Seq[Vec2i], Vec2i) => Vec2i): Vec2i = {
      require(grid.getOrElse(pos, GNil) == Open)
      if (n == 0) return pos
      val newPos = pos.offset(dir, n)
      val line = pos `straightLine` newPos

      if (line.forall(it => grid.getOrElse(it, GNil) != Solid)) {
        if (line.forall(it => grid.getOrElse(it, GNil) != GNil)) {
          // simply return new pos 
          newPos 
        } else {
          handleOff(line, line.findLast( it => grid.getOrElse(it, GNil) != GNil).get)
        }
      } else {
        assert(grid.getOrElse(line.head, GNil) != Solid)
        val solidIdx = line.indexWhere(it => grid.getOrElse(it, GNil) == Solid)
        val openIdx = solidIdx - 1
        val openPos = line(openIdx)
        assert(grid.get(openPos).isDefined && grid(openPos) == Open)
        openPos
      } 
    }
  } 
  enum Instruction {
    case Clockwise
    case Counterclockwise
    case Move(n: Int)
  }
  lazy val input = FileIO.getInput(2022, 22)
  def parseInstructions(input: String): List[Instruction] = {
    @annotation.tailrec 
    def helper(i: String, accum: List[Instruction]): List[Instruction] = {
      i match {
        case "" => accum.reverse 
        case _ => 
          i.head match {
            case 'R' => helper(i.tail, accum.prepended(Instruction.Clockwise))
            case 'L' => helper(i.tail, accum.prepended(Instruction.Counterclockwise))
            case _ => 
              val n = i.takeWhile(_.isDigit)
              val r = i.dropWhile(_.isDigit)
              helper(r, accum.prepended(Instruction.Move(n.toInt)))
          }
      }
    }
    helper(input.strip, List()) 
  }
  def parseMap(input: String): ForbiddenGrid = {
    val lines = 
      input.linesIterator.map { line => 
        line.map { 
          case ' ' => GNil 
          case '.' => Open 
          case '#' => Solid 
        }.toList 
      }.toList
    val width = lines.maxBy(_.length).length 
    val good = lines.map(_.padTo(width, GNil))
    ForbiddenGrid(Grid(good))
  }
  def parse(input: String): (ForbiddenGrid, List[Instruction]) = {
    val List(l, r) = input.split("\n\n").toList: @unchecked 
    (parseMap(l), parseInstructions(r))
  }

  def part1(input: (ForbiddenGrid, List[Instruction])): Int = {
    val (map, instr) = input
    val x = map.grid.rows.head.indexOf(Open)
    var pos = Vec2i(x, 0)
    var facing = Direction2D.Right 
    instr.foreach {
      case Instruction.Clockwise => facing = facing.clockwise
      case Instruction.Counterclockwise => facing = facing.counterClockwise
      case Instruction.Move(n) => pos = map.move(pos, facing, n)
    }
    val facingN = 
      facing match
        case Direction2D.Up => 3
        case Direction2D.Down => 1
        case Direction2D.Left => 2
        case Direction2D.Right => 0
    val rowN = (pos.y + 1) * 1000
    val colN = (pos.x + 1) * 4
    rowN + colN + facingN
  }

  def part2(input: (ForbiddenGrid, List[Instruction])): Int = {
    val (map, instr) = input
    val completeGrid = map.completeGrid

    val x = map.grid.rows.head.indexOf(Open)
    var pos = Vec2i(x, 0)
    var facing = Direction2D.Right
    instr.foreach {
      case Instruction.Clockwise => facing = facing.clockwise
      case Instruction.Counterclockwise => facing = facing.counterClockwise
      case Instruction.Move(n) =>
        val (newPos, newFacing) = completeGrid.move(pos, facing, n)
        pos = newPos
        facing = newFacing
    }
    val facingN =
      facing match
        case Direction2D.Up => 3
        case Direction2D.Down => 1
        case Direction2D.Left => 2
        case Direction2D.Right => 0
    val rowN = (pos.y + 1) * 1000
    val colN = (pos.x + 1) * 4
    rowN + colN + facingN


  }

}
