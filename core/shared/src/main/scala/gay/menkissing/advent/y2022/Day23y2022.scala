package gay.menkissing.advent
package y2022

import gay.menkissing.common.*

object Day23y2022 extends Problem[Grid[Boolean], Int] {
  type ElfGrid =  Grid[Boolean]

  case class Elf(current: Vec2i, proposal: Option[Vec2i])

  val order: Vector[Direction2D] = Vector(Direction2D.Up, Direction2D.Down, Direction2D.Left, Direction2D.Right)

  def propose(grid: ElfGrid, moveOffset: Int, pos: Vec2i): (Vec2i, Vec2i) = {
    if (!grid(pos)) return pos -> pos
    if (!pos.allNeighbors.exists(p => grid.getOrElse(p, false))) return pos -> pos

    val noe = grid.getOrElse(Vec2i(pos.x + 1, pos.y - 1), false)
    val n = grid.getOrElse(pos.copy(y = pos.y - 1), false)
    val nw = grid.getOrElse(Vec2i(pos.x - 1, pos.y - 1), false)
    val sw = grid.getOrElse(Vec2i(pos.x - 1, pos.y + 1), false)
    val se = grid.getOrElse(Vec2i(pos.x + 1, pos.y + 1), false)
    val s = grid.getOrElse(pos.copy( y = pos.y + 1), false)
    val e = grid.getOrElse(pos.copy(x = pos.x + 1), false)
    val w = grid.getOrElse(pos.copy(x = pos.x - 1), false)
    val northValid = !noe && !n && !nw 
    val southValid = !se && !s && !sw 
    val eastValid = !se && !e && !noe 
    val westValid = !sw && !w && !nw
    (0 until 4).map(order.access(moveOffset)).find { it =>
      it match
        case Direction2D.Up => northValid 
        case Direction2D.Down => southValid 
        case Direction2D.Left => westValid 
        case Direction2D.Right => eastValid 
      
    }.map(i => pos -> pos.offset(i)).getOrElse(pos -> pos)
  }
  def round(grid: ElfGrid, moveOffset: Int): (ElfGrid, Boolean) = {
    val elfs =
      for {
        y <- 0 until grid.height
        x <- 0 until grid.width if grid(x, y)
      } yield {
        propose(grid, moveOffset, Vec2i(x, y))
      }
    val daProposals = elfs.filter((x, y) => x != y).groupBy(_._2)
    val goodPositions = daProposals.filter(_._2.lengthCompare(1) == 0).map((x, y) => y.head)
    if goodPositions.isEmpty then
      (grid, true)
    else
      var daGrid = grid.expand(false)(1)
      for {
        (from, to) <- goodPositions
      } do {
        daGrid = daGrid.updated(from + Vec2i(1, 1))(false).updated(to + Vec2i(1, 1))(true)
      }

      (daGrid, false)
  }
  def parse(input: String): ElfGrid = {
    Grid[Boolean](input.linesIterator.map(_.map {
      case '#' => true 
      case _ => false 
    }))
  }
  extension (elfGrid: ElfGrid)
    def sliced: Grid[Boolean] =
      val rows = elfGrid.rows
      val cols = elfGrid.columns
      val topBound = rows.indexWhere (_.exists (identity) )
      val botBound = rows.lastIndexWhere (_.exists (identity) )
      val rightBound = cols.lastIndexWhere (_.exists (identity) )
      val leftBound = cols.indexWhere (_.exists (identity) )

      // println(prettyShowBoolGrid(resGrid))
      elfGrid.slice(Vec2i(leftBound, topBound), Vec2i(rightBound, botBound))

  def part1(grid: ElfGrid): Int = {
    //println(prettyShowBoolGrid(grid))
    
    val (resGrid, _) = (0 until 10).foldLeft((grid, 0)):
      case ((grid, offset), _) =>
        val (newGrid, _) = round(grid, offset)
        //println(prettyShowBoolGrid(newGrid))
        (newGrid, (offset + 1) % 4)

    
    resGrid.sliced.flatten.count(!_)
  }

  extension (order: Vector[Direction2D])
    def access(offset: Int)(idx: Int): Direction2D = order((idx + offset) % 4)

  def part2(grid:ElfGrid): Int = {
    Iterator.iterate((grid, 0, false)): (grid, offset, _) =>
      val (newGrid, didntMove) = round(grid, offset)
      (newGrid, (offset + 1) % 4, didntMove)
    .indexWhere(_._3)
  }

  def prettyShowBoolGrid(grid: Grid[Boolean]): String = {
    grid.rows.map { it => 
      it.map(if (_) '#' else '.').mkString 
    }.foldLeft("")(_ + "\n" + _)
  }

  override lazy val input: String = FileIO.getInput(2022, 23)

}
