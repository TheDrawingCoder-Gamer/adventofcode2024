package gay.menkissing.advent
package y2022

import scala.collection.mutable as mut

object Day05 extends Problem[(Day05.Palettes, List[Day05.CraneMove]), String]:
  lazy val input = FileIO.getInput(2022, 5)

  def parsePalletes(input: String): Palettes =
    Palettes(input.linesIterator.map(parseRow).toVector.reverse.transpose[Option[Crate]]
                  .map(it => mut.ListBuffer.from(it.filter(_.isDefined).map(_.get))))

  def parseRow(input: String): List[Option[Crate]] =
    var good = input
    var temp = true
    Iterator.continually:
      val crate = good.take(3)
      good = good.drop(4)
      if (crate.startsWith("["))
        Some(crate(1))
      else
        None
    .takeWhile: _ =>
      val cur = temp
      temp = good.nonEmpty
      cur
    .toList

  def parse(input: String): (Palettes, List[CraneMove]) =
    val l :: (r :: _) = input.split("\n\n").toList: @unchecked
    (parsePalletes(l), parseMoves(r))


  def parseMoves(input: String): List[CraneMove] =
    input.linesIterator.map:
      case s"move $x from $y to $z" =>
        CraneMove(x.toInt, y.toInt - 1, z.toInt - 1)
    .toList

  type Crate = Char
  // Ordered from bottom to top
  type Pallete = mut.ListBuffer[Crate]


  class Palettes(val palletes: Vector[Pallete]):
    private def moveWith(n: Int, from: Int, to: Int)(extractor: Pallete => mut.ListBuffer[Crate]): Unit =
      val crates = extractor(palletes(from))
      palletes(from).dropRightInPlace(n)
      palletes(to) ++= crates


    def moves(n: Int, from: Int, to: Int): Unit =
      moveWith(n, from, to)(_.takeRight(n).reverse)

    def movesMany(n: Int, from: Int, to: Int): Unit = moveWith(n, from, to)(_.takeRight(n))

    def perform(craneMove: CraneMove): Unit =
      moves(craneMove.n, craneMove.from, craneMove.to)

    def performMany(craneMove: CraneMove): Unit =
      movesMany(craneMove.n, craneMove.from, craneMove.to)

    def performs(craneMoves: Seq[CraneMove]): Unit =
      craneMoves.foreach(perform)

    def performsMany(craneMoves: Seq[CraneMove]): Unit =
      craneMoves.foreach(performMany)

  case class CraneMove(n: Int, from: Int, to: Int)

  def part1(input: (Palettes, List[CraneMove])): String =
    val (pals, moves) = input
    pals.performs(moves)
    pals.palletes.map(_.last).mkString("")

  def part2(input: (Palettes, List[CraneMove])): String =
    val (pals, moves) = input
    pals.performsMany(moves)
    pals.palletes.map(_.last).mkString("")