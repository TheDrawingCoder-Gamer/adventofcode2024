package gay.menkissing.advent
package y2021

import gay.menkissing.common.*

import scala.io.Source

object Day20 extends Problem[(Vector[Boolean], Day20.Image), Int]:
  lazy val input = FileIO.getInput(2021, 20)

  


  case class Image(grid: Grid[Boolean], oobPixel: Boolean) {
    def apply(x: Int, y: Int): Boolean =
      grid.get(x, y).getOrElse(oobPixel)
    def expand(n: Int): Image =
      Image(grid.expand(oobPixel)(n), oobPixel)
    def height = grid.height
    def width = grid.width
    def valuesAround(x: Int, y: Int): Grid[Boolean] = grid.valuesAround(oobPixel)(x, y)
    def flatten: Vector[Boolean] = grid.flatten
    def countLitPixels: Int = grid.values.view.flatten.count(identity)
  }
  @annotation.tailrec
  final def boolsToInt(bs: List[Boolean], accum: Int = 0): Int = {
    bs match {
      case Nil => accum
      case true :: next => boolsToInt(next, (accum << 1) | 1)
      case false :: next => boolsToInt(next, (accum << 1) | 0)
    }
  }
  def convolute(algo: Vector[Boolean], img: Image): Image = {
    val newImg = Grid(for {
      y <- -1 to img.height
      x <- -1 to img.width
    } yield {
      val values = img.valuesAround(x, y)
      algo(boolsToInt(values.flatten.toList))
    }, img.width + 2)
    val daIndex = if img.oobPixel then 511 else 0
    Image(newImg, algo(daIndex))
  }

  def showGrid(grid: Grid[Boolean]): String = {
    grid.values.map { it =>
      it.map(if (_) '#' else '.')
    }.foldLeft("")((accum, data) => accum + "\n" + data.mkString("", "", ""))
  }

  override def parse(str: String): (Vector[Boolean], Image) =
    val List(t: String, b: String) = str.trim.split("\n\n").toList: @unchecked

    val algo = t.trim.map(_ == '#').toVector
    val image = Image(Grid(b.linesIterator.map(_.trim.map(_ == '#')).toVector), false)

    (algo, image)
    

  override def part1(input: (Vector[Boolean], Image)): Int =
    val (algo, img) = input
    Iterator.iterate(img)(convolute(algo, _)).drop(2).next().countLitPixels

  override def part2(input: (Vector[Boolean], Image)): Int =
    val (algo, img) = input
    Iterator.iterate(img)(convolute(algo, _)).drop(50).next().countLitPixels
