package gay.menkissing.common

import cats.*
import cats.syntax.all.*

case class Grid[A] private (values: Vector[Vector[A]])
    extends PartialFunction[Vec2[Int], A]:
  val width: Int = values.head.length

  val height: Int = values.length

  private[common] def checkInvariants(): this.type =
    require(values.forall(_.lengthCompare(width) == 0))
    this

  // assert(values.forall(_.lengthCompare(width) == 0))

  def isDefinedAt(x: Int, y: Int): Boolean =
    (x >= 0 && x < width) && (y >= 0 && y < height)

  def isDefinedAt(n: Int): Boolean =
    val (x, y) = nToXY(n)
    isDefinedAt(x, y)

  def isDefinedAt(v: Vec2[Int]): Boolean = isDefinedAt(v.x, v.y)

  def get(x: Int, y: Int): Option[A] =
    if isDefinedAt(x, y) then Some(apply(x, y))
    else None

  def get(n: Int): Option[A] =
    val (x, y) = nToXY(n)
    get(x, y)
  def indexWhere(f: A => Boolean): Option[Vec2[Int]] =
    var x = 0
    val y =
      values.indexWhere: it =>
        x = it.indexWhere(f)
        x != -1
    if x == -1 || y == -1 then None
    else Some(Vec2(x, y))
  def get(p: Vec2[Int]): Option[A] = get(p.x, p.y)

  def getOrElse(p: Vec2[Int], orElse: => A): A = applyOrElse(p, _ => orElse)

  def apply(x: Int, y: Int): A = values(y)(x)

  def apply(n: Int): A =
    val (x, y) = nToXY(n)
    apply(x, y)

  def apply(p: Vec2[Int]): A = apply(p.x, p.y)

  def extractRow(y: Int): Seq[A] = values(y).toSeq

  def extractColumn(x: Int): Seq[A] = values.transpose.apply(x).toSeq

  def rows: Seq[Seq[A]] = values.map(_.toSeq)

  def columns: Seq[Seq[A]] = values.transpose.toSeq.map(_.toSeq)

  private def nToXY(n: Int): (Int, Int) =
    (n % width, Math.floor(n / width).toInt)
  def updated(x: Int, y: Int)(v: A): Grid[A] =
    Grid(values.updated(y, values(y).updated(x, v)))
  def updated(n: Int)(v: A): Grid[A] =
    val (x, y) = nToXY(n)
    updated(x, y)(v)
  def updated(p: Vec2[Int])(v: A): Grid[A] = updated(p.x, p.y)(v)
  def expand(default: A)(n: Int): Grid[A] =
    Grid[A](
      values.map[Vector[A]](
        _.prependedAll(Vector.fill[A](n)(default))
          .appendedAll(Vector.fill[A](n)(default))
      ).appendedAll(Vector.fill(n, width + n * 2)(default))
        .prependedAll(Vector.fill(n, width + n * 2)(default))
    )
  def expandDir(default: A)(n: Int, dir: Direction2D): Grid[A] =
    val newData =
      dir match
        case Direction2D.Up =>
          values.prependedAll(Vector.fill(n, width)(default))
        case Direction2D.Down =>
          values.appendedAll(Vector.fill(n, width)(default))
        case Direction2D.Left =>
          values.map[Vector[A]](_.prependedAll(Vector.fill[A](n)(default)))
        case Direction2D.Right =>
          values.map[Vector[A]](_.appendedAll(Vector.fill[A](n)(default)))
    Grid(values)
  def valuesAround(default: A)(x: Int, y: Int): Grid[A] =
    val foo =
      for
        y <- (y - 1) to (y + 1)
        x <- (x - 1) to (x + 1)
      yield get(x, y).getOrElse(default)
    Grid(foo, 3)
  def slice(start: Vec2[Int], end: Vec2[Int]): Grid[A] =
    val mxY = start.y `max` end.y
    val mxX = start.x `max` end.x
    val mnY = start.y `min` end.y
    val mnX = start.x `min` end.x
    Grid((mnY to mxY).map(y => (mnX to mxX).map(x => apply(x, y))))
  def flatten: Vector[A] = values.flatten

  def combineHorizontal(that: Grid[A]): Grid[A] =
    require(this.height == that.height)
    // YAY I LOVE CATS
    Grid(values.alignCombine(that.values))

  def combineVertical(that: Grid[A]): Grid[A] =
    require(this.width == that.width)
    Grid(values ++ that.values)

  def tileN(x: Int, y: Int): Grid[A] =
    List.fill(y, x)(this).map(_.reduce(_.combineHorizontal(_)))
      .reduce(_.combineVertical(_))

  // Requires that all returned grids are the same size
  def scaleWith[B](f: A => Grid[B]): Grid[B] =
    val grids = values.map(_.map(f))
    val w = grids.head.head.width
    val h = grids.head.head.height
    require(grids.flatten.forall(it => it.width == w && it.height == h))
    // I feel like APL had a special operator for doing this kind of stuff on lists
    grids.map(_.reduce((l, r) => l.combineHorizontal(r)))
      .reduce((l, r) => l.combineVertical(r))

  // Requires that width and height are divisible w and h
  def scaleDownWith[B](w: Int, h: Int)(f: Grid[A] => B): Grid[B] =
    require(width % w == 0 && height % h == 0)
    val xc = width / w
    val yc = height / h
    val slices = (0 until yc).flatMap: y =>
      (0 until xc).map(x =>
        f(this.slice(Vec2(x * w, y * h), Vec2(x * w + w - 1, y * h + h - 1)))
      )
    Grid(slices, xc)

  def indices: Seq[(Int, Int)] =
    for
      y <- 0 until height
      x <- 0 until width
    yield (x, y)

  def zipWithIndices: Seq[(A, Vec2[Int])] =
    for
      y <- 0 until height
      x <- 0 until width
    yield (this(x, y), Vec2(x, y))

  def mapWithIndex(f: (Vec2[Int], A) => A): Grid[A] =
    Grid:
      values.zipWithIndex.map: (row, y) =>
        row.zipWithIndex.map: (p, x) =>
          f(Vec2(x, y), p)

object Grid:
  def apply[A](values: Iterable[A], width: Int): Grid[A] =
    require(values.nonEmpty && values.size % width == 0)
    Grid[A](values.grouped(width).toVector.map(_.toVector))

  def apply[A](values: IterableOnce[IterableOnce[A]]): Grid[A] =
    Grid[A](values.iterator.to(Vector).map(_.iterator.to(Vector)))
      .checkInvariants()
  def fill[A](x: Int, y: Int)(v: => A) = Grid[A](Vector.fill(y, x)(v))

  def fromSparse[A](x: Int, y: Int, map: Map[Vec2[Int], A])(default: => A) =
    Grid[A](Vector.tabulate(y, x)((y, x) => map.getOrElse(Vec2(x, y), default)))

  def fromStringWithIndex[A](str: String)(fn: (Vec2[Int], Char) => A): Grid[A] =
    Grid(
      str.linesIterator.zipWithIndex
        .map((s, y) => s.zipWithIndex.map((c, x) => fn(Vec2(x, y), c)))
    ).checkInvariants()

  def fromString[A](str: String)(fn: Char => A): Grid[A] =
    Grid(str.linesIterator.map(_.map(fn))).checkInvariants()

  given gridShow[A](using s: Show[A]): Show[Grid[A]] with
    def show(t: Grid[A]): String =
      t.rows.map(it => it.map(s.show).fold("")(_ + " " + _))
        .fold[String]("")(_ + "\n" + _)

  val showBooleanGrid: Show[Grid[Boolean]] =
    _.rows.map(_.map(if _ then '#' else '.').mkString).mkString("\n")

  given gridFunctor: Functor[Grid] with
    def map[A, B](fa: Grid[A])(f: A => B): Grid[B] =
      Grid(fa.values.map(_.map(f.apply)))

  given gridEq[A](using Eq[A]): Eq[Grid[A]] with
    def eqv(x: Grid[A], y: Grid[A]): Boolean = x.values === y.values

  given gridFoldable: Foldable[Grid] with
    override def foldLeft[A, B](fa: Grid[A], b: B)(f: (B, A) => B): B =
      fa.flatten.foldLeft(b)(f)

    override def foldRight[A, B]
      (fa: Grid[A], lb: Eval[B])
      (f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.flatten.foldr[B](lb)(f)
