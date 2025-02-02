package gay.menkissing.common

import cats.*
import cats.syntax.all.*

case class Grid[A] private (values: Vector[Vector[A]]){
  val width: Int = values.head.length 

  val height: Int = values.length

  assert(values.forall(_.lengthCompare(width) == 0))

  def isDefinedAt(x: Int, y: Int): Boolean = {
    (x >= 0 && x < width) && (y >= 0 && y < height)
  }

  def isDefinedAt(n: Int): Boolean = {
    val (x, y) = nToXY(n)
    isDefinedAt(x, y)
  }

  def get(x: Int, y: Int): Option[A] = {
    if (isDefinedAt(x, y))
      Some(apply(x, y))
    else 
      None
  }

  def get(n: Int): Option[A] = {
    val (x, y) = nToXY(n)
    get(x, y)
  }
  def indexWhere(f: A => Boolean): Option[Vec2i] = {
    var x = 0  
    val y = values.indexWhere { it => 
      x = it.indexWhere(f)
      x != -1 
    }
    if (x == -1 || y == -1) 
      None 
    else 
      Some(Vec2i(x, y))
  }
  def get(p: Vec2i): Option[A] = get(p.x, p.y)

  def getOrElse(p: Vec2i, orElse: => A): A = get(p).getOrElse(orElse)

  def apply(x: Int, y: Int): A = values(y)(x)

  def apply(n: Int) : A = {
    val (x, y) = nToXY(n)
    apply(x, y)
  }

  def apply(p: Vec2i): A = apply(p.x, p.y)

  def extractRow(y: Int): Seq[A] = values(y).toSeq

  def extractColumn(x: Int): Seq[A] = values.transpose.apply(x).toSeq

  def rows: Seq[Seq[A]] = values.map(_.toSeq) 

  def columns: Seq[Seq[A]] = values.transpose.toSeq.map(_.toSeq)

  private def nToXY(n: Int): (Int, Int) = {
    (n % width, Math.floor(n / width).toInt)
  }
  def updated(x: Int, y: Int)(v: A): Grid[A] = 
    Grid(values.updated(y, values(y).updated(x, v)))
  def updated(n: Int)(v: A): Grid[A] = {
    val (x, y) = nToXY(n)
    updated(x, y)(v)
  }
  def updated(p: Vec2i)(v: A): Grid[A] = updated(p.x, p.y)(v)
  def expand(default: A)(n: Int): Grid[A] = {
    Grid[A](values.map[Vector[A]](
      _.prependedAll(Vector.fill[A](n)(default))
       .appendedAll(Vector.fill[A](n)(default))
     )
      .appendedAll(Vector.fill(n, width + n * 2)(default))
      .prependedAll(Vector.fill(n, width + n * 2)(default))
    )
  }
  def expandDir(default: A)(n: Int, dir: Direction2D): Grid[A] = {
    val newData = 
      dir match
        case Direction2D.Up => values.prependedAll(Vector.fill(n, width)(default))
        case Direction2D.Down => values.appendedAll(Vector.fill(n, width)(default))
        case Direction2D.Left => values.map[Vector[A]](_.prependedAll(Vector.fill[A](n)(default)))
        case Direction2D.Right => values.map[Vector[A]](_.appendedAll(Vector.fill[A](n)(default)))    
    Grid(values)
  }
  def valuesAround(default: A)(x: Int, y: Int): Grid[A] = {
    val foo = 
      for {
        y <- (y - 1) to (y + 1)
        x <- (x - 1) to (x + 1)
      } yield get(x, y).getOrElse(default)
    Grid(foo, 3)
  }
  def slice(start: Vec2i, end: Vec2i): Grid[A] = {
    val mxY = start.y `max` end.y 
    val mxX = start.x `max` end.x 
    val mnY = start.y `min` end.y 
    val mnX = start.x `min` end.x 
    Grid(for {
      yy <- mnY to mxY 
    } yield { 
      for {
        xx <- mnX to mxX 
      } yield apply(xx, yy)
    })
  }
  def flatten: Vector[A] = values.flatten

  def indices: Seq[(Int, Int)] = {
    for {
      y <- 0 until height
      x <- 0 until width
    } yield {
      (x, y)
    }
  }

  def zipWithIndices: Seq[(A, Vec2i)] = {
    for {
      y <- 0 until height
      x <- 0 until width
    } yield (this(x, y), Vec2i(x, y))
  }

  
  def mapWithIndex(f: (Vec2i, A) => A): Grid[A] =
    Grid:
      values.zipWithIndex.map: (row, y) =>
        row.zipWithIndex.map: (p, x) => 
          f(Vec2i(x, y), p)
        

}
object Grid {
  def apply[A](values: Iterable[A], width: Int): Grid[A] = {
    assert(values.nonEmpty && values.size % width == 0)
    Grid[A](values.grouped(width).toVector.map(_.toVector))
  }

  def apply[A](values: IterableOnce[IterableOnce[A]]): Grid[A] = {
    Grid[A](values.iterator.to(Vector).map(_.iterator.to(Vector)))
  }
  def fill[A](x: Int, y: Int)(v: => A) = {
    Grid[A](Vector.fill(y, x)(v))
  }
  
  def fromString[A](str: String)(fn: Char => A): Grid[A] = Grid(str.linesIterator.map(_.map(fn)))
}
given gridShow[A](using s: Show[A]): Show[Grid[A]] with {
  def show(t: Grid[A]): String = {
    t.rows.map(it => it.map(s.show).fold("")(_ + " " + _)).fold[String]("")(_ + "\n" + _) 
  }
}

given gridFunctor: Functor[Grid] with {
  def map[A, B](fa: Grid[A])(f: A => B): Grid[B] = {
    Grid(fa.values.map(_.map(f.apply)))
  }
}

given gridFoldable: Foldable[Grid] with {
  override def foldLeft[A, B](fa: Grid[A], b: B)(f: (B, A) => B): B =
    fa.flatten.foldLeft(b)(f)

  override def foldRight[A, B](fa: Grid[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    fa.flatten.foldr[B](lb)(f)
}