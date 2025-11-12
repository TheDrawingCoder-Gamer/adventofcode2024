package gay.menkissing.advent
package y2021

import cats.implicits.*
import cats.*
import gay.menkissing.common.ArityN.*
import cats.data.NonEmptyList
import annotation.tailrec

object Day18 extends Problem:
  type Input = NonEmptyList[SnailfishNum]
  type Output = Long

  enum SnailfishNum:
    case SGroup(l: SnailfishNum, r: SnailfishNum)
    case SLiteral(lit: Int)

    infix def +(that: SnailfishNum): SnailfishNum = SGroup(this, that).reduce

    def explodeOnce: (SnailfishNum, Boolean) =
      def add(cur: SnailfishNum, vl: Int, vr: Int): SnailfishNum =
        cur match
          case SGroup(l, r)  => SGroup(add(l, vl, 0), add(r, 0, vr))
          case SLiteral(lit) => SLiteral(lit + vl + vr)
      def go
        (cur: SnailfishNum, nest: Int, done: Boolean): (
        SnailfishNum,
        Int,
        Int,
        Boolean
      ) =
        cur match
          case SGroup(SLiteral(l), SLiteral(r)) if nest >= 4 && !done =>
            (SLiteral(0), l, r, true)
          case SGroup(l, r) =>
            val (ln, ll, lr, doneL) = go(l, nest + 1, done)
            val (rn, rl, rr, doneR) = go(r, nest + 1, doneL)
            (SGroup(add(ln, 0, rl), add(rn, lr, 0)), ll, rr, doneR)
          case s @ SLiteral(_) => (s, 0, 0, done)

      val (n, _, _, done) = go(this, 0, false)
      (n, done)

    def splitOnce: (SnailfishNum, Boolean) =
      def go(cur: SnailfishNum, done: Boolean): (SnailfishNum, Boolean) =
        cur match
          case SGroup(l, r) =>
            val (ln, doneL) = go(l, done)
            val (rn, doneR) = go(r, doneL)
            (SGroup(ln, rn), doneR)
          case SLiteral(lit) =>
            if lit >= 10 && !done then
              val v = lit / 2
              (SGroup(SLiteral(v), SLiteral(lit - v)), true)
            else (cur, done)

      go(this, false)

    @tailrec
    final def reduce: SnailfishNum =
      val (exploded, didExplode) = this.explodeOnce
      if didExplode then exploded.reduce
      else
        val (split, didSplit) = this.splitOnce
        if didSplit then split.reduce
        else this

    def magnitude: Int =
      this match
        case SGroup(l, r)  => l.magnitude * 3 + r.magnitude * 2
        case SLiteral(lit) => lit

  object SnailfishNum:
    import parsley.*
    import parsley.character.char

    lazy val groupParser: Parsley[SnailfishNum] =
      char('[') *> ((parser <* char(',')) <~> parser)
        .map((l, r) => SGroup(l, r)) <* char(']')

    lazy val literalParser: Parsley[SnailfishNum] =
      parsley.character.digit.map(it => SLiteral(it.asDigit))

    lazy val parser: Parsley[SnailfishNum] = groupParser <|> literalParser

    given Show[SnailfishNum] with
      def show(t: SnailfishNum): String =
        t match
          case SGroup(l, r)  => s"[${show(l)},${show(r)}]"
          case SLiteral(lit) => lit.toString

    given Semigroup[SnailfishNum] = _ + _

    def parse(str: String): SnailfishNum = parser.parse(str).get

  override def parse(str: String): NonEmptyList[SnailfishNum] =
    NonEmptyList
      .fromListUnsafe(str.linesIterator.map(SnailfishNum.parse).toList)

  override def part1(input: NonEmptyList[SnailfishNum]): Long =
    val res = input.reduce
    println(res.show)
    res.magnitude

  override def part2(input: NonEmptyList[SnailfishNum]): Long =
    input.combinationsN[2].flatMap: (l, r) =>
      List(l + r, r + l)
    .map(_.magnitude).max
  override lazy val input: String = FileIO.getInput(2021, 18)
