package gay.menkissing.advent
package y2020

import cats.*
import gay.menkissing.common.*

object Day22 extends Problem:
  type Input = (List[Int], List[Int])
  type Output = Int

  lazy val input = FileIO.getInput(2020, 22)

  val debugger = Debuginator(active = true)

  def parse(str: String): Input =
    val Array(p1, p2) = str.trim.split("\n\n")
    val p1s = p1.linesIterator.toList.tail.map(_.toInt)
    val p2s = p2.linesIterator.toList.tail.map(_.toInt)
    (p1s, p2s)

  def stepP2
    (p1: List[Int], p2: List[Int]): Eval[
    (List[Int], List[Int])
  ] =
    val p1v :: p1ls = p1.runtimeChecked
    val p2v :: p2ls = p2.runtimeChecked
    if p1ls.lengthIs >= p1v && p2ls.lengthIs >= p2v then
      // recurse
      subGame(p1ls.take(p1v), p2ls.take(p2v)).map: (p2Won, _) =>
        if p2Won then (p1ls, p2ls ++ List(p2v, p1v))
        else (p1ls ++ List(p1v, p2v), p2ls)
    else
      // business as usual
      if p1v < p2v then Eval.now((p1ls, p2ls ++ List(p2v, p1v)))
      else
        debugger.assert(p1v > p2v)
        Eval.now((p1ls ++ List(p1v, p2v), p2ls))

  def fullRound(p1: List[Int], p2: List[Int]): (List[Int], List[Int]) =
    val (remP1, remP2) =
      p1.lengthCompare(p2) match
        case x if x < 0 => (Nil, p2.drop(p1.length))
        case x if x > 0 => (p1.drop(p2.length), Nil)
        case _          => (Nil, Nil)
    val (newP1, newP2) =
      p1.zip(p2).foldLeft((List.empty[Int], List.empty[Int])):
        case ((l, r), (p1v, p2v)) =>
          if p1v < p2v then (l, p1v :: p2v :: r)
          else
            debugger.assert(p1v > p2v)
            (p2v :: p1v :: l, r)
    (remP1 ++ newP1.reverse, remP2 ++ newP2.reverse)

  def subGame
    (p1: List[Int], p2: List[Int]): Eval[
    (Boolean, List[Int])
  ] =
    MonadPApp[Eval].tailRecM(p1, p2, Set.empty[(List[Int], List[Int])]):
      (p1, p2, seen) =>
        if seen((p1, p2)) then Eval.now(Right(false, p1))
        else
          stepP2(p1, p2).map: (np1, np2) =>
            if np1.isEmpty then Right((true, np2))
            else if np2.isEmpty then Right((false, np1))
            else Left((np1, np2, seen.+((p1, p2))))

  def score(ls: List[Int]): Int =
    ls.reverse.zip(Iterator.from(1)).map(_ * _).sum

  def part1(input: (List[Int], List[Int])): OutputP1 =
    val (p1, p2) = input
    if p1.lengthCompare(p2) != 0 then
      // if this is false then we can end up recursing forever
      // this is only false for our part 2 test case anyway
      return 0

    unfoldedMap((p1, p2)): (p1, p2) =>
      val (newP1, newP2) = fullRound(p1, p2)
      if newP1.isEmpty then Left(score(newP2))
      else if newP2.isEmpty then Left(score(newP1))
      else Right((newP1, newP2))

  def part2(input: (List[Int], List[Int])): OutputP2 =
    val (p1, p2) = input
    val (_, winner) = subGame(p1, p2).value

    score(winner)
