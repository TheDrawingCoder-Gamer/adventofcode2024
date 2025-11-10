package gay.menkissing.advent
package y2022

import gay.menkissing.common.*
import cats.implicits.{catsKernelStdOrderForInt as _, *}
import cats.data.*
import algebra.instances.*
import cats.Show

object Day09 extends Problem:
  type Input = List[Movement]
  type Output = Int

  type Day9State[A] = State[List[Vec2[Int]], A]
  def minDistance(a: Vec2[Int], b: Vec2[Int]) =
    Math.min(Math.abs(a.x - b.x), Math.abs(a.y - b.y))
  def maxDistance(a: Vec2[Int], b: Vec2[Int]) =
    Math.max(Math.abs(a.x - b.x), Math.abs(a.y - b.y))
  def minAxis(a: Vec2[Int], b: Vec2[Int]) =
    List((Math.abs(a.x - b.x), Axis2D.X), (Math.abs(a.y - b.y), Axis2D.Y))
      .maxBy(_._1)._2
  // @annotation.tailrec
  def correctTail(poses: List[Vec2[Int]]): List[Vec2[Int]] =
    poses match
      case head :: (tail :: next) if maxDistance(head, tail) <= 1 =>
        head :: correctTail(poses.tail)
      case head :: (tail :: next) =>
        val newTail =
          val x = head.x.compare(tail.x).sign
          val y = head.y.compare(tail.y).sign
          Vec2(tail.x + x, tail.y + y)
        head :: correctTail(newTail :: next)

      case _ => poses

  def move(dir: Direction2D): Day9State[List[Vec2[Int]]] =
    State:
      case head :: next =>
        val newHead =
          dir match
            case Direction2D.Down  => head.copy(y = head.y - 1)
            case Direction2D.Left  => head.copy(x = head.x - 1)
            case Direction2D.Right => head.copy(x = head.x + 1)
            case Direction2D.Up    => head.copy(y = head.y + 1)

        val r = correctTail(newHead :: next)
        (r, r)
      case _ => !!!

  def moveN(dir: Direction2D, n: Int): Day9State[List[List[Vec2[Int]]]] =
    move(dir).replicateA(n)

  case class Movement(dir: Direction2D, n: Int):
    val execute: Day9State[List[List[Vec2[Int]]]] = moveN(dir, n)

  def parse(input: String): List[Movement] =
    input.linesIterator.map:
      case s"$d $n" =>
        val dir =
          d.head match
            case 'R' => Direction2D.Right
            case 'U' => Direction2D.Up
            case 'D' => Direction2D.Down
            case 'L' => Direction2D.Left
        Movement(dir, n.toInt)
    .toList

  lazy val input = FileIO.getInput(2022, 9)

  def runWithLength(movements: List[Movement], length: Int): Int =
    movements.traverse(_.execute).map(_.flatten).map(_.map(_.tail))
      .runA(List.fill(length)(Vec2(0, 0))).value.map(_.last).toSet.size

  def part1(input: List[Movement]): Int = runWithLength(input, 2)

  def part2(input: List[Movement]): Int = runWithLength(input, 10)
