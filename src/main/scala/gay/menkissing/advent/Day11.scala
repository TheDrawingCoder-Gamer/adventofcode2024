package gay.menkissing.advent

import gay.menkissing.advent.Problem
import gay.menkissing.common.*

import scala.annotation.tailrec
import scala.collection.mutable as mut
import scala.io.Source


object Day11 extends Problem[List[Long], Long]:
  val input = FileIO.getContentsOf("day11.txt").trim

  override def parse(str: String): List[Long] =
    str.split(' ').map(_.toLong).toList


  def testPowOf2(n: Long): Boolean = {
    n != 1 && ((n & (n + 1)) == 0)
  }

  object EvenDigits {
    def unapply(n: Long): Option[(Long, Long)] = {
      val ds = n.digits
      if (ds % 2 == 0) {
        val d2 = ds / 2
        val s = Math.pow(10, d2).toInt
        val high = n / s
        Option((high, n - (high * s)))
      } else None
    }
  }


  def calculateBlink(n: Long): List[Long] = {
    n match {
      case 0 => List(1L)
      case EvenDigits(l, r) => {
        List(l, r)
      }
      case i => List(i * 2024)
    }
  }

  extension (stones: Map[Long, Long]) {
    def diff(key: Long, change: Long): Map[Long, Long] = {
      stones.updatedWith(key) {
        case None => Some(change)
        case Some(n) => {
          val newN = n + change
          Option.when(newN != 0)(newN)
        }
      }
    }
  }

  def calcFreqMap(inStones: Map[Long, Long]): Map[Long, Long] = {
    inStones.foldLeft(inStones) { (stones, stone) =>
      stone match {
        case (0, n) => stones.diff(0, -n).diff(1, n)
        case (o @ EvenDigits(l, r), n) => stones.diff(o, -n).diff(l, n).diff(r, n)
        case (k, n) => stones.diff(k, -n).diff(k * 2024, n)
      }
    }
  }

  override def part1(input: List[Long]): Long =
    val freqMap = input.groupMapReduce(identity)(_ => 1L)(_ + _)
    Iterator.iterate(freqMap)(calcFreqMap).drop(25).next().values.sum


  override def part2(input: List[Long]): Long =
    val freqMap = input.groupMapReduce(identity)(_ => 1L)(_ + _)
    Iterator.iterate(freqMap)(calcFreqMap).drop(75).next().values.sum


