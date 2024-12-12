import scala.io.Source
import gay.menkissing.common.*

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ParSeq
import scala.collection.mutable as mut

val data = Source.fromResource("day11.txt").mkString.trim.split(' ').map(_.toLong).toList



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






// optimization observations:
// 0 => 1 => 2024 => 20 24 => 2 0 2 4 => 4048 1 4048 8096
// 2 => 4048 => 40 48 => 4 0 4 8
// minimum number of digits increase: 3
// for any binary number with trailing zeros, its even
// number of trailing zeros is the number of times it can be divided by 2 while staying even
// albiet this isn't too useful on the lower half on numbers, the higher half is useful


// If we do in steps of 5 we can optimize zeros with hardcoding

// fullCalc5Step.repeated(75 / 5)(data).size


val freqMap = data.groupMapReduce(identity)(_ => 1L)(_ + _)
// i gave up and copied jamie's solution for p2. im pissed i keep forgetting about freq maps
Iterator.iterate(freqMap)(calcFreqMap).drop(75).next.values.sum