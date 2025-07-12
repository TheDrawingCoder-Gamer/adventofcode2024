package gay.menkissing.advent
package y2022

import scala.annotation.tailrec

object Day25 extends HalfDay[List[Long], String] {
  def snafuToDec(input: String): Long = {
    @tailrec
    def helper(input: List[Char], accum: Long): Long = {
      input match {
        case c :: tail => 
          val part = c match {
            case '1' => 1
            case '0' => 0
            case '2' => 2 
            case '-' => -1
            case '=' => -2 
          }
          helper(tail, (accum * 5) + part) 
        case Nil => accum   
      }    
    }
    helper(input.toList, 0)
  }
  def digitStream: LazyList[Int] = {
    // 1, 2, 5, 10, 25, 50 
    LazyList.iterate(1) { it => 
      if ((it % 2) == 0) 
        (it / 2) * 5
      else 
        it * 2
    }
  }
  def toSnigit(digit: Int): Char = {
    digit match {
      case -2 => '='
      case -1 => '-'
      case 0 => '0'
      case 1 => '1'
      case 2 => '2'
      case _ => assert(false)
    }
  }

  def decToSnafu(n: Long): String = {
    val sunits = Iterator.unfold(n) { n => 
      Option.when(n != 0) {
        val s = math.floorMod(n + 2, 5).toInt - 2
        toSnigit(s) -> (n - s) / 5 
      }
    }
    if (sunits.isEmpty) "0"
    else sunits.mkString.reverse
  }
  lazy val input = FileIO.getInput(2022, 25)
  def parse(input: String): List[Long] = {
    input.linesIterator.map(snafuToDec).toList
  }
  def part1(input: List[Long]): String = {
    decToSnafu(input.sum)
  }
}
