package gay.menkissing.advent
package y2023

import cats.*
import cats.syntax.all.*
import scala.io.Source
import scala.util.matching.Regex
object Day01y2023 extends Problem[Vector[String], Int]:


  lazy val input = FileIO.getInput(2023, 1)

  def parse(input: String): Vector[String] =
    input.linesIterator.toVector

  def part1(input: Vector[String]): Int =
    input.map: line =>
      (line.find(_.isDigit).get.toString + line.findLast(_.isDigit).get.toString).toInt
    .sum

  def nameToInt(name: String) =
    name match {
      case "one" => 1
      case "two" => 2
      case "three" => 3
      case "four" => 4
      case "five" => 5
      case "six" => 6
      case "seven" => 7
      case "eight" => 8
      case "nine" => 9
      case _ => ???
    }
  // sinful
  extension (regex: Regex)
    def findLastMatchIn(line: String) = {
      // :troll:
      (1 to line.length).collectFirst({ (n: Int) =>
        regex.findFirstMatchIn(line.takeRight(n))
      }.unlift)
    }
  val regex = "(one|two|three|four|five|six|seven|eight|nine|[1-9])".r

  def part2(input: Vector[String]): Int =
    input.map: line =>
      val first = regex.findFirstMatchIn(line).get.matched
      val last = regex.findLastMatchIn(line).get.matched

      val tens = first.toIntOption.getOrElse(nameToInt(first)) * 10
      val ones = last.toIntOption.getOrElse(nameToInt(last))

      tens + ones
    .sum


