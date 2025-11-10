package gay.menkissing.advent
package y2015

import scala.util.matching.Regex
import gay.menkissing.common.*
import io.circe.*

object Day12 extends Problem:
  type Input = String
  type Output = Int

  def parse(str: String): String = str.trim

  def part1(input: String): Int =
    val regex = new Regex(raw"\-?\d+")
    regex.findAllIn(input).map(_.toInt).sum

  def part2(input: String): Int =
    val freakyObj = parser.parse(input).rightOrDie
    // this is surprisingly elegant
    def go(n: Json): Int =
      n.fold(
        // no null
        ???,
        // no bool
        _ => ???,
        num => num.toInt.get,
        // ignore strings (red standalone/in array does nothing)
        _ => 0,
        arr => arr.map(go).sum,
        obj =>
          if obj.values.exists(it => it.asString.contains("red")) then 0
          else obj.values.map(go).sum
      )

    go(freakyObj)

  lazy val input: String = FileIO.getInput(2015, 12)
