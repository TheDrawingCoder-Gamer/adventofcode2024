package gay.menkissing.advent
package y2015

import cats.Show

object Day08 extends Problem:
  type Input = List[String]
  type Output = Int

  def parse(str: String): List[String] = str.linesIterator.toList

  def part1(input: List[String]): Int =
    input.map: x =>
      var y = ""
      val xx = x.tail.init
      var i = 0
      while i < xx.length do
        val c = xx(i)
        c match
          case '\\' =>
            i += 1
            val d = xx(i)
            d match
              case '"'  => y += '"'
              case '\\' => y += '\\'
              case 'x'  =>
                i += 1
                val e = xx(i)
                i += 1
                val f = xx(i)
                y += Integer.parseInt(e.toString + f.toString, 16).toChar
          case a => y += a

        i += 1
      x.length - y.length
    .sum

  def part2(input: List[String]): Int =
    input.map: x =>
      val y =
        x.flatMap:
          case '"'  => "\\\""
          case '\\' => "\\\\"
          case x    => x.toString

      // expanded code length - original code length
      (y.length + 2) - x.length
    .sum

  lazy val input: String = FileIO.getInput(2015, 8)
