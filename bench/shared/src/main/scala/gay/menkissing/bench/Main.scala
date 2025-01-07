package gay.menkissing.bench

import gay.menkissing.advent.*
import gay.menkissing.bench.Bench
import gay.menkissing.bench.Main.benchmark


// JVM can run this too, but Jmh is preferred bc they know what they are doing
object Main extends Bench:
  case class Year(n: Int) extends AnyVal

  def benchmarkFull[A, B, C](day: Int, p: ProblemAdv[A, B, C], unitP1: TimeUnit = TimeUnit.Milliseconds, unitP2: TimeUnit = TimeUnit.Milliseconds)(using year: Year): Unit =
    benchmark(s"day${day}y${year.n}p1", unitP1):
      p.fullPart1
    benchmark(s"day${day}y${year.n}p2", unitP2):
      p.fullPart2
  def benchmarkHalf[A, B](day: Int, p: HalfDay[A, B], unit: TimeUnit = TimeUnit.Milliseconds)(using year: Year): Unit =
    benchmark(s"day${day}y${year.n}p1", unit):
      p.fullPart1


  {
    given Year = Year(2020)

    import y2020.*

    benchmarkFull(1, Day01y2020)
    benchmarkFull(2, Day02y2020)
    benchmarkFull(3, Day03y2020)
    benchmarkFull(4, Day04y2020)
    benchmarkFull(5, Day05y2020)
    benchmarkFull(6, Day06y2020)
    benchmarkFull(7, Day07y2020)
    benchmarkFull(8, Day08y2020)
    benchmarkFull(9, Day09y2020)
    benchmarkHalf(10, Day10y2020)
  }

  {
    given Year = Year(2021)

    import y2021.*

    benchmarkFull(1, Day01y2021)
    benchmarkFull(2, Day02y2021)
    benchmarkFull(3, Day03y2021)
    benchmarkFull(5, Day05y2021)
    benchmarkFull(6, Day6y2021)
    benchmarkFull(7, Day07y2021)
    benchmarkFull(19, Day19y2021)
    benchmarkFull(20, Day20y2021)
    benchmarkFull(21, Day21y2021)

  }

  {
    given Year = Year(2022)

    import y2022.*

    benchmarkFull(1, Day01y2022)
    benchmarkFull(2, Day02y2022)
    benchmarkFull(3, Day03y2022)
    benchmarkFull(4, Day04y2022)
    benchmarkFull(5, Day05y2022)
    benchmarkFull(6, Day06y2022)
    benchmarkFull(7, Day07y2022)
    benchmarkFull(8, Day08y2022)
    benchmarkFull(9, Day09y2022)
    benchmarkFull(10, Day10y2022)
    benchmarkFull(11, Day11y2022)
    benchmarkFull(12, Day12y2022)
    benchmarkFull(13, Day13y2022)
    benchmarkFull(14, Day14y2022)
    benchmarkFull(15, Day15y2022, unitP1 = TimeUnit.Microseconds, unitP2 = TimeUnit.Microseconds)
    benchmarkFull(16, Day16y2022)

    benchmarkHalf(18, Day18y2022)

    benchmarkFull(20, Day20y2022)
    benchmarkFull(21, Day21y2022)
    benchmarkFull(22, Day22y2022)


    benchmarkHalf(25, Day25y2022)
  }

  {
    given Year = Year(2023)

    import y2023.*

    benchmarkFull(1, Day01y2023)
    benchmarkFull(2, Day02y2023)
    benchmarkFull(3, Day03y2023)
    benchmarkFull(4, Day04y2023)
  }

  {
    given Year = Year(2024)

    benchmarkFull(1, Day1)
    benchmarkFull(2, Day2)
    benchmarkFull(3, Day3)
    benchmarkFull(4, Day4)
    benchmarkFull(5, Day5)
    benchmarkFull(6, Day6)
    benchmarkFull(7, Day7)
    benchmarkFull(8, Day8)
    benchmarkFull(9, Day9)
    benchmarkFull(10, Day10)
    benchmarkFull(11, Day11)
    benchmarkFull(12, Day12)
    benchmarkFull(13, Day13)
    benchmarkFull(14, Day14)
    benchmarkFull(15, Day15)
    benchmarkFull(16, Day16)
    benchmarkFull(17, Day17)
    benchmarkFull(18, Day18)
    benchmarkFull(19, Day19)
    benchmarkFull(20, Day20)
    benchmarkFull(21, Day21)
    benchmarkFull(22, Day22)
    benchmarkFull(23, Day23)
    benchmarkFull(24, Day24)
    benchmarkHalf(25, Day25)
  }






