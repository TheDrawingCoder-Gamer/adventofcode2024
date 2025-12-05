package gay.menkissing.bench

import gay.menkissing.advent.*
import gay.menkissing.bench.Bench

import java.util.concurrent.TimeUnit
import gay.menkissing.bench.Blackhole.consumed
import gay.menkissing.bench.Blackhole.Impl

// JVM can run this too, but Jmh is preferred bc they know what they are doing
object Main extends Bench:
  case class Year(n: Int) extends AnyVal

  // This special casing doesn't particularly matter for JVM and JS, but on
  // native we don't spawn a new process for each benchmark, so
  // it's important to try and free as much memory as possible.
  class Part1BenchmarkBody(argObj: => IncompleteProblem) extends BenchmarkBody:
    lazy val obj = argObj

    private var input: String = null

    override def load(): Unit = input = obj.input

    def run(blackhole: Blackhole.Impl): Unit =
      blackhole.consumed(obj.runPart1(input))

    override def free(): Unit = input = null

  class Part2BenchmarkBody(argObj: => ProblemSuperAdv) extends BenchmarkBody:
    lazy val obj = argObj

    private var input: String = null

    override def load(): Unit = input = obj.input

    def run(blackhole: Impl): Unit = blackhole.consumed(obj.runPart2(input))

    override def free(): Unit = input = null

  enum FullOpts:
    case Separate(p1: BenchmarkOptions, p2: BenchmarkOptions)
    case Both(opts: BenchmarkOptions)

    def part1: BenchmarkOptions =
      this match
        case Both(opts)      => opts
        case Separate(p1, _) => p1

    def part2: BenchmarkOptions =
      this match
        case Both(opts)      => opts
        case Separate(_, p2) => p2

  object FullOpts:
    def part1Only(opts: BenchmarkOptions) =
      FullOpts.Separate(opts, BenchmarkOptions())

    def part2Only(opts: BenchmarkOptions) =
      FullOpts.Separate(BenchmarkOptions(), opts)

  def benchmarkFull
    (
      day: Int,
      // delay loading the object
      p: => ProblemSuperAdv,
      opts: FullOpts = FullOpts.Both(BenchmarkOptions())
    )
    (using year: Year): Unit =
    benchmarkAdvanced(s"day${day}y${year.n}p1", opts.part1)(
      Part1BenchmarkBody(p)
    )
    benchmarkAdvanced(s"day${day}y${year.n}p2", opts.part2)(
      Part2BenchmarkBody(p)
    )
  def benchmarkHalf
    (
      day: Int,
      p: => IncompleteProblem,
      opts: BenchmarkOptions = BenchmarkOptions()
    )
    (using year: Year): Unit =
    benchmarkAdvanced(s"day${day}y${year.n}p1", opts)(Part1BenchmarkBody(p))

  locally:
    given Year = Year(2015)

    import y2015.*

    benchmarkFull(1, Day01)
    benchmarkFull(2, Day02)
    benchmarkFull(3, Day03)
    benchmarkFull(4, Day04)
    benchmarkFull(5, Day05)
    benchmarkFull(6, Day06)
    benchmarkFull(7, Day07)
    benchmarkFull(8, Day08)
    benchmarkFull(9, Day09)
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

  locally:
    given Year = Year(2020)

    import y2020.*

    benchmarkFull(1, Day01)
    benchmarkFull(2, Day02)
    benchmarkFull(3, Day03)
    benchmarkFull(4, Day04)
    benchmarkFull(5, Day05)
    benchmarkFull(6, Day06)
    benchmarkFull(7, Day07)
    benchmarkFull(8, Day08)
    benchmarkFull(9, Day09)
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

  locally:
    given Year = Year(2021)

    import y2021.*

    benchmarkFull(1, Day01)
    benchmarkFull(2, Day02)
    benchmarkFull(3, Day03)
    benchmarkFull(4, Day04)
    benchmarkFull(5, Day05)
    benchmarkFull(6, Day06)
    benchmarkFull(7, Day07)
    benchmarkFull(8, Day08)
    benchmarkFull(9, Day09)
    benchmarkFull(10, Day10)
    benchmarkFull(11, Day11)
    benchmarkFull(12, Day12)
    benchmarkFull(13, Day13)
    benchmarkFull(14, Day14)
    benchmarkFull(15, Day15)
    benchmarkFull(16, Day16)
    benchmarkFull(17, Day17)
    benchmarkFull(18, Day18)
    benchmarkFull(
      19,
      Day19,
      FullOpts.Both(BenchmarkOptions(excludePlatforms = List(PlatformKind.JS)))
    )
    benchmarkFull(20, Day20)
    benchmarkFull(
      21,
      Day21,
      // do not test me child, only woe comes from reenabling this
      FullOpts.part2Only(
        BenchmarkOptions(excludePlatforms = List(PlatformKind.Native))
      )
    )
    benchmarkFull(22, Day22)
    benchmarkFull(23, Day23)
    benchmarkFull(24, Day24)
    benchmarkHalf(25, Day25)

  locally:
    given Year = Year(2022)

    import y2022.*

    benchmarkFull(1, Day01)
    benchmarkFull(2, Day02)
    benchmarkFull(3, Day03)
    benchmarkFull(4, Day04)
    benchmarkFull(5, Day05)
    benchmarkFull(6, Day06)
    benchmarkFull(7, Day07)
    benchmarkFull(8, Day08)
    benchmarkFull(9, Day09)
    benchmarkFull(10, Day10)
    benchmarkFull(11, Day11)
    benchmarkFull(12, Day12)
    benchmarkFull(13, Day13)
    benchmarkFull(14, Day14)
    benchmarkFull(
      15,
      Day15,
      FullOpts.part1Only(BenchmarkOptions(unit = TimeUnit.MICROSECONDS))
    )
    benchmarkFull(16, Day16)
    benchmarkFull(17, Day17)
    benchmarkFull(18, Day18)
    benchmarkFull(19, Day19)
    benchmarkFull(20, Day20)
    benchmarkFull(21, Day21)
    benchmarkFull(22, Day22)
    benchmarkFull(
      23,
      Day23,
      FullOpts.part2Only(
        BenchmarkOptions(excludePlatforms =
          List(PlatformKind.Native, PlatformKind.JS)
        )
      )
    )
    benchmarkFull(24, Day24)
    benchmarkHalf(25, Day25)

  locally:
    given Year = Year(2023)

    import y2023.*

    benchmarkFull(1, Day01)
    benchmarkFull(2, Day02)
    benchmarkFull(3, Day03)
    benchmarkFull(4, Day04)
    benchmarkFull(5, Day05)
    benchmarkFull(6, Day06)
    benchmarkFull(7, Day07)
    benchmarkFull(8, Day08)
    benchmarkFull(9, Day09)
    benchmarkFull(10, Day10)
    benchmarkFull(11, Day11)
    benchmarkFull(12, Day12)

  locally:
    given Year = Year(2024)

    import y2024.*

    benchmarkFull(1, Day01)
    benchmarkFull(2, Day02)
    benchmarkFull(3, Day03)
    benchmarkFull(4, Day04)
    benchmarkFull(5, Day05)
    benchmarkFull(6, Day06)
    benchmarkFull(7, Day07)
    benchmarkFull(8, Day08)
    benchmarkFull(9, Day09)
    benchmarkFull(10, Day10)
    benchmarkFull(11, Day11)
    benchmarkFull(12, Day12)
    benchmarkFull(13, Day13)
    benchmarkFull(14, Day14)
    benchmarkFull(15, Day15)
    benchmarkFull(16, Day16)
    benchmarkFull(
      17,
      Day17,
      FullOpts.Both(BenchmarkOptions(unit = TimeUnit.MICROSECONDS))
    )
    benchmarkFull(18, Day18)
    benchmarkFull(19, Day19)
    benchmarkFull(20, Day20)
    benchmarkFull(21, Day21)
    benchmarkFull(22, Day22)
    benchmarkFull(23, Day23)
    benchmarkFull(24, Day24)
    benchmarkHalf(25, Day25)

  locally:
    given Year = Year(2025)

    import y2025.*

    benchmarkFull(1, Day01)
    benchmarkFull(2, Day02)
    benchmarkFull(3, Day03)
    benchmarkFull(4, Day04)
    benchmarkFull(5, Day05)
