package gay.menkissing.advent
package bench

import gay.menkissing.advent.y2020.*
import gay.menkissing.advent.y2021.*
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(1)
@Warmup(iterations = 3)
@Measurement(iterations = 3)
class Bench:
  @Benchmark
  def day01p1(): Unit =
    Day1.fullPart1
  @Benchmark
  def day01p2(): Unit =
    Day1.fullPart2
  @Benchmark
  def day02p1(): Unit =
    Day2.fullPart1
  @Benchmark
  def day02p2(): Unit =
    Day2.fullPart2
  @Benchmark
  def day03p1(): Unit = Day3.fullPart1
  @Benchmark
  def day03p2(): Unit = Day3.fullPart2
  @Benchmark
  def day04p1(): Unit = Day4.fullPart1
  @Benchmark
  def day04p2(): Unit = Day4.fullPart2

  @Benchmark
  def day05p1(): Unit = Day5.fullPart1

  @Benchmark
  def day05p2(): Unit = Day5.fullPart2

  @Benchmark
  def day06p1(): Unit = Day6.fullPart1

  @Benchmark
  def day06p2(): Unit = Day6.fullPart2

  @Benchmark
  def day07p1(): Unit = Day7.fullPart1

  @Benchmark
  def day07p2(): Unit = Day7.fullPart2

  @Benchmark
  def day08p1(): Unit = Day8.fullPart1

  @Benchmark
  def day08p2(): Unit = Day8.fullPart2

  @Benchmark
  def day09p1(): Unit = Day9.fullPart1

  @Benchmark
  def day09p2(): Unit = Day9.fullPart2

  @Benchmark
  def day10p1(): Unit = Day10.fullPart1

  @Benchmark
  def day10p2(): Unit = Day10.fullPart2

  @Benchmark
  def day11p1(): Unit = Day11.fullPart1

  @Benchmark
  def day11p2(): Unit = Day11.fullPart2

  @Benchmark
  def day12p1(): Unit = Day12.fullPart1

  @Benchmark
  def day12p2(): Unit = Day12.fullPart2

  @Benchmark
  def day13p1(): Unit = Day13.fullPart1

  @Benchmark
  def day13p2(): Unit = Day13.fullPart2

  @Benchmark
  def day14p1(): Unit = Day14.fullPart1

  @Benchmark
  def day14p2(): Unit = Day14.fullPart2

  @Benchmark
  def day15p1(): Unit = Day15.fullPart1

  @Benchmark
  def day15p2(): Unit = Day15.fullPart2

  @Benchmark
  def day16p1(): Unit = Day16.fullPart1

  @Benchmark
  def day16p2(): Unit = Day16.fullPart2

  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  @Benchmark
  def day17p1(): Unit = Day17.fullPart1

  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  @Benchmark
  def day17p2(): Unit = Day17.fullPart2


  @Benchmark
  def day18p1(): Unit = Day18.fullPart1

  @Benchmark
  def day18p2(): Unit = Day18.fullPart2

  @Benchmark
  def day19p1(): Unit = Day19.fullPart1

  @Benchmark
  def day19p2(): Unit = Day19.fullPart2

  @Benchmark
  def day20p1(): Unit = Day20.fullPart1

  @Benchmark
  def day20p2(): Unit = Day20.fullPart2

  @Benchmark
  def day21p1(): Unit = Day21.fullPart1

  @Benchmark
  def day21p2(): Unit = Day21.fullPart2

  @Benchmark
  def day22p1(): Unit = Day22.fullPart1

  @Benchmark
  def day22p2(): Unit = Day22.fullPart2


  @Benchmark
  def day23p1(): Unit =
    Day23.fullPart1
  @Benchmark
  def day23p2(): Unit =
    Day23.fullPart2

  @Benchmark
  def day24p1(): Unit = Day24.fullPart1

  @Benchmark
  def day24p2(): Unit = Day24.fullPart2

  @Benchmark
  def day25p1(): Unit = Day25.fullPart1

  @Benchmark
  def day03y2020p1(): Unit = Day03y2020.fullPart1

  @Benchmark
  def day03y2020p2(): Unit = Day03y2020.fullPart2

  @Benchmark
  def day19y2021p1(): Unit = Day19y2021.fullPart1

  @Benchmark
  def day19y2021p2(): Unit = Day19y2021.fullPart2

  @Benchmark
  def day21y2021p1(): Unit = Day21y2021.fullPart1

  @Benchmark
  def day21y2021p2(): Unit = Day21y2021.fullPart2

