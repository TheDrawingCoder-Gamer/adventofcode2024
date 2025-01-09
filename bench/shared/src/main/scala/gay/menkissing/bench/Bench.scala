package gay.menkissing.bench


import gay.menkissing.bench.*
import gay.menkissing.bench.Blackhole.*

import java.util.concurrent.TimeUnit
import scala.collection.mutable as mut
import scala.concurrent.duration.{Duration, NANOSECONDS}
import util.*


def nanoTimed[U](a: => U): Double =
  val start = System.nanoTime()
  a
  val end = System.nanoTime()
  (end - start).toDouble


case class BenchmarkRunOpts(timeout: Option[Duration],
                            verbosity: Verbosity
                           )

trait Bench:


  val warmup: Int = 3
  val measurement: Int = 3
  val unit: TimeUnit = TimeUnit.MILLISECONDS


  case class Benchmark(name: String, body: Blackhole.Impl => Unit, opts: BenchmarkOptions):
    def run(verbosity: Verbosity): Vector[Double] =
      if (verbosity.ordinal >= Verbosity.Normal.ordinal)
        println(s"benchmarking $name...")
      val hole = Blackhole.obtainBlackhole()

      val times = mut.ListBuffer[Double]()
      // JavaScript gets upset here if I break this into two loops so i dont
      (1 to opts.warmup + opts.measurement).foreach: n =>
        val time = nanoTimed(body(hole))
        hole.teardown()
        if n > opts.warmup then
          times.append(time)

        if (verbosity.ordinal >= Verbosity.Verbose.ordinal)
          val displayN = if n > opts.warmup then n - opts.warmup else n
          println(f"${if n > opts.warmup then "iteration" else "warmup"} $displayN: ${Duration(time, NANOSECONDS).toUnit(opts.unit)}%1.3f ${opts.unit.display}")
        Gc.gc()

      times.toVector

  case class BenchmarkOptions(
                               unit: TimeUnit = unit,
                               warmup: Int = warmup,
                               measurement: Int = measurement
                             )

  private val benchmarks = mut.ListBuffer[Benchmark]()

  lazy val benchmarkMap: Map[String, Benchmark] =
    benchmarks.map(it => (it.name, it)).toMap

  final def benchmark[U](name: String, options: BenchmarkOptions = BenchmarkOptions())(body: => U): Unit =
    benchmarks.append(Benchmark(name, hole => {
      hole.consumed(body)
    }, options))


  def main(badArgs: Array[String]): Unit =
    val args = CLIArgs.parse(Args.args(badArgs))
    val daBenches =
      if args.patterns.nonEmpty then
        benchmarks.filter(it => args.patterns.exists(_.matches(it._1))).toVector
      else
        benchmarks.toVector


    val benches = daBenches.map { it =>
      val samples = spawn.Spawn.run(it.name, BenchmarkRunOpts(args.timeout, args.verbosity))
      samples.map { samples =>
        val result = IterationResult(it.name, ListStatistics(samples), it.opts.unit)
        if args.verbosity.ordinal >= Verbosity.Normal.ordinal then
          println(result.fullResult)
        result
      }.toRight {
        println("timed out")
        IterationFailure(it.name, "Timed Out")
      }

    }

    println("results: ")
    // benches.foreach(it => println(s"${it.name}: ${it.hocon}"))
    benches.foreach:
      case Left(value) =>
        println(s"${value.name} failed: ${value.why}")
      case Right(value) =>
        println(s"${value.name}: ${value.pretty}; ${value.hocon}")


        



