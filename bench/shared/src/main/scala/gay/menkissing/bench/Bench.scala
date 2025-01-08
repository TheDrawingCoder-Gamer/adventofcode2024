package gay.menkissing.bench


import gay.menkissing.bench.*
import gay.menkissing.bench.Blackhole.*

import scala.collection.mutable as mut



def nanoTimed[U](a: => U): Double =
  val start = System.nanoTime()
  a
  val end = System.nanoTime()
  (end - start).toDouble


enum Verbosity:
  case Quiet, Normal, Verbose

trait Bench:


  val warmup: Int = 3
  val measurement: Int = 3
  val unit: TimeUnit = TimeUnit.Milliseconds


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
          println(f"${if n > opts.warmup then "iteration" else "warmup"} $displayN: ${TimeUnit.Nanoseconds.convertTo(time, opts.unit)}%1.3f ${opts.unit.display}")
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
    val args = Args.args(badArgs)
    val goodArgs = args.filter(!_.startsWith("--"))
    val daBenches =
      if goodArgs.nonEmpty then
        val reg = goodArgs(0).r
        benchmarks.filter(it => reg.matches(it._1)).toVector
      else
        benchmarks.toVector

    val quiet = args.contains("--quiet") && !args.contains("--no-quiet")
    val verbose = args.contains("--verbose")

    val verbosity =
      if verbose then
        Verbosity.Verbose
      else if quiet then
        Verbosity.Quiet
      else
        Verbosity.Normal
    val benches = daBenches.map { it =>
      val samples = spawn.Spawn.run(it.name, verbosity)
      val result = IterationResult(it.name, ListStatistics(samples), it.opts.unit)
      println(result.fullResult)
      result
    }

    println("results: ")
    // benches.foreach(it => println(s"${it.name}: ${it.hocon}"))
    benches.foreach(it => println(s"${it.pretty}; ${it.hocon}"))


        



