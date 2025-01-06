package gay.menkissing.bench


import gay.menkissing.bench.*
import gay.menkissing.bench.Blackhole.*

import scala.collection.mutable as mut




def nanoTimed[U](a: => U): Double =
  val start = System.nanoTime()
  a
  val end = System.nanoTime()
  (end - start).toDouble

case class Benchmark(name: String, body: Blackhole.Impl => Unit, unit: TimeUnit):
  def run(plan: IterationPlan, quiet: Boolean = false): IterationResult =
    if (!quiet)
      println(s"benchmarking $name...")
    val hole = Blackhole.obtainBlackhole()

    (1 to plan.warmup).foreach: n =>
      val time = nanoTimed(body(hole))
      hole.teardown()
      if (!quiet)
        println(f"warmup $n: ${TimeUnit.Nanoseconds.convertTo(time, unit)}%1.3f ${unit.display}")
      Gc.gc()
    val times = mut.ListBuffer[Double]()
    (1 to plan.measurement).foreach: n =>
      val time = nanoTimed(body(hole))
      hole.teardown()
      if (!quiet)
        println(f"iteration $n: ${TimeUnit.Nanoseconds.convertTo(time, unit)}%1.3f ${unit.display}")
      times.append(time)
      Gc.gc()
    val stats = ListStatistics(times.toVector)
    val r = IterationResult(name, stats.mean, stats.standardErrorOfTheMean, unit)
    if (!quiet)
      println(r.pretty)
    r

trait Bench:
  val warmup: Int = 3
  val measurement: Int = 3
  val unit: TimeUnit = TimeUnit.Milliseconds

  private val benchmarks = mut.ListBuffer[Benchmark]()

  lazy val benchmarkMap: Map[String, Benchmark] =
    benchmarks.map(it => (it.name, it)).toMap

  final def benchmark[U](name: String, benchUnit: TimeUnit = unit)(body: => U): Unit =
    benchmarks.append(Benchmark(name, hole => {
      hole.consumed(body)
    }, benchUnit))


  def main(badArgs: Array[String]): Unit =
    val args = Args.args(badArgs)
    val goodArgs = args.filter(!_.startsWith("--"))
    val daBenches =
      if goodArgs.nonEmpty then
        val reg = goodArgs(0).r
        benchmarks.filter(it => reg.matches(it._1)).toVector
      else
        benchmarks.toVector

    val quiet = args.contains("--quiet")

    val hole = Blackhole.obtainBlackhole()

    val benches = daBenches.map(it => spawn.Spawn.run(IterationPlan(warmup, measurement), it.name, quiet))

    println("results: ")
    // benches.foreach(it => println(s"${it.name}: ${it.hocon}"))
    benches.foreach(it => println(s"${it.pretty}; ${it.hocon}"))


        



