package gay.menkissing.bench

import gay.menkissing.bench.*
import gay.menkissing.bench.Blackhole.*

import java.util.concurrent.TimeUnit
import scala.collection.mutable as mut
import scala.concurrent.duration.{Duration, NANOSECONDS}
import scala.concurrent.duration.*
import util.*

def nanoTimed[U](a: => U): Double =
  val start = System.nanoTime()
  a
  val end = System.nanoTime()
  (end - start).toDouble

case class BenchmarkRunOpts(timeout: Option[Duration], verbosity: Verbosity)

trait Bench:

  val warmup: Int = 3
  val measurement: Int = 3
  val unit: TimeUnit = TimeUnit.MILLISECONDS

  case class Benchmark
    (name: String, body: Blackhole.Impl => Unit, opts: BenchmarkOptions):
    def run(verbosity: Verbosity): Vector[Double] =
      if verbosity.ordinal >= Verbosity.Normal.ordinal then
        println(s"benchmarking $name...")
      val hole = Blackhole.obtainBlackhole()

      val times = mut.ListBuffer[Double]()

      val warmupStartTime = System.nanoTime()

      (1 to opts.warmup).foreach: n =>
        val time = nanoTimed(body(hole))
        hole.teardown()
        if verbosity.ordinal >= Verbosity.Verbose.ordinal then
          println(f"warmup $n: ${Duration(time, NANOSECONDS)
              .toUnit(opts.unit)}%1.3f ${opts.unit.display}")

        Gc.gc()
      val warmupEndTime = System.nanoTime()
      val warmupDuration = (warmupEndTime - warmupStartTime).nanos
      if warmupDuration < opts.realWarmupTime then
        if verbosity.ordinal >= Verbosity.Verbose.ordinal then
          println("warmups were fast, testing more to get more samples")
        val targetTime =
          warmupEndTime + (opts.realWarmupTime - warmupDuration).toNanos
        var n = opts.warmup + 1
        while System.nanoTime() < targetTime do
          val time = nanoTimed(body(hole))
          hole.teardown()
          if verbosity.ordinal >= Verbosity.Verbose.ordinal then
            println(f"warmup $n: ${Duration(time, NANOSECONDS)
                .toUnit(opts.unit)}%1.3f ${opts.unit.display}")
          n += 1
          Gc.gc()

      val measureStartTime = System.nanoTime()
      (1 to opts.measurement).foreach: n =>
        val time = nanoTimed(body(hole))
        hole.teardown()

        times.append(time)

        if verbosity.ordinal >= Verbosity.Verbose.ordinal then
          println(f"iteration $n: ${Duration(time, NANOSECONDS)
              .toUnit(opts.unit)}%1.3f ${opts.unit.display}")
        Gc.gc()

      val measureEndTime = System.nanoTime()
      val measureDuration = (measureEndTime - measureStartTime).nanos
      if measureDuration < opts.minMeasurementTime then
        if verbosity.ordinal >= Verbosity.Verbose.ordinal then
          println("iterations were fast, testing more to get more samples")
        val targetTime =
          measureEndTime + (opts.realWarmupTime - measureDuration).toNanos
        var n = opts.measurement + 1
        while System.nanoTime() < targetTime do
          val time = nanoTimed(body(hole))
          hole.teardown()

          times.append(time)

          if verbosity.ordinal >= Verbosity.Verbose.ordinal then
            println(f"iteration $n: ${Duration(time, NANOSECONDS)
                .toUnit(opts.unit)}%1.3f ${opts.unit.display}")
          n += 1
          Gc.gc()

      times.toVector

  case class BenchmarkOptions
    (
      unit: TimeUnit = unit,
      warmup: Int = warmup,
      measurement: Int = measurement,
      minMeasurementTime: Duration = 5.seconds,
      minWarmupTime: Option[Duration] = None,
      excludePlatforms: List[PlatformKind] = List()
    ):
    val realWarmupTime: Duration = minWarmupTime.getOrElse(minMeasurementTime)

  private val benchmarks = mut.ListBuffer[Benchmark]()

  lazy val benchmarkMap: Map[String, Benchmark] =
    benchmarks.map(it => (it.name, it)).toMap

  final def benchmark[U]
    (name: String, options: BenchmarkOptions = BenchmarkOptions())
    (body: => U): Unit =
    benchmarks.append(Benchmark(name, hole => hole.consumed(body), options))

  def main(badArgs: Array[String]): Unit =
    val args = CLIArgs.parse(Args.args(badArgs))
    def patternMatches(name: String): Boolean =
      if args.patterns.nonEmpty then args.patterns.exists(_.matches(name))
      else true
    val daBenches =
      benchmarks.filter(it =>
        patternMatches(it._1) && args.excludedPatterns.forall(!_.matches(it._1))
      ).toVector

    val benches =
      daBenches.map: it =>
        if it.opts.excludePlatforms.contains(Platform.current) then
          Left:
            println(s"skipping ${it.name} for current platform")
            IterationFailure(it.name, "Skipped for current platform")
        else
          val samples =
            spawn.Spawn
              .run(it.name, BenchmarkRunOpts(args.timeout, args.verbosity))
          samples.map: samples =>
            val result =
              IterationResult(it.name, ListStatistics(samples), it.opts.unit)
            if args.verbosity.ordinal >= Verbosity.Normal.ordinal then
              println(result.fullResult)
            result
          .toRight:
            println("timed out")
            IterationFailure(it.name, "Timed Out")

    println("results: ")
    // benches.foreach(it => println(s"${it.name}: ${it.hocon}"))
    benches.foreach:
      case Left(value)  => println(s"${value.name} failed: ${value.why}")
      case Right(value) => println(s"${value.name}: ${value.pretty}")

    args.outputHoconTo.foreach: path =>
      val stringBuilder = new StringBuilder()
      stringBuilder.append("import java.util.concurrent.TimeUnit\n\n")
      stringBuilder.append(s"object ${Platform.name}Benches {\n")
      stringBuilder.append("  val benchmarks = Map(")
      benches.foreach:
        case Left(_)      => ()
        case Right(value) =>
          stringBuilder.append(value.sbtDsl.indent(4).stripTrailing())
          stringBuilder.append(",")
      stringBuilder.append(
        s"\n  ).map { case (k, v) => (k + \"plat${Platform.name.toLowerCase}\", v) }\n}"
      )
      SaveFile.saveFile(path, stringBuilder.mkString)
