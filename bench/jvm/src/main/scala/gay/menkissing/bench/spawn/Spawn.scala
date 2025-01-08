package gay.menkissing.bench.spawn

import gay.menkissing.bench.{IterationPlan, IterationResult, Main}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scala.sys.process.*

object Spawn:
  private def forkedMainCommand(plan: IterationPlan, name: String, quiet: Boolean): ProcessBuilder =
    Process(Seq("java", "-cp", System.getProperty("java.class.path"), "gay.menkissing.bench.spawn.ForkedMain", plan.warmup.toString, plan.measurement.toString, name, quiet.toString))
  def run(plan: IterationPlan, name: String, quiet: Boolean): Vector[Double] =
    var errLine = ""
    val logger = ProcessLogger(out => println(out), err => {
      errLine = err
    })
    forkedMainCommand(plan, name, quiet).!(logger)
    errLine.split(',').map(_.toDouble).toVector

object ForkedMain:
  def main(args: Array[String]): Unit = {
    val warmup = args(0).toInt
    val measurement = args(1).toInt
    val name = args(2)
    val quiet = args(3).toBoolean

    val plan = IterationPlan(warmup, measurement)

    val benchmark = Main.benchmarkMap(name)
    val res = benchmark.run(quiet)

    System.err.println(res.mkString(","))
  }