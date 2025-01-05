package gay.menkissing.bench.spawn

import gay.menkissing.bench.{IterationPlan, IterationResult, Main}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scala.sys.process.*

object Spawn:
  private def forkedMainCommand(plan: IterationPlan, name: String): ProcessBuilder =
    Process(Seq("java", "-cp", System.getProperty("java.class.path"), "gay.menkissing.bench.ForkedMain", plan.warmup.toString, plan.measurement.toString, name))
  def run(plan: IterationPlan, name: String): IterationResult =
    var errLine = ""
    val logger = ProcessLogger(out => println(out), err => {
      println(err)
      errLine = err
    })
    forkedMainCommand(plan, name).!(logger)
    IterationResult.parse(errLine)

object ForkedMain:
  def main(args: Array[String]): Unit = {
    val warmup = args(0).toInt
    val measurement = args(1).toInt
    val name = args(2)

    val plan = IterationPlan(warmup, measurement)

    val benchmark = Main.benchmarkMap(name)
    val res = benchmark.run(plan)

    System.err.println(res.serialized)
  }