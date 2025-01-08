package gay.menkissing.bench.spawn

import gay.menkissing.bench.{IterationPlan, IterationResult, Main, Verbosity}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scala.sys.process.*

object Spawn:
  private def forkedMainCommand(name: String, verbosity: Verbosity): ProcessBuilder =
    Process(Seq("java", "-cp", System.getProperty("java.class.path"), "gay.menkissing.bench.spawn.ForkedMain", name, verbosity.ordinal.toString))
  def run(name: String, verbosity: Verbosity): Vector[Double] =
    var errLine = ""
    val logger = ProcessLogger(out => println(out), err => {
      errLine = err
    })
    forkedMainCommand(name, verbosity).!(logger)
    errLine.split(',').map(_.toDouble).toVector

object ForkedMain:
  def main(args: Array[String]): Unit = {
    val name = args(0)
    val verbosity = Verbosity.fromOrdinal(args(1).toInt)

    val benchmark = Main.benchmarkMap(name)
    val res = benchmark.run(verbosity)

    System.err.println(res.mkString(","))
  }