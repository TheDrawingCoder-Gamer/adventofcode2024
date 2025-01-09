package gay.menkissing.bench.spawn

import gay.menkissing.bench.{BenchmarkRunOpts, IterationPlan, IterationResult, Main, Verbosity}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationDouble
import scala.concurrent.duration.DurationConversions.*
import scala.sys.process.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Await

object Spawn:
  private def forkedMainCommand(name: String, runOpts: BenchmarkRunOpts): ProcessBuilder =
    Process(Seq("java", "-cp", System.getProperty("java.class.path"), "gay.menkissing.bench.spawn.ForkedMain", name, runOpts.verbosity.ordinal.toString, runOpts.timeout.toString))
  def run(name: String, runOpts: BenchmarkRunOpts): Option[Vector[Double]] =
    var errLine = ""
    val logger = ProcessLogger(out => println(out), err => {
      errLine = err
    })
    forkedMainCommand(name, runOpts).!(logger)
    if errLine == "timed-out" then
      None
    else
      Some(errLine.split(',').map(_.toDouble).toVector)

object ForkedMain:
  def main(args: Array[String]): Unit = {
    val name = args(0)
    val verbosity = Verbosity.fromOrdinal(args(1).toInt)
    val timeout =
      args(2) match
        case s"Some($value)" => Some(Duration(value))
        case "None" => None

    val benchmark = Main.benchmarkMap(name)
    val res = Future(benchmark.run(verbosity))
    
    try {
      val r = Await.result(res, timeout.getOrElse(Duration.Inf))
      System.err.println(r.mkString(","))
    } catch {
      case e: TimeoutException => System.err.println("timed-out")
    }


  }