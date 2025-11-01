package gay.menkissing.bench.spawn

import gay.menkissing.bench.{
  Args,
  BenchmarkRunOpts,
  IterationPlan,
  IterationResult,
  Main,
  Verbosity
}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel, JSImport}
import scala.sys.process.*

object Spawn:

  @js.native
  trait SpawnResult extends js.Object:
    val pid: Int = js.native
    val stdout: String = js.native
    val stderr: String = js.native
    val status: Int | Null = js.native
    val signal: String | Null = js.native

  @js.native
  trait ChildProcessPackage extends js.Object:
    def spawnSync
      (
        command: String,
        args: js.Array[String],
        options: js.Dynamic
      ): SpawnResult = js.native

  @js.native
  @JSImport("node:child_process", JSImport.Namespace)
  object childProcess extends ChildProcessPackage

  private def runForkedCommand
    (name: String, runOpts: BenchmarkRunOpts): SpawnResult =
    val args = Args.process.argv
    val frontMatter = args.takeWhile(_ != "--").toSeq
    childProcess.spawnSync(
      frontMatter.head,
      js.Array(Seq("--expose-gc")*),
      js.Dynamic.literal(
        "input" ->
          s"""
             |const { JSForkedMain } = await import("${frontMatter.last}")
             |
             |JSForkedMain.forkedMain("$name", ${runOpts.verbosity.ordinal})
             |""".stripMargin,
        "encoding" -> "utf8",
        "stdio" -> js.Array("pipe", "inherit", "pipe"),
        "timeout" -> runOpts.timeout.map(_.toMillis).getOrElse(js.undefined)
      )
    )

  def run(name: String, runOpts: BenchmarkRunOpts): Option[Vector[Double]] =
    val res = runForkedCommand(name, runOpts)
    if res.signal != null then None
    else
      Some(
        res.stderr.linesIterator.toList.last.split(',').map(_.toDouble).toVector
      )

@JSExportTopLevel("JSForkedMain")
object ForkedMain:
  @JSExport("forkedMain")
  def forkedMain(name: String, verbosityN: Int): Unit =
    val verbosity = Verbosity.fromOrdinal(verbosityN)

    val benchmark = Main.benchmarkMap(name)
    val res = benchmark.run(verbosity)

    System.err.println(res.mkString(","))
