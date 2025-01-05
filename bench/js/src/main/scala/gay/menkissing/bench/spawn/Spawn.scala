package gay.menkissing.bench.spawn

import gay.menkissing.bench.{Args, IterationPlan, IterationResult, Main}

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
    def spawnSync(command: String, args: js.Array[String], options: js.Dynamic): SpawnResult = js.native

  @js.native
  @JSImport("node:child_process", JSImport.Namespace)
  object childProcess extends ChildProcessPackage

  private def runForkedCommand(plan: IterationPlan, name: String): SpawnResult =
    val args = Args.process.argv
    val frontMatter = args.takeWhile(_ != "--").toSeq
    childProcess.spawnSync(frontMatter.head, js.Array(Seq("--expose-gc")*), js.Dynamic.literal("input" ->
      s"""
        |const { JSForkedMain } = await import("${frontMatter.last}")
        |
        |JSForkedMain.forkedMain("$name", ${plan.warmup}, ${plan.measurement})
        |""".stripMargin,
      "encoding" -> "utf8", "stdio" -> js.Array("pipe", "inherit", "pipe")))


  def run(plan: IterationPlan, name: String): IterationResult =
    val res = runForkedCommand(plan, name)
    IterationResult.parse(res.stderr.linesIterator.toList.last)


@JSExportTopLevel("JSForkedMain")
object ForkedMain:
  @JSExport("forkedMain")
  def forkedMain(name: String, warmup: Int, measurement: Int): Unit = {

    val plan = IterationPlan(warmup, measurement)

    val benchmark = Main.benchmarkMap(name)
    val res = benchmark.run(plan)

    System.err.println(res.serialized)
  }