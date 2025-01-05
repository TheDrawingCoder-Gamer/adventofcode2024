package gay.menkissing.bench

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("JSMain")
object JSMain:
  @JSExport("main")
  def main(): Unit = Main.main(Array())
