package gay.menkissing.advent

import java.nio.file.Path
import scala.io.Source
import scala.scalajs.js.annotation.JSImport
import scala.util.Using
import scalajs.js




object FileIO:
  @js.native
  @JSImport("node:fs", "readFileSync")
  def readFileSync(name: String, opts: String): String = js.native


  private def padNum(day: Int): String =
    if day < 10 then s"0$day" else day.toString
  def getInput(year: Int, day: Int, test: Boolean = false, extraInfo: String = ""): String =
    readFileSync(s"core/shared/src/main/resources/y$year/day${padNum(day)}$extraInfo${if test then "tst" else ""}.txt", "utf8")
