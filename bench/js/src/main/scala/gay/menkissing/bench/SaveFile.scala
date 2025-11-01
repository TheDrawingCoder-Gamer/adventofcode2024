package gay.menkissing.bench

import scala.scalajs.js.annotation.JSImport
import scalajs.js

object SaveFile:
  @JSImport("node:fs", "writeFileSync")
  @js.native
  def writeFileSync(file: String, data: String): Unit = js.native

  def saveFile(to: String, contents: String): Unit = writeFileSync(to, contents)
