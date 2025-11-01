package gay.menkissing.bench

import scala.scalajs.js.annotation.JSGlobal
import scalajs.js

object Gc:
  @js.native
  @JSGlobal("gc")
  def jsGc(minorGc: Boolean): Unit = js.native

  def gc(): Unit = jsGc(false)
