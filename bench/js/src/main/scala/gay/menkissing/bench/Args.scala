package gay.menkissing.bench

import scala.scalajs.js.annotation.JSGlobal
import scalajs.js

object Args:
  @js.native
  trait ProcessObj extends js.Object:
    val argv: js.Array[String] = js.native

  @js.native
  @JSGlobal("process")
  object process extends ProcessObj

  def args(args: Array[String]): Array[String] =
    val _ = args
    val r = process.argv.toArray.dropWhile(_ != "--")
    if r.nonEmpty then r.drop(1)
    else r
