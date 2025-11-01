package gay.menkissing.advent

import java.nio.file.Path
import scala.io.Source
import scala.util.Using

object FileIO:
  def getContentsOf(str: String): String =
    Using(Source.fromFile(s"core/shared/src/main/resources/$str")): src =>
      src.mkString
    .get

  private def padNum(day: Int): String =
    if day < 10 then s"0$day" else day.toString
  def getInput
    (
      year: Int,
      day: Int,
      test: Boolean = false,
      extraInfo: String = ""
    ): String =
    getContentsOf(
      s"y$year/day${padNum(day)}$extraInfo${if test then "tst" else ""}.txt"
    )
