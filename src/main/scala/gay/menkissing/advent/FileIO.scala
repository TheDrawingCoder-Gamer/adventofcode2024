package gay.menkissing.advent

import java.nio.file.Path
import scala.io.Source
import scala.util.Using

object FileIO:
  def getContentsOf(str: String): String =
    Using(Source.fromFile(s"src/main/resources/$str")): src => 
      src.mkString
    .get
  
  def getInput(year: Int, day: Int, test: Boolean = false, extraInfo: String = ""): String =
    if year == 2024 then
      getContentsOf(s"day$day$extraInfo${if test then "tst" else ""}.txt")
    else
      getContentsOf(s"y$year/day$day$extraInfo${if test then "tst" else ""}.txt")
    