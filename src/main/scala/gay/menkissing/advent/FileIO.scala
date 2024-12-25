package gay.menkissing.advent

import java.nio.file.Path
import scala.io.Source
import scala.util.Using

object FileIO:
  def getContentsOf(str: String): String = 
    println(Path.of("").toAbsolutePath)
    Using(Source.fromFile(s"src/main/resources/$str")): src => 
      src.mkString
    .get
