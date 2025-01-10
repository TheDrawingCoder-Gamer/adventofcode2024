package gay.menkissing.bench

import java.nio.file.{Files, Path}

object SaveFile:
  def saveFile(to: String, contents: String): Unit =
    Files.writeString(Path.of(to), contents)
