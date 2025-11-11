import sttp.client4.quick.*
import java.nio.file.Path
import java.nio.file.Files

object Main:

  def requestInput(cookie: String, year: Int, day: Int): String =
    quickRequest.get(uri"https://adventofcode.com/$year/day/$day/input")
      .cookie("session" -> cookie).send().body

  def padNum(n: Int): String = if n < 10 then s"0$n" else n.toString

  val inputDir = Path.of("core/shared/src/main/resources")

  def main(args: Array[String]): Unit =
    if args.isEmpty then
      println(
        "Hey pal. You have to go to your AoC website and get your session cookie,"
      )
      println("and submit it as an arg.")
      println("YOU KNOW HOW YOU NERD!!!")
      return
    val cookie = args(0)

    for year <- 2015 to 2024 do
      val dir = inputDir.resolve(s"y$year")
      if !Files.exists(dir) then Files.createDirectory(dir)
      for day <- 1 to 25 do
        val dayFile = dir.resolve(s"day${padNum(day)}.txt")

        if !Files.exists(dayFile) then
          val i = requestInput(cookie, year, day)

          Files.writeString(dayFile, i)
