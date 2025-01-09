package gay.menkissing.bench

import java.util.concurrent.TimeUnit
import scala.util.matching.Regex
import scala.collection.mutable as mut
import scala.concurrent.duration.Duration


enum Verbosity:
  case Quiet, Normal, Verbose

case class CLIArgs(verbosity: Verbosity = Verbosity.Normal,
                   timeout: Option[Duration] = None,
                   patterns: List[Regex] = List())

object CLIArgs:
  def parse(args: Array[String]): CLIArgs =
    var hasQuiet = false
    var hasNoQuiet = false
    var hasVerbose = false
    var timeout = Option.empty[Duration]
    val patterns = mut.ListBuffer.empty[Regex]
    var i = 0
    while (i < args.length) {
      val arg = args(i)
      if arg.startsWith("--") then
        arg.drop(2) match
          case "quiet" => hasQuiet = true
          case "no-quiet" => hasNoQuiet = true
          case "verbose" => hasVerbose = true
          case "timeout" =>
            i += 1
            val nextArg = args(i)
            if !nextArg.last.isDigit then
              timeout = Some(Duration(nextArg))
            else
              timeout = Some(Duration(nextArg.toDouble, TimeUnit.MILLISECONDS))
      else
        patterns.prepend(arg.r)
      i += 1
    }
    val verbosity =
      if hasVerbose then
        Verbosity.Verbose
      else if hasNoQuiet then
        Verbosity.Normal
      else if hasQuiet then
        Verbosity.Quiet
      else
        Verbosity.Normal
    CLIArgs(verbosity, timeout, patterns.toList)
