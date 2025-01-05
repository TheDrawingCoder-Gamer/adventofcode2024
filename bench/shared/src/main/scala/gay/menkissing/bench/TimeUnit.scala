package gay.menkissing.bench

// Pow of 10 related to seconds
enum TimeUnit(val pow: Int, val display: String):
  case Seconds extends TimeUnit(0, "s")
  case Milliseconds extends TimeUnit(3, "ms")
  case Microseconds extends TimeUnit(6, "μs")
  case Nanoseconds extends TimeUnit(9, "ns")

  def convertTo(sourceValue: Double, to: TimeUnit): Double =
    TimeUnit.convert(this, to, sourceValue)

  def serialized: String =
    this match
      case Seconds => "s"
      case Milliseconds => "ms"
      case Microseconds => "us"
      case Nanoseconds => "ns"

object TimeUnit:
  def convert(from: TimeUnit, to: TimeUnit, value: Double): Double =
    value * math.pow(10, to.pow - from.pow)
    
  def parse(str: String): TimeUnit =
    str match
      case "s" => Seconds
      case "ms" => Milliseconds
      case "us" => Microseconds
      case "ns" => Nanoseconds