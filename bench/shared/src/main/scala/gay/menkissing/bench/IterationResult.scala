package gay.menkissing.bench

case class IterationResult(name: String, mean: Double, error: Double, unit: TimeUnit):
  def pretty: String =
    val goodMean = TimeUnit.Nanoseconds.convertTo(mean, unit)
    val goodError = TimeUnit.Nanoseconds.convertTo(error, unit)

    f"$name $goodMean%1.3f [+/-] ${goodError}%1.3f ${unit.display}"

  def serialized: String =
    s"$name;$mean;$error;${unit.serialized}"

object IterationResult:
  def parse(str: String): IterationResult =
    println(str)
    str match
      case s"$name;$mean;$error;$unit" =>
        IterationResult(name, mean.toDouble, error.toDouble, TimeUnit.parse(unit))
