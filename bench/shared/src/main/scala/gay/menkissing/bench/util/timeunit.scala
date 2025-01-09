package gay.menkissing.bench.util

import java.util.concurrent.TimeUnit

extension (self: TimeUnit) {
  def display: String =
    self match
      case TimeUnit.DAYS => "days"
      case TimeUnit.HOURS => "hours"
      case TimeUnit.MINUTES => "mins"
      case TimeUnit.SECONDS => "s"
      case TimeUnit.MILLISECONDS => "ms"
      case TimeUnit.MICROSECONDS => "μs"
      case TimeUnit.NANOSECONDS => "ns"
      
  def serialized: String =
    self match
      case TimeUnit.MICROSECONDS => "us"
      case _ => self.display
}