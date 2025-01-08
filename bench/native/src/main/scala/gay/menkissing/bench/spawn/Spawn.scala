package gay.menkissing.bench.spawn

import gay.menkissing.bench.{IterationPlan, IterationResult, Main, Verbosity}

object Spawn:
  def run(name: String, verbosity: Verbosity): Vector[Double] =
    // Native doesn't actually need forking so this should be fine
    Main.benchmarkMap(name).run(verbosity)

