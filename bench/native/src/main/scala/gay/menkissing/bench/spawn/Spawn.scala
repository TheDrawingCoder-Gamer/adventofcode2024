package gay.menkissing.bench.spawn

import gay.menkissing.bench.{IterationPlan, IterationResult, Main}

object Spawn:
  def run(plan: IterationPlan, name: String, quiet: Boolean): Vector[Double] =
    // Native doesn't actually need forking so this should be fine
    Main.benchmarkMap(name).run(quiet)

