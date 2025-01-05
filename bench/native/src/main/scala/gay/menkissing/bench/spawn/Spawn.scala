package gay.menkissing.bench.spawn

import gay.menkissing.bench.{IterationPlan, IterationResult, Main}

object Spawn:
  def run(plan: IterationPlan, name: String, quiet: Boolean): IterationResult =
    // Native doesn't actually need forking so this should be fine
    Main.benchmarkMap(name).run(plan, quiet)

