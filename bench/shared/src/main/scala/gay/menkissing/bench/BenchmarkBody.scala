package gay.menkissing.bench

trait BenchmarkBody:
  /**
   * Load anything needed forall runs (default nothing)
   */
  def load(): Unit = ()

  /**
   * Run the benchmark
   *
   * @param blackhole
   */
  def run(blackhole: Blackhole.Impl): Unit

  /**
   * Free any items loaded after all runs are done
   */

  def free(): Unit = ()
