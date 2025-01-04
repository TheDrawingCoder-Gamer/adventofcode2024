package gay.menkissing.bench

object Blackhole:
  type Impl = Unit

  extension (hole: Impl)
    def consumed(obj: Any): Unit = ()

    def teardown(): Unit = ()

  def obtainBlackhole(): Impl = ()