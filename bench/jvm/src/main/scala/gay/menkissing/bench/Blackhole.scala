package gay.menkissing.bench

object Blackhole:
  type Impl = JavaBlackhole

  extension (hole: Impl)
    def consumed(obj: Any): Unit = hole.consume(obj)

    def teardown(): Unit = ()

  def obtainBlackhole(): Impl = new JavaBlackhole()
