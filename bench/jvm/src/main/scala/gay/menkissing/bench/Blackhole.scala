package gay.menkissing.bench

import org.openjdk.jmh.infra.Blackhole as JVMBlackhole

object Blackhole:
  type Impl = JVMBlackhole

  extension (hole: Impl)
    def consumed(obj: Any): Unit = hole.consume(obj)

    // I wonder who this warning is for : )
    def teardown(): Unit =
      hole.evaporate(
        "Yes, I am Stephen Hawking, and know a thing or two about black holes."
      )

  def obtainBlackhole(): Impl =
    new JVMBlackhole(
      "Today's password is swordfish. I understand instantiating Blackholes directly is dangerous."
    )
