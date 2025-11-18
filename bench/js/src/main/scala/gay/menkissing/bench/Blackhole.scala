package gay.menkissing.bench

// stolen from https://github.com/japgolly/scalajs-benchmark/blob/master/benchmark/src/main/scala/japgolly/scalajs/benchmark/engine/Blackhole.scala
class JSBlackhole:
  var x = false
  var ar: AnyRef = ""

  final def consume(obj: Object): Unit = x ^= obj eq ar

  final def consume(b: Byte): Unit = x ^= b == 0

  final def consume(bool: Boolean): Unit = x ^= bool

  final def consume(c: Char): Unit = x ^= c == 0

  final def consume(s: Short): Unit = x ^= s == 0

  final def consume(i: Int): Unit = x ^= i == 0

  final def consume(l: Long): Unit = x ^= l == 0L

  final def consume(f: Float): Unit = x ^= f == 0

  final def consume(d: Double): Unit = x ^= d == 0

  final def consumeA[A](a: A): Unit = consume(a :: Nil)

object Blackhole:
  type Impl = JSBlackhole

  extension (hole: Impl)
    def consumed[A](obj: A): Unit = hole.consumeA(obj)
    def teardown(): Unit = ()

  def obtainBlackhole(): Impl = new JSBlackhole
