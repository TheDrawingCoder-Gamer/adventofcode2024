package gay.menkissing.common

trait Debuginator:
  inline def assert(inline arg: Boolean): Unit
  inline def assertEq(inline expected: Any, inline actual: Any): Unit
  inline def debug(inline arg: Any): Unit
  inline def verbose(inline arg: Any): Unit

object Debuginator:
  enum Level:
    case None
    case Simple
    case Verbose

  trait ActiveishDebuginator extends Debuginator:
    inline def assert(inline arg: Boolean): Unit =
      if !arg then throw AssertionError("Assertion failed")
    inline def assertEq(inline expected: Any, inline actual: Any): Unit =
      val exp = expected
      val act = actual
      if exp != act then throw AssertionError(s"expected $exp, got $act")
    inline def debug(inline arg: Any): Unit = println(arg)

  object VerboseDebuginator extends ActiveishDebuginator:
    inline def verbose(inline arg: Any): Unit = println(arg)

  object SimpleDebuginator extends ActiveishDebuginator:
    inline def verbose(inline arg: Any): Unit = ()

  object InactiveDebuginator extends Debuginator:
    inline def assert(inline arg: Boolean): Unit = ()
    inline def assertEq(inline left: Any, inline right: Any): Unit = ()
    inline def debug(inline arg: Any): Unit = ()
    inline def verbose(inline arg: Any): Unit = ()
  transparent inline def apply(inline active: Boolean) =
    inline if active then VerboseDebuginator
    else InactiveDebuginator
  transparent inline def apply(inline level: Level) =
    inline level match
      case Level.None    => InactiveDebuginator
      case Level.Simple  => SimpleDebuginator
      case Level.Verbose => VerboseDebuginator
