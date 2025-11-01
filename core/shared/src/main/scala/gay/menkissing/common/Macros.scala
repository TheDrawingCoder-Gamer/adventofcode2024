package gay.menkissing.common

import compiletime.*
import scala.deriving.*

object Macros:
  private inline def summonSingletonsImpl[T <: Tuple, A](inline typeName: Any): List[A] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t) =>
        inline summonInline[Mirror.Of[h]] match
          // You may not like it, but m may be null
          case m: Mirror.Singleton => m.asInstanceOf[A] :: summonSingletonsImpl[t, A](typeName)
          case m: Mirror.SumOf[h] => summonSingletonsImpl[m.MirroredElemTypes, A](typeName) ++ summonSingletonsImpl[t, A](typeName)
          case m: Mirror => error("Non-singleton/sum type found: " + constValue[m.MirroredLabel])

  inline def summonSingletons[T](using s: Mirror.SumOf[T]): List[T] = summonSingletonsImpl[s.MirroredElemTypes, T](constValue[s.MirroredLabel])
