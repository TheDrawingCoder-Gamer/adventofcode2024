package gay.menkissing.common

import cats.*
import cats.syntax.all.*

object ArityN:
  export ArityNMacros.TupleN

  extension[F[_], A] (self: F[A])(using Foldable[F])
    inline def slidingN[N <: Int]: List[TupleN[A, N]] = ArityNMacros.slidingN[F, A, N](self)
    inline def groupedN[N <: Int]: List[TupleN[A, N]] = ArityNMacros.groupedN[F, A, N](self)

  extension[F[_], TS <: Tuple](self: F[TS])(using Functor[F])
    inline def unzipN: Tuple.Map[TS, F] = ArityNMacros.unzipN[F, TS](self)

