package gay.menkissing.common

import compiletime.*
import compiletime.ops.int.*
import scala.deriving.*
import quoted.*

object Macros:
  private inline def summonSingletonsImpl[T <: Tuple, A]
    (inline typeName: Any): List[A] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   =>
        inline summonInline[Mirror.Of[h]] match
          // You may not like it, but m may be null
          case m: Mirror.Singleton =>
            m.asInstanceOf[A] :: summonSingletonsImpl[t, A](typeName)
          case m: Mirror.SumOf[h] =>
            summonSingletonsImpl[m.MirroredElemTypes, A](typeName) ++
              summonSingletonsImpl[t, A](typeName)
          case m: Mirror =>
            error(
              "Non-singleton/sum type found: " + constValue[m.MirroredLabel]
            )

  inline def summonSingletons[T](using s: Mirror.SumOf[T]): List[T] =
    summonSingletonsImpl[s.MirroredElemTypes, T](constValue[s.MirroredLabel])

  type TupleN[T, N <: Int] =
    N match
      case 0    => EmptyTuple
      case S[n] => T *: TupleN[T, n]

  type TupleIsHomo[T <: Tuple] = T =:= TupleN[Tuple.Head[T], Tuple.Size[T]]

  def countTuple[Elems: Type](using Quotes): Int =
    Type.of[Elems] match
      case '[elem *: elems] => 1 + countTuple[elems]
      case '[EmptyTuple]    => 0

  def countTupleExpr[Elems: Type](using Quotes): Expr[Int] =
    val size = countTuple[Elems]
    Expr(size)

  // Tuple size in term, but determined from type instead of term
  inline def tupleSize[T <: Tuple]: Int = ${ countTupleExpr[T] }
