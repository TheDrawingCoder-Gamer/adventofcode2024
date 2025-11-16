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

  def summonProductFields[T: Type]
    (m: Expr[Mirror.ProductOf[T]])
    (using q: Quotes): List[Field[q.type, T, ?]] =
    import quotes.reflect.*
    m match
      case '{
            $m: Mirror.ProductOf[T] {
              type MirroredElemLabels = elementLabels
              type MirroredElemTypes = elementTypes
            }
          } =>
        val parentType = Type.of[T]
        val labels =
          tupleToList(TypeRepr.of[elementLabels]).map(_.valueAs[String])
        val typeReprs = tupleToList(TypeRepr.of[elementTypes])
        labels.zip(typeReprs).map: (label, kind) =>
          kind.asType match
            case '[t] => Field(using quotes, parentType, Type.of[t])(label)

  class Field[Q <: Quotes, S, A]
    (using val quotes: Q, val parentType: Type[S], val fieldType: Type[A])
    (val label: String):
    import quotes.reflect.*
    def dereference(parent: Term): Term = Select.unique(parent, label)
    def get(parent: Expr[S]): Expr[A] = dereference(parent.asTerm).asExprOf[A]
    def set(parent: Expr[S], value: Expr[A]): Expr[S] =
      import quotes.reflect.*
      val term =
        Select.overloaded(
          parent.asTerm,
          "copy",
          TypeRepr.of[A] :: Nil,
          value.asTerm :: Nil
        )
      term.asExprOf[S]

  def exprOfMap[V: Type]
    (using Quotes)
    (as: Seq[(String, Expr[V])]): Expr[Map[String, V]] =
    '{ Map(${ Varargs(as.map { case (k, v) => '{ ${ Expr(k) } -> $v } }) }*) }

  type TupleN[T, N <: Int] =
    N match
      case 0    => EmptyTuple
      case S[n] => T *: TupleN[T, n]

  type TupleIsHomo[T <: Tuple] = T =:= TupleN[Tuple.Head[T], Tuple.Size[T]]

  extension (using q: Quotes)(self: q.reflect.TypeRepr)
    def valueAs[A]: A =
      import q.reflect.*
      self.asType match
        case '[t] => Type.valueOfConstant[t].get.asInstanceOf[A]
        case _ => report.errorAndAbort(s"expected a literal, got ${self.show}")

  def tupleToList
    (using q: Quotes)
    (repr: q.reflect.TypeRepr): List[q.reflect.TypeRepr] =
    import q.reflect.*
    repr.asType match
      case '[t *: ts]    => TypeRepr.of[t] :: tupleToList(TypeRepr.of[ts])
      case '[EmptyTuple] => Nil

  def countTuple[Elems: Type](using Quotes): Int =
    Type.of[Elems] match
      case '[elem *: elems] => 1 + countTuple[elems]
      case '[EmptyTuple]    => 0

  def countTupleExpr[Elems: Type](using Quotes): Expr[Int] =
    val size = countTuple[Elems]
    Expr(size)

  // Tuple size in term, but determined from type instead of term
  inline def tupleSize[T <: Tuple]: Int = ${ countTupleExpr[T] }
