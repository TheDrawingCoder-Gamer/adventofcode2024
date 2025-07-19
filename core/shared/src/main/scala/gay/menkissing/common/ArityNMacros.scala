package gay.menkissing.common

import annotation.{experimental, tailrec}
import compiletime.ops.int.*
import scala.quoted.*
import cats.*

object ArityNMacros:

  type TupleN[A, N <: Int] <: Tuple = N match
    case 0 => EmptyTuple
    case S[n] => A *: TupleN[A, n]

  private def tupleTypeElements(tpe: Type[?])(using Quotes): List[quotes.reflect.TypeRepr] =
    @tailrec def loop(using Quotes)(curr: Type[?], acc: List[quotes.reflect.TypeRepr]): List[quotes.reflect.TypeRepr] =
      import quotes.reflect.*

      curr match
        case '[head *: tail] => loop(Type.of[tail], TypeRepr.of[head] :: acc)
        case '[EmptyTuple] => acc
        
    loop(tpe, Nil).reverse

  @tailrec def sizeOfTupleMacro(curr: Type[?], acc: Int)(using Quotes): Int =
    curr match
      case '[head *: tail] => sizeOfTupleMacro(Type.of[tail], acc + 1)
      case '[EmptyTuple] => acc
  
    




  def unzipNMacro[F[_], TS <: Tuple](fa: Expr[F[TS]], func: Expr[Functor[F]])(using Quotes, Type[F], Type[TS]): Expr[Tuple.Map[TS, F]] =
    val size = sizeOfTupleMacro(Type.of[TS], 0)
    '{
      val functor = $func
      val h = $fa
      ${
        Expr.ofTupleFromSeq:
          (0 until size).map: i =>
            val z = Expr(i)
            '{ functor.fmap(h)((it: TS) => it.productElement($z)) }

      }.asInstanceOf[Tuple.Map[TS, F]]
    }

  def seqToTupleNMacro[A, N <: Int](e: Expr[Seq[A]])(using Quotes, Type[A], Type[N]): Expr[TupleN[A, N]] =
    val n = Type.valueOfConstant[N].get
    '{
      val x = $e
      ${
        Expr.ofTupleFromSeq:
          (0 until n).map: i =>
            val z = Expr(i)
            '{ x($z) }
        .asExprOf[TupleN[A, N]]
      }
    }

  def sharedGroupedNMacro[F[_], A, Size <: Int](fa: Expr[F[A]], step: Expr[Int], foldable: Expr[Foldable[F]])(using Quotes, Type[F], Type[A], Type[Size]): Expr[List[TupleN[A, Size]]] =
    val n = Type.valueOfConstant[Size].get
    val e = '{ (x: Seq[A]) => ${ seqToTupleNMacro('x) }  }
    val ne = Expr(n)
    '{
      val fold = $foldable
      val iterator = fold.toIterable($fa).iterator
      new iterator.GroupedIterator[A](iterator, $ne, $step).withPartial(false).map($e).toList

    }

  def slidingNMacro[F[_], A, N <: Int](fa: Expr[F[A]], foldable: Expr[Foldable[F]])(using Quotes, Type[F], Type[A], Type[N]): Expr[List[TupleN[A, N]]] =
    sharedGroupedNMacro[F, A, N](fa, Expr(1), foldable)

  def groupedNMacro[F[_], A, N <: Int](fa: Expr[F[A]], foldable: Expr[Foldable[F]])(using Quotes, Type[F], Type[A], Type[N]): Expr[List[TupleN[A, N]]] =
    val n = Type.valueOfConstant[N].get
    sharedGroupedNMacro[F, A, N](fa, Expr(n), foldable)



  inline def slidingN[F[_], A, N <: Int](inline fa: F[A])(using foldable: Foldable[F]): List[TupleN[A, N]] =
    ${ slidingNMacro('fa, 'foldable) }

  inline def groupedN[F[_], A, N <: Int](inline fa: F[A])(using foldable: Foldable[F]): List[TupleN[A, N]] =
    ${ groupedNMacro('fa, 'foldable) }

  inline def unzipN[F[_], TS <: Tuple](fa: F[TS])(using functor: Functor[F]): Tuple.Map[TS, F] =
    ${ unzipNMacro[F, TS]('fa, 'functor)}
