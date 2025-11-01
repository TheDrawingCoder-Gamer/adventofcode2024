package gay.menkissing.common

import annotation.{experimental, tailrec}
import compiletime.ops.int.*
import scala.quoted.*
import cats.*

import scala.compiletime.erasedValue
import scala.reflect.ClassTag

object ArityNMacros:

  type TupleN[A, N <: Int] <: Tuple =
    N match
      case 0    => EmptyTuple
      case S[n] => A *: TupleN[A, n]

  private def tupleTypeElements
    (tpe: Type[?])
    (using Quotes): List[quotes.reflect.TypeRepr] =
    @tailrec
    def loop
      (curr: Type[?], acc: List[quotes.reflect.TypeRepr]): List[
      quotes.reflect.TypeRepr
    ] =
      import quotes.reflect.*

      curr match
        case '[head *: tail] => loop(Type.of[tail], TypeRepr.of[head] :: acc)
        case '[EmptyTuple]   => acc

    loop(tpe, Nil).reverse

  @tailrec def sizeOfTupleMacro(curr: Type[?], acc: Int)(using Quotes): Int =
    curr match
      case '[head *: tail] => sizeOfTupleMacro(Type.of[tail], acc + 1)
      case '[EmptyTuple]   => acc

  def seqToTupleNMacro[A, N <: Int]
    (e: Expr[Seq[A]])
    (using Quotes, Type[A], Type[N]): Expr[TupleN[A, N]] =
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

  def sharedGroupedNMacro[F[_], A, Size <: Int]
    (
      fa: Expr[F[A]],
      step: Expr[Int],
      foldable: Expr[Foldable[F]]
    )
    (using Quotes, Type[F], Type[A], Type[Size]): Expr[List[TupleN[A, Size]]] =
    val n = Type.valueOfConstant[Size].get
    val e = '{ (x: Seq[A]) => ${ seqToTupleNMacro('x) } }
    val ne = Expr(n)
    '{
      val fold = $foldable
      val iterator = fold.toIterable($fa).iterator
      new iterator.GroupedIterator[A](iterator, $ne, $step).withPartial(false)
        .map($e).toList

    }

  def slidingNMacro[F[_], A, N <: Int]
    (
      fa: Expr[F[A]],
      foldable: Expr[Foldable[F]]
    )
    (using Quotes, Type[F], Type[A], Type[N]): Expr[List[TupleN[A, N]]] =
    sharedGroupedNMacro[F, A, N](fa, Expr(1), foldable)

  def groupedNMacro[F[_], A, N <: Int]
    (
      fa: Expr[F[A]],
      foldable: Expr[Foldable[F]]
    )
    (using Quotes, Type[F], Type[A], Type[N]): Expr[List[TupleN[A, N]]] =
    val n = Type.valueOfConstant[N].get
    sharedGroupedNMacro[F, A, N](fa, Expr(n), foldable)

  inline def combinationsN[F[_], A, N <: Int]
    (
      fa: F[A]
    )
    (using foldable: Foldable[F], tag: ClassTag[A]): Iterator[TupleN[A, N]] =
    val ls = foldable.toIterable(fa).toArray
    ls.combinations(compiletime.constValue[N])
      .map(it => Tuple.fromArray(it).asInstanceOf[TupleN[A, N]])

  inline def slidingN[F[_], A, N <: Int]
    (inline fa: F[A])
    (using foldable: Foldable[F]): List[TupleN[A, N]] =
    ${ slidingNMacro('fa, 'foldable) }

  inline def groupedN[F[_], A, N <: Int]
    (inline fa: F[A])
    (using foldable: Foldable[F]): List[TupleN[A, N]] =
    ${ groupedNMacro('fa, 'foldable) }

  inline def unzipN[F[_], TS <: Tuple]
    (fa: F[TS])
    (using functor: Functor[F]): Tuple.Map[TS, F] =
    val size = compiletime.constValue[Tuple.Size[TS]]
    Tuple.fromArray(
      (0 until size).map(i => functor.fmap(fa)(_.productElement(i))).toArray
    ).asInstanceOf[Tuple.Map[TS, F]]

  type RightNestedTuple[TS <: Tuple] =
    Tuple.Fold[Tuple.Take[TS, Tuple.Size[TS] - 2], Tuple.Drop[TS, Tuple.Size[
      TS
    ] - 2], Tuple2]

  def nestedValues[TS <: Tuple](t: RightNestedTuple[TS]): TS =
    @tailrec def go[XS](cur: XS, acc: List[Any]): List[Any] =
      cur match
        case (head, tail: Tuple) => go(tail, head :: acc)
        case (head, tail)        => tail :: head :: acc
        case _                   => acc

    val r = go[RightNestedTuple[TS]](t, Nil).reverse
    Tuple.fromArray(r.toArray).asInstanceOf[TS]

  def asNested[TS <: Tuple](t: TS): RightNestedTuple[TS] =
    val arr = t.toArray
    arr.dropRight(2).foldRight[Any]((arr(arr.length - 2), arr(arr.length - 1))):
      (i, acc) => (i, acc).asInstanceOf[Any]
    .asInstanceOf[RightNestedTuple[TS]]

  inline def imapN[F[_], TS <: Tuple, Z]
    (fa: Tuple.Map[TS, F])
    (f: TS => Z)
    (g: Z => TS)
    (using s: Semigroupal[F], i: Invariant[F]): F[Z] =
    val size = compiletime.constValue[Tuple.Size[TS]]
    val nestedProducts = (0 until (size - 2)).foldRight(
      s.product(
        fa.productElement(size - 2).asInstanceOf[F[Any]],
        fa.productElement(size - 1).asInstanceOf[F[Any]]
      ).asInstanceOf[F[Any]]
    ): (i, acc) =>
      s.product(fa.productElement(i).asInstanceOf[F[Any]], acc)
        .asInstanceOf[F[Any]]
    .asInstanceOf[F[RightNestedTuple[TS]]]

    i.imap(nestedProducts) { x => f(nestedValues(x)) } { z =>
      val res = g(z); asNested(res)
    }
