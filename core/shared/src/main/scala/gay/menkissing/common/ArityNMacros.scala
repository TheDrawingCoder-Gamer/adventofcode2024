package gay.menkissing.common

import annotation.{experimental, tailrec}
import compiletime.ops.int.*
import cats.*

object ArityNMacros:

  import Macros.TupleN
  export Macros.TupleN

  class ListProduct[A](val ls: Seq[A]) extends Product:
    def productArity: Int = ls.length
    override def productElement(n: Int): Any = ls(n)

    def canEqual(that: Any): Boolean =
      that match
        case _: ListProduct[?] => true
        case _                 => false

  def seqToTupleN[A, N <: Int](e: Seq[A]): TupleN[A, N] =
    Tuple.fromProduct(new ListProduct(e)).asInstanceOf[TupleN[A, N]]

  inline def sharedGroupedN[F[_], A, Size <: Int]
    (fa: F[A], step: Int)
    (using fold: Foldable[F]): List[TupleN[A, Size]] =
    val n = compiletime.constValue[Size]
    val iterator = fold.toIterable(fa).iterator
    new iterator.GroupedIterator[A](iterator, n, step).withPartial(false)
      .map(seqToTupleN[A, Size]).toList

  inline def combinationsN[F[_], A, N <: Int]
    (inline fa: F[A])
    (using foldable: Foldable[F]): Iterator[TupleN[A, N]] =
    val n = compiletime.constValue[N]
    val ls = foldable.toList(fa)
    ls.combinations(n).map(seqToTupleN[A, N])

  inline def slidingN[F[_], A, N <: Int]
    (inline fa: F[A])
    (using foldable: Foldable[F]): List[TupleN[A, N]] =
    sharedGroupedN[F, A, N](fa, 1)

  inline def groupedN[F[_], A, N <: Int]
    (inline fa: F[A])
    (using foldable: Foldable[F]): List[TupleN[A, N]] =
    val n = compiletime.constValue[N]
    sharedGroupedN[F, A, N](fa, n)

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
