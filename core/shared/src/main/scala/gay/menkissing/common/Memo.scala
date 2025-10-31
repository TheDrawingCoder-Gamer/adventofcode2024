package gay.menkissing.common
import scala.annotation.experimental
import scala.collection.mutable as mut

object Memo:
  def memoize[A, B](f: A => B): A => B =
    val cache = mut.HashMap[A, B]()
    (a: A) => cache.getOrElseUpdate(a, f(a))
  def memoize[A1, A2, B](f: (A1, A2) => B): (A1, A2) => B =
    val cache = mut.HashMap[(A1, A2), B]()
    (a1: A1, a2: A2) => cache.getOrElseUpdate((a1, a2), f(a1, a2))
  def memoize[A1, A2, A3, B](f: (A1, A2, A3) => B): (A1, A2, A3) => B =
    val cache = mut.HashMap[(A1, A2, A3), B]()
    (a1: A1, a2: A2, a3: A3) => cache.getOrElseUpdate((a1, a2, a3), f(a1, a2, a3))
  @experimental def memoizeTupled[F, Args <: NonEmptyTuple, R](f: F)(using tf: util.TupledFunction[F, Args => R]): F =
    val cache = mut.HashMap[Args, R]()
    val tupledF = tf.tupled(f)
    tf.untupled(args => cache.getOrElseUpdate(args, tupledF(args)))

  def memoizeCond[A, B](f: A => B, saveWhen: A => Boolean): A => B =
    val cache = mut.HashMap[A, B]()
    (a: A) => if saveWhen(a) then cache.getOrElseUpdate(a, f(a)) else f(a)

  def memoizeCond[A1, A2, B](f: (A1, A2) => B, saveWhen: (A1, A2) => Boolean): (A1, A2) => B =
    val cache = mut.HashMap[(A1, A2), B]()
    (a1: A1, a2: A2) => if saveWhen(a1, a2) then cache.getOrElseUpdate((a1, a2), f(a1, a2)) else f(a1, a2)
