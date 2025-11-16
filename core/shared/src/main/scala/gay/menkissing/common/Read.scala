package gay.menkissing.common

import cats.*

/**
 * The dual to [[cats.Show]], Read lets you read a value from a string.
 */
trait Read[A] extends Read.CovariantRead[A]

object Read:
  trait CovariantRead[+A]:
    def read(str: String): Option[A]

  given stdReadForInt: Read[Int] with
    def read(str: String): Option[Int] = str.toIntOption

  given stdReadForLong: Read[Long] with
    def read(str: String): Option[Long] = str.toLongOption

  def unapply[A](in: String)(using read: Read[A]): Option[A] = read.read(in)

  given functorForRead: Functor[Read] with
    def map[A, B](fa: Read[A])(f: A => B): Read[B] = s => fa.read(s).map(f)
