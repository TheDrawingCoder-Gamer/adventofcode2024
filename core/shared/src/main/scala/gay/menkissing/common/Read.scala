package gay.menkissing.common

/**
 * The dual to [[cats.Show]], Read lets you read a value from a string.
 */
trait Read[A]:
  def read(str: String): Option[A]

object Read:
  given stdReadForInt: Read[Int] with
    def read(str: String): Option[Int] = str.toIntOption

  given stdReadForLong: Read[Long] with
    def read(str: String): Option[Long] = str.toLongOption

  def unapply[A](in: String)(using read: Read[A]): Option[A] = read.read(in)
