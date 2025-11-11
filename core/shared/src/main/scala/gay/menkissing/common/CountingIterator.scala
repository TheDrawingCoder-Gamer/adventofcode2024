package gay.menkissing.common

final class CountingIterator[A]
  (val underlying: Iterator[A])
    extends Iterator[A]:
  var count = 0L

  def next(): A =
    count += 1
    underlying.next()
  def hasNext: Boolean = underlying.hasNext
