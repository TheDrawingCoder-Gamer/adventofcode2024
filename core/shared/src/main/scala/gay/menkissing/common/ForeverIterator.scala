package gay.menkissing.common

import scala.collection.mutable

class ForeverIterator[A](val underlying: Iterator[A]) extends Iterator[A]:
  private var memoizedAll: Boolean = false
  private val memoizedValues = mutable.ArrayBuffer[A]()
  private var currentIterator = underlying

  override def hasNext: Boolean = memoizedAll || currentIterator.hasNext

  override def next(): A =
    if currentIterator.hasNext then
      val v = currentIterator.next()
      if !memoizedAll then
        memoizedValues.append(v)
        if !currentIterator.hasNext then memoizedAll = true
      v
    else if memoizedAll then
      currentIterator = memoizedValues.iterator
      currentIterator.next()
    else throw new NoSuchElementException("Input iterator was empty")
