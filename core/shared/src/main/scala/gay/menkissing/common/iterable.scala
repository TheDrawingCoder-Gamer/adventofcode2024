package gay.menkissing.common

import annotation.tailrec


extension[A](seq: IterableOnce[A])
  def countWhile(f: A => Boolean): Int =
    val iterator = seq.iterator
    var i = 0
    while iterator.hasNext do
      val head = iterator.next()
      if f(head) then
        i += 1
      else
        return i
    i
