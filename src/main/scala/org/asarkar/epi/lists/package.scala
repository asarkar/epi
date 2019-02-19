package org.asarkar.epi

package object lists {
  /*
     * 7.1 Merge two sorted lists
     */
  def merge[A](xs: LinkedList[A], ys: LinkedList[A])(implicit ord: Ordering[A]): LinkedList[A] = {
    import ord.mkOrderingOps

    val merged = Iterator.iterate((LinkedList.empty[A], xs, ys)) { case (acc, a, b) =>
      if (a.isEmpty && b.isEmpty) (acc, a, b)
      else if (
        (a.isEmpty && b.nonEmpty) ||
          (a.nonEmpty && b.nonEmpty && a.datum > b.datum)
      ) (LinkedList(b.datum, acc), a, b.next)
      else (LinkedList(a.datum, acc), a.next, b)
    }
      .dropWhile(x => !x._2.isEmpty || !x._3.isEmpty)
      .take(1)
      .map(_._1)
      .next()

    reverseList(merged)
  }

  def reverseList[T](xs: LinkedList[T]): LinkedList[T] = {
    xs.foldLeft(LinkedList.empty[T])((acc, x) => LinkedList(x, acc))
  }

  //  def merge[T](xs: ListNode[T], ys: ListNode[T]): ListNode[T] = {
  //    def smaller(x: ListNode[T], y: ListNode[T]): Option[ListNode[T]] = {
  //      Iterator.iterate(Option(x)) {
  //        _
  //          .filter(_ <= y)
  //          .flatMap(_.next)
  //      }
  //        .takeWhile(_.isDefined)
  //        .flatten
  //        .filter(_ <= y)
  //        .reduceOption(snd[ListNode[T]])
  //    }
  //
  //    val x = smaller(xs, ys)
  //      .orElse(smaller(ys, xs))
  //      .get
  //    val y = if (x eq xs) ys else xs
  //    val next = x.next
  //
  //    x.next = Some(y)
  //    next.foreach(merge(_, y))
  //
  //    x
  //  }

  /*
   * 7.2 Reverse a single sublist
   */
  def reverseSublist[T](xs: ListNode[T], start: Int, finish: Int): ListNode[T] = {
    require(start >= 1, "start must be positive")
    require(finish > start, "finish must be greater than start")

    val beforeStart = Iterator.iterate(Option(xs))(_.flatMap(_.next))
      .take(start - 1)
      .flatten
      .reduceOption(snd[ListNode[T]])
    val startNode = beforeStart.flatMap(_.next).orElse(Some(xs))
    val stream = Stream.iterate(startNode)(_.flatMap(_.next))

    stream.zip(stream.tail)
      /*
       * in case finish - start + 1 > length
       */
      .takeWhile(_._1.isDefined)
      .take(finish - start + 1)
      .zipWithIndex
      /*
       * force evaluation of all elements to prevent infinite loop from lazy evaluation
       */
      .toList.collect {
      case ((current, next), i) if i == finish - start || next.isEmpty =>
        startNode.foreach(_.next = next)
        beforeStart.foreach(_.next = current)
        current
      case ((current, next), _) =>
        next.foreach(_.next = current)
        current
    }
      .last
      /*
       * if start > 1, head is xs, else head is last element that was reversed
       */
      .filterNot(_ => start > 1)
      .getOrElse(xs)
  }

  //  def cycle[T](xs: ListNode[T]): Option[ListNode[T]] = {
  //    Iterator.iterate((xs.next, xs.next.flatMap(_.next), false)) {
  //      case (Some(slow), Some(fast), _) if slow eq fast => (Some(slow), None, true)
  //      case (Some(slow), Some(fast), _) => (slow.next, fast.next.flatMap(_.next), false)
  //      case _ => (None, None, false)
  //    }
  //      .dropWhile(_._2.isDefined)
  //      .take(1)
  //      /*
  //       * either end of list or cycle found
  //       */
  //      .filter(_._3)
  //      /*
  //       * map + flatten
  //       */
  //      .flatMap(_._1)
  //      .reduceOption(snd[ListNode[T]])
  //  }

  /*
   * 7.3 Test for cyclicity
   *
   * ANSWER: We will use the Floyd's Tortoise and Hare algorithm.
   * https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_Tortoise_and_Hare
   *
   * See image cycle.jpg.
   * Let's say when pointers 'slow' and 'fast' meet, they each have completed 'i' and 'j' rounds of the loop,
   * respectively. Since 'fast' traveled twice as fast as 'slow', we can write:
   * s + jl + x = 2(s + il + x)
   * s + jl + x = 2s + 2il + 2x
   * s + x = (j - 2i)l
   * s = kl - x, where k is some integer
   *
   * But the RHS is precisely the start of the loop. Thus, if we restart the slow pointer from the beginning,
   * and advance the fast pointer by one step at a time, they'll meet at the start of the loop.
   */
  def startOfCycle[A](xs: LinkedList[A]): Option[A] = {
    val x = Iterator.iterate((xs, xs)) { case (slow, fast) =>
      if (fast.next.isEmpty) (LinkedList.empty[A], LinkedList.empty[A])
      else if ((slow ne xs) && (slow.datum == fast.datum)) (slow, LinkedList.empty[A])
      else (slow.next, fast.next.next)
    }
      .dropWhile(y => y._2.nonEmpty)
      .take(1)
      .map(_._1)
      .next()

    if (x.isEmpty) None
    else {
      Iterator.iterate((xs, x))(p => (p._1.next, p._2.next))
        .dropWhile(y => y._1.datum != y._2.datum)
        .take(1)
        .map(y => Some(y._1.datum))
        .next()
    }
  }

  /*
   * 7.4 Test for overlapping lists - cycle-free
   */
  def overlapNoCycle[T](xs: ListNode[T], ys: ListNode[T]): Option[ListNode[T]] = {
    val size1 = xs.size
    val size2 = ys.size
    val shorter = if (size1 < size2) xs else ys
    val longer = if (shorter eq xs) ys else xs

    val start: Option[ListNode[T]] = Iterator.iterate(Option(longer))(_.flatMap(_.next))
      .zipWithIndex
      .dropWhile(_._2 < math.abs(size1 - size2))
      .take(1)
      .flatMap(_._1)
      .reduceOption(snd[ListNode[T]])

    Iterator.iterate((start, Option(shorter), false)) {
      case (Some(l), Some(s), _) if l eq s => (Some(s), None, true)
      case (Some(l), Some(s), _) => (l.next, s.next, false)
      case _ => (None, None, false)
    }
      .dropWhile(_._2.isDefined)
      .take(1)
      .filter(_._3)
      .flatMap(_._1)
      .reduceOption(snd[ListNode[T]])
  }

  /*
   * 7.10 Implement even-odd merging
   */
  def evenOdd[T](xs: ListNode[T]): ListNode[T] = {
    val (lastEven, firstOdd) = Iterator.iterate((Option(xs), xs.next)) { case (even, odd) =>
      even.foreach(x => x.next = odd.flatMap(_.next))
      odd.foreach(x => x.next = even.flatMap(_.next).flatMap(_.next))

      (even.flatMap(_.next), odd.flatMap(_.next))
    }
      .takeWhile(x => x._1.isDefined || x._2.isDefined)
      .foldLeft((Option.empty[ListNode[T]], Option.empty[ListNode[T]])) { case (acc, (even, odd)) =>
        (even, acc._2.orElse(odd))
      }

    lastEven.foreach(_.next = firstOdd)
    xs
  }
}
