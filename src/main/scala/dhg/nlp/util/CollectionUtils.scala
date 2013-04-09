package dhg.nlp.util

import scala.collection.GenIterable
import scala.collection.GenIterableLike
import scala.collection.GenTraversable
import scala.collection.GenTraversableLike
import scala.collection.GenTraversableOnce
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

import dhg.util.CollectionUtil._

object CollectionUtils {

  //////////////////////////////////////////////////////
  // countCompare(p: A => Boolean, count: Int): Int
  //   - Compares the number of items satisfying a predicate to a test value.
  //   - Functionally equivalent to (but more efficient than):
  //         this.count(p).compareTo(count)
  //////////////////////////////////////////////////////

  implicit class Enriched_countCompare_GenTraversableOnce[A](val self: GenTraversableOnce[A]) extends AnyVal {
    /**
     * Compares the number of items satisfying a predicate to a test value.
     *
     *   @param p       the predicate used to test elements.
     *   @param count   the test value that gets compared with the count.
     *   @return Int value `x` where
     *   {{{
     *        x <  0       if this.count(p) <  count
     *        x == 0       if this.count(p) == count
     *        x >  0       if this.count(p) >  count
     *   }}}
     *  The method as implemented here does not call `length` directly; its running time
     *  is `O(length min count)` instead of `O(length)`.
     */
    def countCompare(p: A => Boolean, count: Int): Int = {
      val itr = self.toIterator
      var i = 0
      while (itr.hasNext && i <= count) {
        if (p(itr.next))
          i += 1
      }
      i - count
    }
  }

  //////////////////////////////////////////////////////
  // sumBy[B: Numeric](f: A => B): B
  //   - Map a numeric-producing function over each item and sum the results 
  //   - Functionally equivalent to:
  //         this.map(f).sum
  //////////////////////////////////////////////////////

  implicit class Enriched_sumBy_GenTraversableOnce[A](val self: GenTraversableOnce[A]) extends AnyVal {
    /**
     * Map a numeric-producing function over each item and sum the results.
     *
     * Functionally equivalent to `this.map(f).sum`
     *
     * @param f	A function that produces a Numeric
     * @return the sum of the results after applications of f
     */
    def sumBy[B](f: A => B)(implicit num: Numeric[B]): B = {
      (num.zero /: self)((accum, x) => num.plus(accum, f(x)))
    }
  }

  //////////////////////////////////////////////////////
  // sliding2: Iterator[(A,A)]
  //   - slide over this collection to produce pairs.
  //   - Functionally equivalent to:
  //         this.sliding(2).map{Seq(a,b) => (a,b)}
  //////////////////////////////////////////////////////

  implicit class Enriched_sliding2_Iterator[A](val self: Iterator[A]) { // extends AnyVal {
    /**
     * Slide over this collection to produce pairs.
     */
    def sliding2[B >: A](): Iterator[(B, B)] =
      self.sliding(2).map { case Seq(a, b) => (a, b) }
  }

  implicit class Enriched_sliding2_GenTraversableLike[A, Repr <: GenTraversable[A]](val self: GenTraversableLike[A, Repr]) { // extends AnyVal {
    /**
     * Slide over this collection to produce pairs.
     */
    def sliding2[B >: A](): Iterator[(B, B)] =
      self.toIterator.sliding2()
  }

  //////////////////////////////////////////////////////
  // flattenByKey: Repr[(T,Traversable[S])]
  //   - Reduce a collection of collections of pairs (T,Traversable[S]), into
  //     a single collection such that all values associated with the same
  //     key are concatenated.
  //   - Functionally equivalent to:
  //         this.flatten.groupBy(_._1).mapValues(_.map(_._2).reduce(_ ++ _))
  //       or
  //         this.reduce(_ +++ _)
  //////////////////////////////////////////////////////

  implicit class Enriched_flattenByKey_TraversableOnce_Traversable[T, S, Repr2 <: Traversable[S], Repr1 <: Traversable[(T, TraversableLike[S, Repr2])]](val self: TraversableOnce[TraversableLike[(T, TraversableLike[S, Repr2]), Repr1]]) extends AnyVal {
    /**
     * Reduce a collection of collections of pairs (T,Traversable[S]), into
     * a single collection such that all values associated with the same
     * key are concatenated.
     *
     * @return a collection of pairs
     */
    def flattenByKey[That1 <: Traversable[(T, TraversableLike[S, Repr2])], That2](implicit bf2: CanBuildFrom[Repr2, S, That2], bf1: CanBuildFrom[Repr1, (T, That2), That1]) = {
      val grouped = self.toIterator.flatten.groupByKey
      val b = bf1(grouped.asInstanceOf[Repr1])
      for ((k, vs) <- grouped) {
        val b2 = bf2()
        for (v <- vs) b2 ++= v
        b += k -> b2.result
      }
      b.result
    }
  }

  //////////////////////////////////////////////////////
  // sumByKey[U:Numeric](other: Traversable[(T,U)]): Repr[(T,U)]
  //   - Given a collection of collections of pairs (T,U:Numeric), combine into
  //     a single collection such that all values associated with the same
  //     key are summed.
  //   - Functionally equivalent to:
  //         (this.iterator ++ other).groupBy(_._1).mapValues(_.map(_._2).sum)
  //       or
  //         (this.iterator ++ other).groupByKey.mapValues(_.sum)
  //       or
  //         this.reduce(_ +++ _)
  //////////////////////////////////////////////////////

  implicit class Enriched_sumByKey_GenTraversableOnce_Numeric[T, U: Numeric, Repr <: Traversable[(T, U)]](val self: GenTraversableOnce[TraversableLike[(T, U), Repr]]) {
    /**
     * Given a collection of collections of pairs (T,U:Numeric), combine into
     * a single collection such that all values associated with the same
     * key are summed.
     *
     * @param other 	another collection to add to
     * @return a collection of pairs
     */
    def sumByKey[That <: Traversable[(T, U)]](implicit bf: CanBuildFrom[Repr, (T, U), That]) = {
      val grouped = self.toIterator.flatten.groupByKey
      val b = bf(grouped.asInstanceOf[Repr])
      b.sizeHint(grouped.size)
      for ((k, vs) <- grouped) b += k -> vs.sum
      b.result
    }
  }

  implicit class Enriched_sumByKey_GenTraversableOnce_Iterator[T, U: Numeric](val self: GenTraversableOnce[Iterator[(T, U)]]) {
    /**
     * Given a collection of collections of pairs (T,U:Numeric), combine into
     * a single collection such that all values associated with the same
     * key are summed.
     *
     * @param other 	another collection to add to
     * @return a collection of pairs
     */
    def sumByKey = self.toIterator.flatten.groupByKey.mapVals(_.sum)
  }

  //////////////////////////////////////////////////////
  // takeSub[GenIterable[B]](n: Int): Repr[GenIterable[B]]
  //   - Take iterables from this collection until the total number of 
  //     elements in the taken items is about to exceed `n`.  The total number
  //     of elements will be less than or equal to `n`.
  //////////////////////////////////////////////////////

  implicit class Enriched_takeSub_Iterator[A, R <: GenIterable[A]](val self: Iterator[GenIterableLike[A, R]]) {
    /**
     * Take iterables from this collection until the total number of elements
     * in the taken items is about to exceed `n`.  The total number of
     * elements will be less than or equal to `n`.
     *
     * @param n	the maximum number of sub-elements to take
     * @return the new collection
     */
    def takeSub(n: Int): Iterator[R] = {
      if (self.isEmpty) {
        self.asInstanceOf[Iterator[R]]
      }
      else {
        new Iterator[R] {
          private var nextElement: R = self.next.asInstanceOf[R]
          private var total: Int = nextElement.size

          override def hasNext = total <= n

          override def next = {
            if (hasNext) {
              val x = nextElement
              if (self.hasNext) {
                nextElement = self.next.asInstanceOf[R]
                total += nextElement.size
              }
              else
                total = n + 1
              x
            }
            else
              throw new NoSuchElementException("next on empty iterator")
          }
        }
      }
    }
  }

  implicit class Enriched_takeSub_GenTraversableLike[A, R <: GenIterable[A], Repr <: GenTraversable[GenIterable[A]]](val self: GenTraversableLike[GenIterableLike[A, R], Repr]) extends AnyVal {
    /**
     * Take iterables from this collection until the total number of elements
     * in the taken items is about to exceed `n`.  The total number of
     * elements will be less than or equal to `n`.
     *
     * @param n	the maximum number of sub-elements to take
     * @return the new collection
     */
    def takeSub[That](n: Int)(implicit bf: CanBuildFrom[Repr, R, That]): That = {
      val b = bf(self.asInstanceOf[Repr])
      for (x <- self.toIterator.takeSub(n)) b += x
      b.result
    }
  }

  //  implicit class ReversableIterableMap[A, B](val map: Map[A, GenTraversableOnce[B]]) extends AnyVal {
  //    def reverse(): Map[B, GenTraversableOnce[A]] =
  //      map.ungroup.groupBy(_._2).mapValues(_.map(_._1)).iterator.toMap
  //  }
  //
  //  implicit class ReversableMap[A, B](val map: Map[A, B]) extends AnyVal {
  //    def reverse(): Map[B, Iterable[A]] =
  //      map.toIndexedSeq.groupBy(_._2).mapValues(_.map(_._1)).iterator.toMap
  //  }

  /**
   * This Set implementation always returns 'true' from its 'contains' method.
   */
  class UniversalSet[A] extends Set[A] {
    override def contains(key: A): Boolean = true
    override def iterator: Iterator[A] = sys.error("UniversalSet.iterator is not implemented")
    override def +(elem: A): UniversalSet[A] = sys.error("UniversalSet.+ is not implemented")
    override def -(elem: A): UniversalSet[A] = sys.error("UniversalSet.- is not implemented")
    override def toString() = "UniversalSet()"
  }
  object UniversalSet {
    def apply[A]() = new UniversalSet[A]
  }

  //////////////////////////////////////////////////////
  // Conversion (.toX) methods
  //////////////////////////////////////////////////////
  implicit class EnrichedWithToMMap[K, V](val self: TraversableOnce[(K, V)]) extends AnyVal {
    def toMMap =
      if (self.isEmpty) mutable.Map.empty[K, V]
      else mutable.Map.newBuilder[K, V] ++= self result
  }
}
