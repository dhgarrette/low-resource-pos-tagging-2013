package dhg.nlp.freq

import dhg.util.CollectionUtil._
import scalaz._
import scalaz.Scalaz._
import breeze.stats.distributions.RandBasis
import breeze.stats.distributions.Rand

/**
 * This class stores a map of items and their counts along with a "default"
 * count (for not-yet-seen items) and a "total addition" value that is added
 * to the total count to account for the count mass of any possibly unseen
 * items.
 *
 * @tparam A	the conditioning item being counted; P(B|A).
 * @tparam B	the conditioned item being counted; P(B|A).
 */
case class DefaultedCondFreqCounts[A, B](counts: Map[A, DefaultedMultinomial[B]]) {
  def simpleCounts = counts.mapVals(_.counts)
}

object DefaultedCondFreqCounts {
  def fromMap[A, B](counts: Map[A, Map[B, Double]])(
    implicit rand: RandBasis = Rand): DefaultedCondFreqCounts[A, B] =
    DefaultedCondFreqCounts(counts.mapVals(c => new DefaultedMultinomial(c)(rand)))

  implicit def defaultedCondFreqCountsSemigroup[A, B]: Semigroup[DefaultedCondFreqCounts[A, B]] =
    new Semigroup[DefaultedCondFreqCounts[A, B]] {
      def append(f1: DefaultedCondFreqCounts[A, B], f2: => DefaultedCondFreqCounts[A, B]) = {
        DefaultedCondFreqCounts(f1.counts |+| f2.counts)
      }
    }
}
