package dhg.nlp.freq

import dhg.util.CollectionUtil._
import scalaz._
import Scalaz._
import breeze.stats.distributions.RandBasis
import breeze.stats.distributions.Rand

/**
 * Utilities for frequency distributions: functions to probabilities: P(B).
 */
object FreqDist {

  /**
   * Return an "empty" frequency distribution: a function that maps
   * everything to the zero-probability.  P(B) = 0 for all B.
   */
  def empty[B]: DefaultedMultinomial[B] = static(0.0)

  /**
   * Return a "static" frequency distribution: a function that maps
   * everything to the same probability.  P(B) = v for all B.
   */
  def static[B](v: Double): DefaultedMultinomial[B] = new DefaultedMultinomial[B](Map(), v, 1.0)

}

case class CondFreqDist[A, B](dists: Map[A, DefaultedMultinomial[B]], default: DefaultedMultinomial[B]) extends (A => DefaultedMultinomial[B]) {
  override def apply(a: A) = dists.getOrElse(a, default)
  override def toString = s"CondFreqDist($dists, $default)"
}

/**
 * Utilities for conditional frequency distributions: functions to functions
 * to probabilities: P(B|A).
 */
object CondFreqDist {

  /**
   * Return an "empty" frequency distribution: a function that maps
   * everything to the zero-probability.  P(B|A) = 0 for all A,B.
   */
  def empty[A, B]: A => DefaultedMultinomial[B] = static(0.0)

  /**
   * Return a "static" frequency distribution: a function that maps
   * everything to the same probability.  P(B|A) = v for all A,B.
   */
  def static[A, B](v: Double): A => DefaultedMultinomial[B] = (_: Any) => FreqDist.static(v)

  /**
   * Construct a frequency distribution from the counts. Calculates
   * the distribution for each entry by dividing each count by the total
   * count for that entry.
   * P(B|A) = For each A: C(B|A) / Sum[C(x|A) for all x].
   *
   * Note that if the total for a given 'A' is zero, then
   * that 'A' will map to the empty distribution.
   *
   * @tparam A	the conditioning item being counted; P(B|A).
   * @tparam B	the conditioned item being counted; P(B|A).
   */
  def apply[A, B](counts: Map[A, Map[B, Double]])(
    implicit rand: RandBasis = Rand): CondFreqDist[A, B] = {
    apply(counts.mapVals(DefaultedMultinomial(_)(rand)))
  }

  /**
   * Construct a frequency distribution from the counter result. Calculates
   * the distribution for each entry by dividing each count by the total
   * count for that entry.
   * P(B|A) = For each A: C(B|A) / Sum[C(x|A) for all x].
   * Argument should be from a call to CondFreqDist.resultCounts.
   *
   * The "totalAddition" portions at each level are added to the total
   * counts before dividing.  The grand total includes the additions to each
   * individual 'A' entry.  The "defaultCount" at each level are used as
   * the count for "unseen" items, those items not included in the counts.
   *
   * Note that if the total for a given 'A' (after additions) is zero, then
   * that 'A' will map to the empty distribution.
   *
   * @tparam A	the conditioning item being counted; P(B|A).
   * @tparam B	the conditioned item being counted; P(B|A).
   */
  def apply[A, B](counts: Map[A, DefaultedMultinomial[B]]): CondFreqDist[A, B] = {
    val summedBackoffCounts =
      if (counts.nonEmpty)
        counts.values.reduce { (f1, f2) => DefaultedMultinomial(f1.counts |+| f2.counts, f1.defaultCount + f2.defaultCount, f1.totalAddition + f2.totalAddition)(f1.rand) }
      else
        DefaultedMultinomial(Map[B, Double](), 0, 0)
    CondFreqDist(counts, summedBackoffCounts)
  }

  /**
   * Construct a frequency distribution from the counter result. Calculates
   * the distribution for each entry by dividing each count by the total
   * count for that entry.
   * P(B|A) = For each A: C(B|A) / Sum[C(x|A) for all x].
   * Argument should be from a call to CondFreqDist.resultCounts.
   *
   * The "totalAddition" portions at each level are added to the total
   * counts before dividing.  The grand total includes the additions to each
   * individual 'A' entry.  The "defaultCount" at each level are used as
   * the count for "unseen" items, those items not included in the counts.
   *
   * Note that if the total for a given 'A' (after additions) is zero, then
   * that 'A' will map to the empty distribution.
   *
   * @tparam A	the conditioning item being counted; P(B|A).
   * @tparam B	the conditioned item being counted; P(B|A).
   */
  def apply[A, B](resultCounts: DefaultedCondFreqCounts[A, B]): CondFreqDist[A, B] = {
    val DefaultedCondFreqCounts(counts) = resultCounts
    apply(counts)
  }
}
