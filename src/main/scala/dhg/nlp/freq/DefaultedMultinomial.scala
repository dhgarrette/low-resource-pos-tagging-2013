package dhg.nlp.freq

import scala.util.Random
import dhg.util.CollectionUtil._
import breeze.stats.distributions.RandBasis
import breeze.stats.distributions.Rand
import scalaz._
import scalaz.Scalaz._

/**
 * This class stores a map of items and their counts along with a "default"
 * count (for not-yet-seen items) and a "total addition" value that is added
 * to the total count to account for the count mass of any possibly unseen
 * items.
 *
 * Construct a frequency distribution from the counter result.  Calculates
 * the distribution by dividing each count by the total count.
 * P(B) = C(B) / Sum[C(x) for all x].
 *
 * The "totalAddition" portion is added to the total count before
 * dividing.  The "defaultCount" is used as the count for "unseen" items,
 * those items not included in the counts.
 */
case class DefaultedMultinomial[T](
  counts: Map[T, Double], defaultCount: Double = 0.0, totalAddition: Double = 0.0)(
    implicit val rand: RandBasis = Rand)
  extends DiscreteDistribution[T] {

  private[this] lazy val sum = counts.values.sum // sum of counts of all known events 
  private[this] lazy val total = sum + totalAddition // sum of all counts (including unknown events)
  private[this] lazy val isEmpty = total == 0
  private[this] lazy val defaultProb = if (isEmpty) 0.0 else (defaultCount / total)

  def knownKeys = counts.keySet
  
  /** A map of the probabilities (excluding defaults) */
  lazy val probMap = {
    if (isEmpty) Map[T, Double]().withDefaultValue(0.0)
    else counts.mapVals(_ / total)
  }

  lazy val sortedCounts = counts.toVector.sortBy(-_._2)

  override def apply(key: T) = probMap.get(key).getOrElse(defaultProb)
  def get(key: T) = probMap.get(key)
  def getNoDefault(key: T) = probMap.get(key).getOrElse(sys.error(s"key not found: $key"))
  def iterator = probMap.iterator

  override def sample(): T = {
    sampleProb()._1
  }

  def sampleProb(): (T, Double) = {
    assert(!isEmpty, "Cannot sample from a multinomial with no mass.")
    var key = rand.uniform.get * sum
    val itr = sortedCounts.iterator
    while (itr.hasNext) {
      val (item, p) = itr.next()
      key -= p
      if (key <= 0)
        return (item, p / total)
    }
    throw new RuntimeException(s"Could not sample from: ${val s = s"[${sortedCounts.take(50).mkString(", ")}]"; if (s.length <= 50) s else s.take(47) + "..."} ")
  }

  override def toString = "DefaultedMultinomial(%s, %s, %s)".format(counts, defaultCount, totalAddition)
}

object DefaultedMultinomial {
  implicit def DefaultedMultinomialSemigroup[B]: Semigroup[DefaultedMultinomial[B]] =
    new Semigroup[DefaultedMultinomial[B]] {
      def append(f1: DefaultedMultinomial[B], f2CallByName: => DefaultedMultinomial[B]) = {
        val f2 = f2CallByName
        val countSum = f1.counts |+| f2.counts

        if (f1.defaultCount == 0.0 && f1.totalAddition == 0.0)
          new DefaultedMultinomial(countSum, f2.defaultCount, f2.totalAddition)(f2.rand)

        else if (f2.defaultCount == 0.0 && f2.totalAddition == 0.0)
          new DefaultedMultinomial(countSum, f1.defaultCount, f1.totalAddition)(f1.rand)

        else {
          assert(f1.defaultCount == f2.defaultCount)
          assert(f1.totalAddition == f2.totalAddition)
          new DefaultedMultinomial(countSum, f1.defaultCount, f1.totalAddition)(f1.rand)
        }
      }
    }
}
