package dhg.nlp.freq

import dhg.util.CollectionUtil._
import breeze.stats.distributions.RandBasis
import breeze.stats.distributions.Rand

class BinomialFreqDist[T](label0: T, label1: T, firstProb: Double)(
  implicit rand: RandBasis = Rand)
  extends DiscreteDistribution[T] {

  val dist = Map(label0 -> firstProb, label1 -> (1 - firstProb))
  override def apply(key: T) = dist.getOrElse(key, 0.0)

  override def sample(): T = {
    if (rand.uniform.get < firstProb)
      label0
    else
      label1
  }

  override def toString = "BinomialFreqDist(%s, %s, %s)".format(label0, label1, firstProb)
}

object BinomialFreqDist {
  def apply[T](label0: T, label1: T, firstProb: Double)(
    implicit rand: RandBasis = Rand) = {
    new BinomialFreqDist(label0, label1, firstProb)(rand)
  }

  def apply[T](labels: Seq[T], firstProb: Double)(
    implicit rand: RandBasis = Rand) = {
    require(labels.size == 2, "BinomialFreqDist must have exactly two labels")
    val Seq(l0, l1) = labels
    new BinomialFreqDist(l0, l1, firstProb)(rand)
  }
}

object BooleanFreqDist {
  def apply(propTrue: Double): BinomialFreqDist[Boolean] = BinomialFreqDist(true, false, propTrue)
}
