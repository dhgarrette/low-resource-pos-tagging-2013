package dhg.nlp.freq

import breeze.linalg.Counter
import breeze.stats.distributions.Dirichlet
import breeze.stats.distributions.Rand
import breeze.stats.distributions.RandBasis
import dhg.util.CollectionUtil._
import breeze.linalg.DenseVector
import scala.collection.breakOut

/**
 * This class allows a Multinomial to be sampled from a Dirichlet distribution.
 *
 * Note that even though the Dirichlet can be seeded by a Multinomial whose
 * underlying counts are arbitrarily large, the resulting sampled Multinomial
 * will have counts that sum to one.
 */
class DirichletSampler[T](dirichlet: Dirichlet[Counter[T, Double], T]) {
  def sample = DefaultedMultinomial(sampleMap)
  def sampleMap = dirichlet.sample.toMap
}

object DirichletSampler {

  def apply[T](labels: Iterable[T], pseudocounts: T => Double)(implicit rand: RandBasis = Rand): DirichletSampler[T] = {
    new DirichletSampler(new Dirichlet(Counter(labels.mapTo(pseudocounts)))(implicitly, rand, implicitly))
  }

  def apply[T](pseudocounts: Map[T, Double])(implicit rand: RandBasis = Rand): DirichletSampler[T] = {
    new DirichletSampler(new Dirichlet(Counter(pseudocounts))(implicitly, rand, implicitly))
  }

  def apply[T](multinomial: DefaultedMultinomial[T])(implicit rand: RandBasis = Rand): DirichletSampler[T] = {
    DirichletSampler(multinomial.counts)(rand)
  }

}

/**
 * This class allows a Multinomial to be sampled from a Dirichlet distribution.
 *
 * Note that even though the Dirichlet can be seeded by a Multinomial whose
 * underlying counts are arbitrarily large, the resulting sampled Multinomial
 * will have counts that sum to one.
 */
class UglyDirichletSampler[T](labels: IndexedSeq[T], dirichlet: Dirichlet[DenseVector[Double], Int]) {
  def sample = DefaultedMultinomial(sampleMap)
  def sampleMap: Map[T, Double] = (labels zipSafe dirichlet.sample.valuesIterator)(breakOut)
}

object UglyDirichletSampler {

  def apply[T](labels: IndexedSeq[T], pseudocounts: T => Double)(implicit rand: RandBasis = Rand): UglyDirichletSampler[T] = {
    val arr = labels.map(pseudocounts).toArray
    new UglyDirichletSampler(labels, new Dirichlet(DenseVector(arr)))
  }

  def apply[T](pseudocounts: Map[T, Double])(implicit rand: RandBasis = Rand): UglyDirichletSampler[T] = {
    val (labels, values) = pseudocounts.toVector.unzip
    val arr = values.toArray
    new UglyDirichletSampler(labels, new Dirichlet(DenseVector(arr)))
  }

  def apply[T](multinomial: DefaultedMultinomial[T])(implicit rand: RandBasis = Rand): UglyDirichletSampler[T] = {
    UglyDirichletSampler(multinomial.counts)(rand)
  }

}
