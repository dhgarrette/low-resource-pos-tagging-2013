package dhg.util.math

import scala.util.Random

trait RandBasis {
  def uniform: RandBasis = this
  def get: Double
}

class UniformRandSampler() extends RandBasis {
  val rand = new Random
  override def get: Double = rand.nextDouble
}

class SeededUniformRandSampler(seed: Int) extends RandBasis {
  val rand = new Random(seed)
  override def get: Double = rand.nextDouble
}

object Rand extends RandBasis {
  override val uniform = new UniformRandSampler
  override def get = uniform.get
}
