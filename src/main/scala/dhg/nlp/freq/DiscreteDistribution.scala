package dhg.nlp.freq

trait DiscreteDistribution[T] extends (T => Double) {

  def sample(): T

}
