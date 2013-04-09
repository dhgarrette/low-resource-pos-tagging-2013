package dhg.nlp.tag.hmm

import dhg.nlp.freq.CondFreqDist
import dhg.nlp.tag.SupervisedTaggerTrainer
import dhg.util.CollectionUtil._

/**
 * Factory for training a Hidden Markov Model tagger directly from labeled data.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 *
 * @param transitionCountsTransformer	factory for generating builders that count tag occurrences and compute distributions
 * @param emissionCountsTransformer		factory for generating builders that count symbol occurrences and compute distributions
 * @param hmmTaggerFactory				factory for actually creating the HmmTagger
 */
class SupervisedHmmTaggerTrainer[Sym, Tag](
  val transitionCountsTransformer: TransitionCountsTransformer[Tag],
  val emissionCountsTransformer: EmissionCountsTransformer[Tag, Sym],
  val hmmTaggerFactory: HmmTaggerFactory[Sym, Tag])
  extends SupervisedTaggerTrainer[Sym, Tag] {

  /**
   * @inheritdoc
   *
   * Uses transition and emission counters to compute distributions based on labeled data.
   */
  override def makeTagger[N](transitionCounts: Map[Option[Tag], Map[Option[Tag], N]], emissionCounts: Map[Option[Tag], Map[Option[Sym], N]])(implicit num: Numeric[N]) = {
    val transitionDist = CondFreqDist(transitionCountsTransformer(transitionCounts.mapVals(_.mapVals(num.toDouble))))
    val emissionDist = CondFreqDist(emissionCountsTransformer(emissionCounts.mapVals(_.mapVals(num.toDouble))))
    hmmTaggerFactory(transitionDist, emissionDist)
  }
}
