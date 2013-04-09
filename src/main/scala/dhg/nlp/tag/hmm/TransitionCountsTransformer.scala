package dhg.nlp.tag.hmm

import dhg.nlp.freq.CondCountsTransformer
import dhg.nlp.freq.DefaultedCondFreqCounts
import dhg.nlp.freq.DefaultedMultinomial
import dhg.nlp.freq.PassthroughCondCountsTransformer

class TransitionCountsTransformer[Tag](delegate: CondCountsTransformer[Option[Tag], Option[Tag]])
  extends CondCountsTransformer[Option[Tag], Option[Tag]] {

  override def apply(counts: DefaultedCondFreqCounts[Option[Tag], Option[Tag]]) = {
    DefaultedCondFreqCounts(
      delegate(counts).counts.map {
        case (tag, dfc @ DefaultedMultinomial(c, d, t)) =>
          tag -> (tag match {
            case None => DefaultedMultinomial(c + (None -> 0.0), d, t)
            case _ => dfc
          })
      })
  }

}

object TransitionCountsTransformer {
  def apply[Tag]() = {
    new TransitionCountsTransformer(PassthroughCondCountsTransformer[Option[Tag], Option[Tag]]())
  }
}
