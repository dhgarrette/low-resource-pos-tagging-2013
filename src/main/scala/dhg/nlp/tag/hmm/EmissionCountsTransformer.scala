package dhg.nlp.tag.hmm

import dhg.nlp.freq.CondCountsTransformer
import dhg.nlp.freq.ConstrainingCondCountsTransformer
import dhg.nlp.freq.DefaultedCondFreqCounts
import dhg.nlp.freq.DefaultedMultinomial
import dhg.nlp.freq.PassthroughCondCountsTransformer
import dhg.nlp.tag.OptionalTagDict
import dhg.nlp.tag.TagDict
import dhg.util.CollectionUtil._

/**
 *
 */
class EmissionCountsTransformer[Tag, Sym](delegate: CondCountsTransformer[Option[Tag], Option[Sym]])
  extends CondCountsTransformer[Option[Tag], Option[Sym]] {

  override def apply(counts: DefaultedCondFreqCounts[Option[Tag], Option[Sym]]) = {
    DefaultedCondFreqCounts(
      delegate(counts).counts.map {
        case (tag, DefaultedMultinomial(c, d, t)) =>
          tag -> (tag match {
            case None => DefaultedMultinomial(Map((None: Option[Sym]) -> 1.0), 0.0, 0.0)
            case _ => DefaultedMultinomial(c + (None -> 0.0), d, t)
          })
      })
  }

}

object EmissionCountsTransformer {
  def apply[Tag, Sym]() = {
    new EmissionCountsTransformer(PassthroughCondCountsTransformer[Option[Tag], Option[Sym]]())
  }
}

/**
 * TODO: This is perhaps too constraining.  It limits the available emissions
 * to that based on the tag dictionary, but, unfortunately, that means that
 * words _not_ in the tag dictionary are impossible.  We need an option for
 * allowing the 'default' counts to not be zeroed.
 */
object TagDictConstrainedEmissionCountsTransformer {
  //  def apply[Tag, Sym](tagDict: TagDict[Sym, Tag], allowUnseenWordTypes: Boolean): EmissionCountsTransformer[Tag, Sym] = {
  //    TagDictConstrainedEmissionCountsTransformer(tagDict, !allowUnseenWordTypes,
  //      PassthroughCondCountsTransformer[Option[Tag], Option[Sym]]())
  //  }

  def apply[Tag, Sym](tagDict: TagDict[Sym, Tag], delegate: CondCountsTransformer[Option[Tag], Option[Sym]]): EmissionCountsTransformer[Tag, Sym] = {
    val c = (OptionalTagDict(tagDict).setIterator.ungroup.map(_.swap) :+ (None, None)).to[Set].groupByKey
    new EmissionCountsTransformer(
      new ConstrainingCondCountsTransformer(c, false,
        delegate))
  }
}
