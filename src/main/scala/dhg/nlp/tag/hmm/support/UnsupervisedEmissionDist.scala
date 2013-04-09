package dhg.nlp.tag.hmm.support

import com.typesafe.scalalogging.log4j.Logging

import dhg.nlp.freq.CondFreqDist
import dhg.nlp.freq.CountsTransformer
import dhg.nlp.freq.DefaultedCondFreqCounts
import dhg.nlp.freq.DefaultedMultinomial
import dhg.nlp.tag.TagDict
import dhg.util.CollectionUtil._
import scalaz._
import scalaz.Scalaz._

/**
 * Produce a conditional frequency distribution without labeled training data.
 */
trait UnsupervisedEmissionDistFactory[Tag, Sym] {
  def apply(rawData: Seq[Seq[Sym]], tagDict: TagDict[Sym, Tag]): Option[Tag] => Option[Sym] => Double
}

/**
 * Count occurrences of each word in the raw data and spread them evenly
 * among the tags to which the word is associated in the tag dictionary.
 *
 * So, if a word w appears C(w) times in the raw corpus, and is seen with
 * |TD(w)| tags in the tag dictionary, then assume
 * for each t in TD(w): C(w,t) = C(w) / |TD(w)|.
 *
 * If a word from the raw corpus does not appear in the tag dictionary, then
 *
 *
 * Known words not associated with the tag in the tag dictionary are given
 * zero counts.  Unseen words are assumed to have a 'default' count of 1 for
 * each tag.
 */
class EstimatedRawCountUnsupervisedEmissionDistFactory[Tag, Sym](
  countsTransformer: CountsTransformer[Sym])
  extends UnsupervisedEmissionDistFactory[Tag, Sym]
  with Logging {

  override def apply(rawData: Seq[Seq[Sym]], tagDict: TagDict[Sym, Tag]): Option[Tag] => Option[Sym] => Double = {
    val DefaultedMultinomial(rawCounts, defaultCount, totalAddition) = countsTransformer(rawData.flatten.counts)

    val rawSymbolCounts = rawCounts.withDefaultValue(defaultCount) // number of times each symbol appears in the raw data
    val tagToSymbolDict = tagDict.setIterator.ungroup.map(_.swap).to[Set].groupByKey // a reversed tag dict; Tag -> Set[Symbol]

    val vocabRaw = rawSymbolCounts.keySet // set of all symbols in raw data
    val vocabKnown = tagDict.symbols // set of all symbols in tag dict (known symbols)
    val vocabUnknown = vocabRaw -- vocabKnown // set of all symbols NOT found in tag dict (unknown symbols)

    val rawKnownCountByWord = rawSymbolCounts.filter(x => vocabKnown(x._1)) // counts of each known type from raw data
    val rawUnkwnCountByWord = rawSymbolCounts.filter(x => vocabUnknown(x._1)) // counts of each unknown type from raw data

    logger.debug("totalRawWordCount = " + rawSymbolCounts.values.sum)
    logger.debug("totalRawWordCount (known)   = " + rawKnownCountByWord.values.sum)
    logger.debug("totalRawWordCount (unknown) = " + rawUnkwnCountByWord.values.sum)

    val knownCounts = // estimated C(w,t) for known symbols
      tagToSymbolDict.mapVals(
        _.mapTo(s =>
          rawSymbolCounts(s) / tagDict.set(s).size.toDouble).toMap)

    val estimatedUnknownProportions = {
      val estUnkProportionsFromRaw = {
        logger.debug("raw proportions =              " + knownCounts.mapValues(_.values.sum).normalizeValues.mapValues("%.3f".format(_)))

        knownCounts
          .mapVals(_.values.sum) // estimated number of unknown tokens for each tag 
          .normalizeValues // estimated count mass for unknowns spread across all unknown tokens
      }

      val estUnkProportionsFromTagDict = {
        logger.debug("tagDict proportions =          " + tagToSymbolDict.mapValues(_.size).normalizeValues.mapValues("%.3f".format(_)))

        val x =
          tagToSymbolDict
            .mapVals(_.size) // number of symbols associated with each tag
            .mapVals(math.pow(_, 2)) // exaggerate the differences
            .normalizeValues

        logger.debug("tagDict (skewed) proportions = " + x.normalizeValues.mapValues("%.3f".format(_)))

        x
      }

      estUnkProportionsFromRaw.keySet.mapTo {
        tag => estUnkProportionsFromTagDict(tag) * estUnkProportionsFromRaw(tag)
      }.normalizeValues.toMap
    }

    logger.debug("combined =                     " + estimatedUnknownProportions.normalizeValues.mapValues("%.3f".format(_)))

    val counts =
      knownCounts.map {
        case (tag, estimatedKnownCounts) =>
          val estimatedUnknownProportion = estimatedUnknownProportions(tag)
          val unknownCounts = rawUnkwnCountByWord.mapVals(_ * estimatedUnknownProportion)
          val totalCounts = estimatedKnownCounts ++ unknownCounts // combine known and unknown estimates
          (tag, DefaultedMultinomial(totalCounts, defaultCount * estimatedUnknownProportion, (totalAddition + defaultCount) * estimatedUnknownProportion)) // default for unseen words in test is one count 
      }

    logger.debug("totalEstWordCount           = " + counts.values.map(_.counts.values.sum).sum)
    val totalEstKnown = counts.values.map(_.counts.filter(x => vocabKnown(x._1)).values.sum).sum; logger.debug("totalEstWordCount (known)   = " + totalEstKnown)
    val totalEstUnkwn = counts.values.map(_.counts.filter(x => vocabUnknown(x._1)).values.sum).sum; logger.debug("totalEstWordCount (unknown) = " + totalEstUnkwn)
    logger.debug("totalEstWordCount (known + unknown) = " + (totalEstKnown + totalEstUnkwn))

    val liftedCounts =
      counts.map {
        case (tag, DefaultedMultinomial(a, b, c)) =>
          (Option(tag), DefaultedMultinomial(a.mapKeys(Option(_)), b, c))
      }
    val startEnd = (none[Tag] -> DefaultedMultinomial(Map(none[Sym] -> 1.0)))

    CondFreqDist(DefaultedCondFreqCounts(liftedCounts + startEnd))
  }
}
