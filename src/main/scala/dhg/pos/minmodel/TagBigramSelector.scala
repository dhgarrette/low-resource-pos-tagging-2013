package dhg.pos.minmodel

import dhg.util.CollectionUtil._
import dhg.nlp.util.CollectionUtils._
import dhg.util.Pattern._
import scala.collection.{ Map => CMap }
import scala.collection.{ Set => CSet }
import scala.collection.parallel.immutable.ParMap
import scala.collection.GenMap
import com.typesafe.scalalogging.log4j.Logging

trait TagBigramSelector[Sym, Tag] extends Logging {

  /**
   * @param tagBigramsToWordBigrams	full and complete mapping from all tag
   *     bigrams in the graph to word bigrams
   * @param cand	set of candidate tag bigrams from which the next bigram
   *     should be selected (along with their corresponding word bigrams, for
   *     fast lookup)
   * @param chosen	set of chosen tag bigrams
   */
  final def select(
    cand: CMap[(Option[Tag], Option[Tag]), (WordBigramList[Sym, Tag], WordBigramList[Sym, Tag])],
    chosen: CSet[(Option[Tag], Option[Tag])],
    seenWordTags: CSet[(Option[Sym], Option[Tag])]): ((Option[Tag], Option[Tag]), WordBigramList[Sym, Tag], Int) = {

    val (best, score) = doSelectTagBigram(cand, chosen, seenWordTags)

    val (tagBigram, (_, newlyCovered)) = best.head // pick randomly from those that are equally the best

    if (logger.underlying.isDebugEnabled) {
      logger.debug("        chose: %s (%s): covers %d: %s".format(tagBigram, score, newlyCovered.wordSetMap.keySet.size,
        "" /*tagBigramsToWordBigrams(bigram).map { case Seq((_, (w1, _)), (_, (w2, _))) => (w1, w2) }.toSet.toVector.sorted.take(5)*/ ))
    }

    (tagBigram, newlyCovered, best.size)
  }

  def doSelectTagBigram(
    cand: CMap[(Option[Tag], Option[Tag]), (WordBigramList[Sym, Tag], WordBigramList[Sym, Tag])],
    chosen: CSet[(Option[Tag], Option[Tag])],
    seenWordTags: CSet[(Option[Sym], Option[Tag])]): (GenMap[(Option[Tag], Option[Tag]), (WordBigramList[Sym, Tag], WordBigramList[Sym, Tag])], Double)
}

/**
 * Simply return the set of candidates.
 */
class PassthroughTagBigramSelector[Sym, Tag] extends TagBigramSelector[Sym, Tag] {
  override def doSelectTagBigram(
    cand: CMap[(Option[Tag], Option[Tag]), (WordBigramList[Sym, Tag], WordBigramList[Sym, Tag])],
    chosen: CSet[(Option[Tag], Option[Tag])],
    seenWordTags: CSet[(Option[Sym], Option[Tag])]) = {

    (cand.toMap /* TODO: .par */ , 0.0)

  }
}

/**
 * Maximize the number of word tokens covered.
 */
class CoverageMaximizingTagBigramSelector[Sym, Tag](delegate: TagBigramSelector[Sym, Tag]) extends TagBigramSelector[Sym, Tag] {
  override def doSelectTagBigram(
    cand: CMap[(Option[Tag], Option[Tag]), (WordBigramList[Sym, Tag], WordBigramList[Sym, Tag])],
    chosen: CSet[(Option[Tag], Option[Tag])],
    seenWordTags: CSet[(Option[Sym], Option[Tag])]) = {

    // maximize coverage
    val (score, best) =
      delegate.doSelectTagBigram(cand, chosen, seenWordTags)._1
        .groupBy { case (b, (_, wordList)) => wordList.wordSetMap.keySet.size }
        .maxBy(_._1)

    if (logger.underlying.isDebugEnabled && best.size > 1) {
      logger.debug("    %d choices maximize coverage: %s".format(best.size, "" /*best.toVector.map(_._1).sorted.take(10)*/ ))
      //for (((a, _), b) <- best1.mapTo { case (_, words) => (words.map(_._2) -- wordTagsSeen) })
      //  logger.debug("                %s introduces %d: %s".format(a, b.size, b))
    }

    (best, score)
  }
}

/**
 * Minimize the number of new tags added by the choice.
 * It will add 0, 1, or 2 new tags.
 */
class NewTagMinimizingTagBigramSelector[Sym, Tag](delegate: TagBigramSelector[Sym, Tag]) extends TagBigramSelector[Sym, Tag] {
  override def doSelectTagBigram(
    cand: CMap[(Option[Tag], Option[Tag]), (WordBigramList[Sym, Tag], WordBigramList[Sym, Tag])],
    chosen: CSet[(Option[Tag], Option[Tag])],
    seenWordTags: CSet[(Option[Sym], Option[Tag])]) = {

    val chosenTags = chosen.flatMap { case (a, b) => Set(a, b) }.toSet

    // minimize introduction of new tags
    val (score, best) =
      delegate.doSelectTagBigram(cand, chosen, seenWordTags)._1
        .groupBy { case ((t1, t2), _) => (Set(t1, t2) -- chosenTags).size }
        .minBy(_._1)

    if (logger.underlying.isDebugEnabled && best.size > 1) {
      logger.debug("    %d choices minimize new tags: %s".format(best.size, "" /*best.toVector.map(_._1).sorted.take(10)*/ ))
    }

    (best, score)
  }
}

/**
 * Minimize the number of new word/tag pairs added by the choice.
 */
class NewWordTagPairMinimizingTagBigramSelector[Sym, Tag](delegate: TagBigramSelector[Sym, Tag]) extends TagBigramSelector[Sym, Tag] {
  override def doSelectTagBigram(
    cand: CMap[(Option[Tag], Option[Tag]), (WordBigramList[Sym, Tag], WordBigramList[Sym, Tag])],
    chosen: CSet[(Option[Tag], Option[Tag])],
    seenWordTags: CSet[(Option[Sym], Option[Tag])]) = {

    // minimize introduction of new word/tag pairs
    val (score, best) =
      delegate.doSelectTagBigram(cand, chosen, seenWordTags)._1
        .groupBy {
          case (b, (fullWordList, remainingWordList)) =>
            val wordTagsFromB = fullWordList.wordSetMap.iterator.flatMap(_._2).toSet
            (wordTagsFromB.mapt((w, t, _) => (w, t)) -- seenWordTags).size
        }.minBy(_._1)

    if (logger.underlying.isDebugEnabled && best.size > 1) {
      logger.debug("    %d choices minimize new word/tag: %s".format(best.size, "" /*best.toVector.map(_._1).sorted.take(10)*/ ))
      //for (((a, _), b) <- best2.mapTo { case ((t1, t2), _) => (Set(t1, t2) -- chosenTags) })
      //  logger.debug("                %s introduces %d: %s".format(a, b.size, b))
    }

    (best, score)
  }
}

/**
 * The score of a tag bigram is the sum of the scores of all of its instances.
 */
class EdgeScoreSummingTagBigramSelector[Sym, Tag](
  tagEdgeScorer: ModelMinTagEdgeScorer[Sym, Tag], // probably want SummingTagEdgeScorer
  delegate: TagBigramSelector[Sym, Tag])
  extends TagBigramSelector[Sym, Tag] {

  override def doSelectTagBigram(
    cand: CMap[(Option[Tag], Option[Tag]), (WordBigramList[Sym, Tag], WordBigramList[Sym, Tag])],
    chosen: CSet[(Option[Tag], Option[Tag])],
    seenWordTags: CSet[(Option[Sym], Option[Tag])]) = {

    val fromDelegate = delegate.doSelectTagBigram(cand, chosen, seenWordTags)._1
    val grouped = fromDelegate.groupBy { case (b, (fullWordList, remainingWordList)) => scoreWordList(remainingWordList) }
    val (score, best) = grouped.maxBy(_._1)

    if (logger.underlying.isDebugEnabled && best.size > 1) {
      logger.debug("    %d choices maximize edge score: %s".format(best.size, "" /*best.toVector.map(_._1).sorted.take(10)*/ ))
      //for (((a, _), b) <- best2.mapTo { case ((t1, t2), _) => (Set(t1, t2) -- chosenTags) })
      //  logger.debug("                %s introduces %d: %s".format(a, b.size, b))
    }

    (best, score)
  }

  protected def scoreWordList(wordList: WordBigramList[Sym, Tag]): Double = {
    wordList.pairs.map {
      case ((_, (prevSym, prevTag, prevTagP)), (_, (currSym, currTag, currTagP))) =>
        tagEdgeScorer.apply(prevSym, prevTag, prevTagP, currSym, currTag, currTagP)
    }.sum
  }
}

class HackyEdgeScoreSummingTagBigramSelector[Sym, Tag](
  tagEdgeScorer: ModelMinTagEdgeScorer[Sym, Tag], // probably want SummingTagEdgeScorer
  divisorScaleFactor: Double = 1.0,
  delegate: TagBigramSelector[Sym, Tag])
  extends TagBigramSelector[Sym, Tag] {

  override def doSelectTagBigram(
    cand: CMap[(Option[Tag], Option[Tag]), (WordBigramList[Sym, Tag], WordBigramList[Sym, Tag])],
    chosen: CSet[(Option[Tag], Option[Tag])],
    seenWordTags: CSet[(Option[Sym], Option[Tag])]) = {

    val fromDelegate = delegate.doSelectTagBigram(cand, chosen, seenWordTags)._1
    val grouped = fromDelegate.groupBy {
      case (b, (fullWordList, remainingWordList)) =>
        val score = scoreWordList(remainingWordList)
        val wordTagsFromB = fullWordList.wordSetMap.iterator.flatMap(_._2).toSet
        val newWordTagPairCount = (wordTagsFromB.mapt((w, t, _) => (w, t)) -- seenWordTags).size
        (score / ((newWordTagPairCount * divisorScaleFactor) + 1), score)
    }
    val ((score, totalScore), best) = grouped.maxBy(_._1)

    if (logger.underlying.isDebugEnabled && best.size > 1) {
      logger.debug("    %d choices maximize hacky edge score: %s".format(best.size, "" /*best.toVector.map(_._1).sorted.take(10)*/ ))
      //for (((a, _), b) <- best2.mapTo { case ((t1, t2), _) => (Set(t1, t2) -- chosenTags) })
      //  logger.debug("                %s introduces %d: %s".format(a, b.size, b))
    }

    (best, score)
  }

  protected def scoreWordList(wordList: WordBigramList[Sym, Tag]): Double = {
    wordList.pairs.map {
      case ((_, (prevSym, prevTag, prevTagP)), (_, (currSym, currTag, currTagP))) =>
        tagEdgeScorer.apply(prevSym, prevTag, prevTagP, currSym, currTag, currTagP)
    }.sum
  }
}

/**
 * The score of a tag bigram is the sum of the scores of all of its instances.
 */
class NodeScoreSummingTagBigramSelector[Sym, Tag](
  doubleCoveredWordResolver: DoubleCoveredWordResolver,
  delegate: TagBigramSelector[Sym, Tag],
  noneWeight: Double = 1.0) extends TagBigramSelector[Sym, Tag] {

  override def doSelectTagBigram(
    cand: CMap[(Option[Tag], Option[Tag]), (WordBigramList[Sym, Tag], WordBigramList[Sym, Tag])],
    chosen: CSet[(Option[Tag], Option[Tag])],
    seenWordTags: CSet[(Option[Sym], Option[Tag])]) = {

    def scoreWordList(wordList: WordBigramList[Sym, Tag]): Double =
      wordList.wordSetMap.values
        .submap(_._3.getOrElse(noneWeight))
        .map {
          case USet(a) => a
          case USet(a, b) => doubleCoveredWordResolver(a, b)
        }.sum

    val fromDelegate = delegate.doSelectTagBigram(cand, chosen, seenWordTags)._1
    // TODO: Tag bigrams in 'cand' are only associated with uncovered words.
    //       Do we need to explore other alternatives?  For example, using
    //       the word lists from 'tagBigramsToWordBigrams'?
    val grouped = fromDelegate.groupBy { case (b, (fullWordList, remainingWordList)) => scoreWordList(remainingWordList) }
    val (score, best) = grouped.maxBy(_._1)

    if (logger.underlying.isDebugEnabled && best.size > 1) {
      logger.debug("    %d choices maximize node score: %s".format(best.size, "" /*best.toVector.map(_._1).sorted.take(10)*/ ))
      //for (((a, _), b) <- best2.mapTo { case ((t1, t2), _) => (Set(t1, t2) -- chosenTags) })
      //  logger.debug("                %s introduces %d: %s".format(a, b.size, b))
    }

    (best, score)
  }
}

class HackyNodeScoreSummingTagBigramSelector[Sym, Tag](
  doubleCoveredWordResolver: DoubleCoveredWordResolver,
  divisorScaleFactor: Double = 1.0,
  delegate: TagBigramSelector[Sym, Tag],
  noneWeight: Double = 1.0) extends TagBigramSelector[Sym, Tag] {

  override def doSelectTagBigram(
    cand: CMap[(Option[Tag], Option[Tag]), (WordBigramList[Sym, Tag], WordBigramList[Sym, Tag])],
    chosen: CSet[(Option[Tag], Option[Tag])],
    seenWordTags: CSet[(Option[Sym], Option[Tag])]) = {

    def scoreWordList(wordList: WordBigramList[Sym, Tag]): Double =
      wordList.wordSetMap.values
        .submap(_._3.getOrElse(noneWeight))
        .map {
          case USet(a) => a
          case USet(a, b) => doubleCoveredWordResolver(a, b)
        }.sum

    val fromDelegate = delegate.doSelectTagBigram(cand, chosen, seenWordTags)._1
    // TODO: Tag bigrams in 'cand' are only associated with uncovered words.
    //       Do we need to explore other alternatives?  For example, using
    //       the word lists from 'tagBigramsToWordBigrams'?
    val grouped = fromDelegate.groupBy {
      case (b, (fullWordList, remainingWordList)) =>
        val score = scoreWordList(remainingWordList)
        val wordTagsFromB = fullWordList.wordSetMap.iterator.flatMap(_._2).toSet
        val newWordTagPairCount = (wordTagsFromB.mapt((w, t, _) => (w, t)) -- seenWordTags).size
        (score / ((newWordTagPairCount * divisorScaleFactor) + 1), score)
    }
    val ((score, totalScore), best) = grouped.maxBy(_._1)

    //    if (logger.underlying.isDebugEnabled) {
    //      grouped.seq.toSeq.sortBy(_._1).reverse.flatMap { case (score, bigrams) => score.toString +: bigrams.map("  " + _).toSeq :+ "" } :+ "" foreach logger.debug
    //    }

    //logger.info("totalScore=%.2f, newWordTagPairCount=%.1f, score=%.2f; chosen=%s; %s%s".format(
    //  totalScore.toDouble, (totalScore / score).toDouble - 1, score.toDouble,
    //  best.head._1,
    //  if (best.size > 1) "tied=%s; ".format(best.size) else "",
    //  grouped.toVector.map(_._1).sorted.reverse.drop(1).take(5).map { case (a, b) => "(%.2f,%.2f)".format(a.toDouble, b.toDouble) }))

    if (logger.underlying.isDebugEnabled && best.size > 1) {
      logger.debug("    %d choices maximize hacky node score: %s".format(best.size, "" /*best.toVector.map(_._1).sorted.take(10)*/ ))
      //for (((a, _), b) <- best2.mapTo { case ((t1, t2), _) => (Set(t1, t2) -- chosenTags) })
      //  logger.debug("                %s introduces %d: %s".format(a, b.size, b))
    }

    (best, score)
  }
}

trait DoubleCoveredWordResolver {
  def apply(a: Double, b: Double): Double
}

class SummingDoubleCoveredWordResolver extends DoubleCoveredWordResolver {
  override def apply(a: Double, b: Double): Double = a + b
}

class MaxingDoubleCoveredWordResolver extends DoubleCoveredWordResolver {
  override def apply(a: Double, b: Double): Double = Set(a, b).max
}

class HalvingDoubleCoveredWordResolver(delegate: DoubleCoveredWordResolver) extends DoubleCoveredWordResolver {
  override def apply(a: Double, b: Double): Double = delegate(a, b) / 2
}
