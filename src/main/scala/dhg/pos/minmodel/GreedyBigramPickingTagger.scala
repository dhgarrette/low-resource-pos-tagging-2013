package dhg.pos.minmodel

import scala.collection.{ Map => CMap }
import scala.collection.{ Set => CSet }
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import WordBigramList.WordBigram
import dhg.nlp.tag.TagUtils._
import dhg.nlp.tag.TagDict.OptionalTagDict
import dhg.nlp.tag.support.Viterbi
import dhg.nlp.tag.TagDict
import dhg.nlp.tag.WeightedTagDict
import dhg.nlp.util.CollectionUtils._
import dhg.util.CollectionUtil._
import dhg.util.Pattern
import dhg.util.Time.time
import dhg.nlp.tag.Tagger
import dhg.nlp.tag.OptionalTagDict

/**
 * Greedily search for a minimal set of tags that spans every sentence.
 */
class GreedyBigramPickingTagger[Sym, Tag](
  stage1: ModelMinimizerStage1[Sym, Tag],
  stage2: ModelMinimizerStage2[Sym, Tag],
  viterbiEdgeScorer: ModelMinTagEdgeScorer[Sym, Tag], // probably want CurrentWeightTagEdgeScorer
  findPathAtEnd: Boolean = false)
  extends ModelMinimizingTagger[Sym, Tag] {

  type Bigrams[Tag] = Map[Option[Tag], Set[Option[Tag]]]

  /**
   * Find the minimal set of tag bigrams across the sentences with a starting point for the set of chosen tags.
   */
  override protected def tags(rawTokenTagDists: Vector[Vector[(Sym, Map[Tag, Double])]]): Vector[Option[Vector[Tag]]] = {
    val end = ((None: Option[Sym]), Map((None: Option[Tag]) -> (None: Option[Double])))
    val newSentences: Vector[Vector[(Option[Sym], Map[Option[Tag], Option[Double]])]] =
      rawTokenTagDists.map(s => end +: s.map { case (word, tags) => (Option(word), tags.mapt((tag, p) => Option(tag) -> Option(p))) } :+ end)

    val tagBigramsToWordBigrams = allCoverSubsets(newSentences) // mapping from tag bigrams to a set of words covered
    val (chosen1, paths1) = stage1.apply(tagBigramsToWordBigrams, newSentences)
    val (chosen2, paths2) = stage2.apply(tagBigramsToWordBigrams, newSentences, chosen1, paths1)

    val paths3 =
      if (findPathAtEnd)
        Vector.fill(newSentences.size)(None)
      else
        paths2

    val tagBigramMap = chosen2.to[Set].groupByKey
    val viterbi = new ModelMinViterbi(viterbiEdgeScorer, tagBigramMap)

    // Find any remaining paths
    val paths =
      (newSentences zipSafe paths3).map {
        case (sentence, None) => viterbi.tagSequence(sentence)
        case (sentence, Some(path)) => Some(path.flatten)
      }

    paths
  }

  /**
   * Map tag bigrams to the word bigrams they cover.  Word bigrams are
   * represented as a list of size two.  Words are represented as a
   * pair of (sentence id, word id) and a word/tag/weight triple.
   */
  private def allCoverSubsets(newSentences: Vector[Vector[(Option[Sym], Map[Option[Tag], Option[Double]])]]) = {
    val tagsBigramsWordIndexPairs =
      for (
        (words, sid) <- newSentences.zipWithIndex;
        Seq(((word1, tags1), wid1), ((word2, tags2), wid2)) <- words.zipWithIndex.sliding(2);
        (label1, label1P) <- tags1;
        (label2, label2P) <- tags2
      ) yield (label1, label2) -> ((sid, wid1) -> (word1, label1, label1P), (sid, wid2) -> (word2, label2, label2P))
    val grouped = tagsBigramsWordIndexPairs.groupByKey
    grouped.mapVals(SimpleWordBigramList(_))
  }

}

trait WordBigramList[Sym, Tag] {
  def pairs: Iterable[(WordBigram[Sym, Tag], WordBigram[Sym, Tag])]
  def wordSetMap: CMap[(Int, Int), CSet[(Option[Sym], Option[Tag], Option[Double])]]
}

object WordBigramList {
  type WordBigram[Sym, Tag] = ((Int, Int), (Option[Sym], Option[Tag], Option[Double]))
}

case class SimpleWordBigramList[Sym, Tag](override val pairs: Iterable[(WordBigram[Sym, Tag], WordBigram[Sym, Tag])]) extends WordBigramList[Sym, Tag] {
  override lazy val wordSetMap: Map[(Int, Int), Set[(Option[Sym], Option[Tag], Option[Double])]] = pairs.flatMap { case (a, b) => Set(a, b) }.to[Set].groupByKey
}
