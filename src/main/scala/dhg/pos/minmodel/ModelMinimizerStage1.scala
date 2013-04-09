package dhg.pos.minmodel

import scala.collection.{ Map => CMap }
import scala.collection.{ Set => CSet }
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import dhg.nlp.tag.TagUtils._
import dhg.nlp.tag.TagDict.OptionalTagDict
import dhg.nlp.tag.support.Viterbi
import dhg.nlp.tag.OptionalTagDict
import dhg.nlp.util.CollectionUtils._
import dhg.util.CollectionUtil._
import dhg.util.Time.time
import WordBigramList.WordBigram
import com.typesafe.scalalogging.log4j.Logging

trait ModelMinimizerStage1[Sym, Tag] {
  def apply(
    tagBigramsToWordBigrams: Map[(Option[Tag], Option[Tag]), WordBigramList[Sym, Tag]],
    sentences: Vector[Vector[(Option[Sym], Map[Option[Tag], Option[Double]])]]): (Set[(Option[Tag], Option[Tag])], Vector[Option[Vector[Option[Tag]]]])
}

class ModelMinimizerStage1Impl[Sym, Tag](
  tagBigramSelector: TagBigramSelector[Sym, Tag], // probably want NodeScoreSummingTagBigramSelector
  viterbiEdgeScorer: ModelMinTagEdgeScorer[Sym, Tag],
  labeledData: Vector[Vector[(Sym, Tag)]] = Vector())
  extends ModelMinimizerStage1[Sym, Tag]
  with Logging {

  override def apply(
    tagBigramsToWordBigrams: Map[(Option[Tag], Option[Tag]), WordBigramList[Sym, Tag]],
    sentences: Vector[Vector[(Option[Sym], Map[Option[Tag], Option[Double]])]]): (Set[(Option[Tag], Option[Tag])], Vector[Option[Vector[Option[Tag]]]]) = {

    val numWords = sentences.sumBy(_.size)

    val cand: MMap[(Option[Tag], Option[Tag]), (WordBigramList[Sym, Tag], MutableWordBigramList[Sym, Tag])] = MMap() ++ tagBigramsToWordBigrams.mapVals(wordList => (wordList, new MutableWordBigramList(wordList)))
    val chosen = MSet[(Option[Tag], Option[Tag])]() // tag bigrams selected for use
    val coveredWords = MSet[(Int, Int)]()
    val seenWordTags = MSet[(Option[Sym], Option[Tag])]()
    var choiceAmbiguityTotal = 0

    var paths: Vector[Option[Vector[Option[Tag]]]] =
      Vector.fill(sentences.size)(None) // a tag-path through each sentence.  None until a path is found. 

    logger.info("%d sentences, %d candidate tag bigrams, %d words".format(sentences.size, cand.size, numWords))

    { // Handle the labeled data first
      val labeledDataTagBigrams = labeledData.map(_.map(_._2).ended.sliding2).flatten.toSet
      for (chosenTagBigram <- labeledDataTagBigrams if chosenTagBigram._1 != Some("X") && chosenTagBigram._2 != Some("X")) {
        val newlyCovered = tagBigramsToWordBigrams(chosenTagBigram)
        val newlyCoveredWords = newlyCovered.wordSetMap
        cand -= chosenTagBigram
        cand.foreach { case (_, (_, v)) => v.wordSetMap --= newlyCoveredWords.keys }
        cand.retain { case (_, (_, v)) => v.wordSetMap.nonEmpty }
        chosen += chosenTagBigram
        coveredWords ++= newlyCoveredWords.keys
        seenWordTags ++= newlyCoveredWords.values.flatten.mapt((w, t, _) => (w, t))
      }

      val tagBigramMap = chosen.to[Set].groupByKey
      val viterbi = new ModelMinViterbi(viterbiEdgeScorer, tagBigramMap)
      paths = (sentences zipSafe paths).map {
        case (sentence, None) => viterbi.tagSequence(sentence).map(_.ended)
        case (_, p) => p
      }
    }

    time("Stage1", {
      while (coveredWords.size < numWords) {
        val (chosenTagBigram, newlyCovered, choiceAmbiguity) = tagBigramSelector.select(cand, chosen, seenWordTags)
        val newlyCoveredWords = newlyCovered.wordSetMap

        cand -= chosenTagBigram
        cand.foreach { case (_, (_, v)) => v.wordSetMap --= newlyCoveredWords.keys }
        cand.retain { case (_, (_, v)) => v.wordSetMap.nonEmpty }
        chosen += chosenTagBigram
        coveredWords ++= newlyCoveredWords.keys
        seenWordTags ++= newlyCoveredWords.values.flatten.mapt((w, t, _) => (w, t))
        choiceAmbiguityTotal += choiceAmbiguity

        val tagBigramMap = chosen.to[Set].groupByKey
        val viterbi = new ModelMinViterbi(viterbiEdgeScorer, tagBigramMap)

        paths = (sentences zipSafe paths).map {
          case (sentence, None) => viterbi.tagSequence(sentence).map(_.ended)
          case (_, p) => p
        }

        if (chosen.size % 100 == 0)
          logger.info("  %d chosen: %d sentences remaining, %d words remaining, %d candidates remaining, %.2f choice ambiguity".format(
            chosen.size, paths.count(_.isEmpty), numWords - coveredWords.size, cand.size, choiceAmbiguityTotal.toDouble / chosen.size))
      }
      logger.info("  %d chosen: %d sentences remaining, %.2f choice ambiguity".format(
        chosen.size, paths.count(_.isEmpty), choiceAmbiguityTotal.toDouble / chosen.size))
    }, logger.info(_))

    (chosen.toSet, paths)
  }

  class MutableWordBigramList[Sym, Tag](original: WordBigramList[Sym, Tag]) extends WordBigramList[Sym, Tag] {
    override lazy val pairs = sys.error("...")
    override lazy val wordSetMap: MMap[(Int, Int), MSet[(Option[Sym], Option[Tag], Option[Double])]] =
      MMap() ++ original.wordSetMap.mapVals(MSet() ++ _)
  }
}
