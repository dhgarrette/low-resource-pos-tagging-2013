package dhg.pos.minmodel

import scala.collection.{ Map => CMap }
import scala.collection.{ Set => CSet }
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import WordBigramList.WordBigram
import dhg.nlp.tag.TagUtils._
import dhg.nlp.tag.TagDict.OptionalTagDict
import dhg.nlp.tag.support.Viterbi
import dhg.nlp.tag.OptionalTagDict
import dhg.nlp.util.CollectionUtils._
import dhg.util.CollectionUtil._
import dhg.util.Pattern._
import dhg.pos.minmodel._
import dhg.util.Time.time
import com.typesafe.scalalogging.log4j.Logging

trait ModelMinimizerStage2[Sym, Tag] {
  def apply(
    tagBigramsToWordBigrams: Map[(Option[Tag], Option[Tag]), WordBigramList[Sym, Tag]],
    sentences: Vector[Vector[(Option[Sym], Map[Option[Tag], Option[Double]])]],
    chosen1: Set[(Option[Tag], Option[Tag])],
    paths1: Vector[Option[Vector[Option[Tag]]]]): (Set[(Option[Tag], Option[Tag])], Vector[Option[Vector[Option[Tag]]]])
}

class ModelMinimizerStage2Impl[Sym, Tag](
  tagBigramSelector: TagBigramSelector[Sym, Tag], // probably want EdgeScoreSummingTagBigramSelector(SummingTagEdgeScorer)
  viterbiEdgeScorer: ModelMinTagEdgeScorer[Sym, Tag],
  getHolesOptimization: Boolean = true,
  pctSentToComplete: Int = 100)
  extends ModelMinimizerStage2[Sym, Tag]
  with Logging {

  override def apply(
    tagBigramsToWordBigrams: Map[(Option[Tag], Option[Tag]), WordBigramList[Sym, Tag]],
    sentences: Vector[Vector[(Option[Sym], Map[Option[Tag], Option[Double]])]],
    chosen1: Set[(Option[Tag], Option[Tag])],
    paths1: Vector[Option[Vector[Option[Tag]]]]): (Set[(Option[Tag], Option[Tag])], Vector[Option[Vector[Option[Tag]]]]) = {

    val sentenceLengths = sentences.map(_.size)
    var startsEnds = makeGraph(chosen1, sentences, sentenceLengths).map(Option(_))
    val maxIncompleteSentences = sentences.size * (100 - pctSentToComplete) / 100

    val cand: MMap[(Option[Tag], Option[Tag]), (WordBigramList[Sym, Tag], WordBigramList[Sym, Tag])] = MMap() ++ tagBigramsToWordBigrams.mapVals(wordList => (wordList, wordList)) -- chosen1
    val chosen = MSet() ++ chosen1
    val seenWordTags = MSet[(Option[Sym], Option[Tag])]()
    var choiceAmbiguityTotal = 0
    val numChosenStage1 = chosen.size

    var paths = paths1

    time("phase2", {
      //TODO:      logger.info("  %d chosen: %d sentences remaining, %d candidates remaining".format(chosen.size, paths.count(_.isEmpty), cand.size))
      while (paths.countCompare(_.isEmpty, maxIncompleteSentences) > 0 && cand.nonEmpty) {
        if (getHolesOptimization)
          startsEnds = for ((se, p) <- (startsEnds zipSafe paths)) yield { if (p.isDefined) None else se } //remove completed sentences from startsEnds

        // pick the tag bigram that fills the most holes
        // break ties by minimizing the addition of new tags
        val bigramsAndHoleTuplesBySent = getHoles(sentences, startsEnds, cand, chosen)
        val bigramsAndHoleTuples = bigramsAndHoleTuplesBySent.mapVals(SimpleWordBigramList(_))
        val (chosenTagBigram, newlyCovered, choiceAmbiguity) = tagBigramSelector.select(bigramsAndHoleTuples.mapVals(x => (x, x)), chosen, seenWordTags)
        val newlyCoveredWords = newlyCovered.wordSetMap

        cand -= chosenTagBigram
        chosen += chosenTagBigram
        seenWordTags ++= newlyCoveredWords.values.flatten.mapt((w, t, _) => (w, t))
        choiceAmbiguityTotal += choiceAmbiguity

        for (
          (w1, w2) <- bigramsAndHoleTuplesBySent(chosenTagBigram);
          UMap(sid -> Vector(((_, wid1), (_, tag1, tag1P)), ((_, wid2), (_, tag2, tag2P)))) = Vector(w1, w2).groupBy(_._1._1);
          (sentStarts, sentEnds) <- startsEnds(sid)
        ) {
          assert(tag1 == chosenTagBigram._1)
          assert(tag2 == chosenTagBigram._2)
          sentStarts(wid1) update (tag1, tag1P)
          sentEnds(wid2) update (tag2, tag2P)
        }

        val tagBigramMap = chosen.to[Set].groupByKey
        val viterbi = new ModelMinViterbi(viterbiEdgeScorer, tagBigramMap)

        paths = (sentences zipSafe paths).map {
          case (sentence, None) => viterbi.tagSequence(sentence).map(_.ended)
          case (_, p) => p
        }

        if (chosen.size % 100 == 0)
          logger.info("  %d chosen: %d sentences remaining, %d holes remaining, %d candidates remaining, %.2f choice ambiguity".format(
            chosen.size, paths.count(_.isEmpty), bigramsAndHoleTuples.sumBy(_._2.pairs.size), cand.size, choiceAmbiguityTotal.toDouble / (chosen.size - numChosenStage1)))
      }
      logger.info("  %d chosen: %d sentences remaining, %d holes remaining, %d candidates remaining, %.2f choice ambiguity".format(
        chosen.size, paths.count(_.isEmpty), getHoles(sentences, startsEnds, cand, chosen).sumBy(_._2.size), cand.size, choiceAmbiguityTotal.toDouble / (chosen.size - numChosenStage1)))
    }, logger.info(_))

    (chosen.toSet, paths)
  }

  /**
   * For each word, list tags that start/end bigrams on that word.
   *
   * TODO: Rewrite this
   */
  private def makeGraph(
    chosen: Set[(Option[Tag], Option[Tag])],
    sentences: Vector[Vector[(Option[Sym], Map[Option[Tag], Option[Double]])]],
    sentenceLengths: Iterable[Int]) = { //: Seq[(Vector[Set[Option[Tag]]], Vector[Set[Option[Tag]]])] = {

    val SetNone: MMap[Option[Tag], Option[Double]] = MMap(None -> None)
    val startsEnds =
      (for (sl <- sentenceLengths)
        yield (Vector.fill(sl - 1)(MMap[Option[Tag], Option[Double]]()) :+ SetNone, SetNone +: Vector.fill(sl - 1)(MMap[Option[Tag], Option[Double]]())))
        .toBuffer

    for (
      (words, (sentStarts, sentEnds)) <- sentences zipSafe startsEnds;
      Seq(((word1, tags1), (starts1, _)), ((word2, tags2), (_, ends2))) <- (words zipSafe (sentStarts zipSafe sentEnds)).sliding(2);
      (label1, label1P) <- tags1;
      (label2, label2P) <- tags2
    ) {
      if (chosen.contains(label1, label2)) {
        starts1 update (label1, label1P)
        ends2 update (label2, label2P)
      }
    }

    startsEnds.toVector //.map { case (a, b) => (a.map(_.toSet), b.map(_.toSet)) }.toVector
  }

  /**
   * Find the subset of candidates that would fill holes in the graph.  A hole
   * is a candidate edge that connects an 'end' to a 'start'; ie, it connects
   * two disconnected edges to make a path.
   */
  private def getHoles(
    sentences: Vector[Vector[(Option[Sym], Map[Option[Tag], Option[Double]])]],
    allStartsEnds: Vector[Option[(Vector[CMap[Option[Tag], Option[Double]]], Vector[CMap[Option[Tag], Option[Double]]])]],
    cand: CMap[(Option[Tag], Option[Tag]), (WordBigramList[Sym, Tag], WordBigramList[Sym, Tag])],
    chosen: CSet[(Option[Tag], Option[Tag])]): Map[(Option[Tag], Option[Tag]), Seq[(WordBigram[Sym, Tag], WordBigram[Sym, Tag])]] = {

    val candidateBigrams = cand.keySet.groupByKey

    val tagBigramsToHoles =
      for (
        ((startsEndsOpt, sid), sentence) <- (allStartsEnds.zipWithIndex zipSafe sentences).par;
        (sentStarts, sentEnds) <- startsEndsOpt.toSeq;
        Seq((((_, ends1), (word1, _)), wid1), (((starts2, _), (word2, _)), wid2)) <- ((sentStarts zipSafe sentEnds zipSafe sentence).zipWithIndex).sliding(2); // each word token pair along with the tags bigrams that end on the first word and the tag bigrams that start on the second word
        (label1, label1P) <- ends1;
        candStarts <- candidateBigrams.get(label1).toSeq;
        (label2, label2P) <- starts2 filterKeys candStarts
      ) yield (label1, label2) -> ((sid, wid1) -> (word1, label1, label1P), (sid, wid2) -> (word2, label2, label2P))

    tagBigramsToHoles.seq.groupByKey
  }
}
