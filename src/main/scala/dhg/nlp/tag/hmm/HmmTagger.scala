package dhg.nlp.tag.hmm

import scala.annotation.tailrec

import dhg.nlp.tag.SimpleTagDict
import dhg.nlp.tag.TagDict.OptionalTagDict
import dhg.nlp.tag.Tagger
import dhg.nlp.tag.hmm.HmmUtils.addDistributionsToRawSequences
import dhg.util.CollectionUtil._
import dhg.util.Pattern._

/**
 * Hidden Markov Model for tagging.
 *
 * @tparam Sym	visible symbols in the sequences
 * @tparam Tag	tags applied to symbols
 *
 * @param transitions		function giving the conditional probability of a tag given its previous tag
 * @param emissions			function giving the probability of a symbol given a tag
 * @param tagSet			the set of all possible tags
 *
 * NOTE: Start and end symbols and tags are represented by None.
 */
case class HmmTagger[Sym, Tag](
  transitions: Option[Tag] => Option[Tag] => Double,
  emissions: Option[Tag] => Option[Sym] => Double,
  tagDict: OptionalTagDict[Sym, Tag],
  validTransitions: Map[Option[Tag], Set[Option[Tag]]],
  fastHmmTagger: FastHmmTagger[Sym, Tag])
  extends Tagger[Sym, Tag] {

  override def tagOptions(rawSequences: Vector[Vector[Sym]]): Vector[Option[Vector[(Sym, Tag)]]] = {
    val newSequences = addDistributionsToRawSequences[Sym, Tag](rawSequences, tagDict, transitions, emissions, validTransitions)
    fastHmmTagger.tagAllFast(newSequences).map(Some(_))
  }

  override def tagSequence(sequence: Vector[Sym]): Option[Vector[(Sym, Tag)]] = {
    val Vector(newSequence) = addDistributionsToRawSequences[Sym, Tag](Vector(sequence), tagDict, transitions, emissions, validTransitions)
    Some(fastHmmTagger.tagSequenceFast(newSequence))
  }
}

object HmmTagger {
  def apply[Sym, Tag](transitions: Option[Tag] => Option[Tag] => Double, emissions: Option[Tag] => Option[Sym] => Double, tagDict: OptionalTagDict[Sym, Tag]) = {
    val allTags = tagDict.allTags + None
    new HmmTagger[Sym, Tag](transitions, emissions, tagDict, allTags.mapToVal(allTags).toMap, new FastHmmTagger[Sym, Tag])
  }
  def apply[Sym, Tag](transitions: Option[Tag] => Option[Tag] => Double, emissions: Option[Tag] => Option[Sym] => Double, tagDict: OptionalTagDict[Sym, Tag], validTransitions: Map[Option[Tag], Set[Option[Tag]]]) = {
    new HmmTagger[Sym, Tag](transitions, emissions, tagDict, validTransitions, new FastHmmTagger[Sym, Tag])
  }
  def apply[Sym, Tag](transitions: Option[Tag] => Option[Tag] => Double, emissions: Option[Tag] => Option[Sym] => Double, tagDict: OptionalTagDict[Sym, Tag], fastHmmTagger: FastHmmTagger[Sym, Tag]) = {
    val allTags = tagDict.allTags + None
    new HmmTagger[Sym, Tag](transitions, emissions, tagDict, allTags.mapToVal(allTags).toMap, fastHmmTagger)
  }
}

class FastHmmTagger[Sym, Tag] {

  type OSym = Option[Sym]
  type OTag = Option[Tag]

  type TokTag = (OTag, (Map[OTag, Double], Double))
  type Tok = (OSym, Vector[TokTag])

  def tagAllFast(sequences: Vector[Vector[Tok]]): Vector[Vector[(Sym, Tag)]] = {
    sequences /* TODO: .par*/ .map(tagSequenceFast) //TODO: .seq
  }

  def tagSequenceFast(sequence: Vector[Tok]): Vector[(Sym, Tag)] = {
    // viterbi(t)(j) = the probability of the most likely subsequence of states 
    // that accounts for the first t observations and ends in state j.

    // Set the initial values for the fold based on the initial observation
    val startViterbi = Vector[(Option[Tag], Double)](None -> 1.0)
    val startBackpointers = List[Map[Option[Tag], Option[Tag]]]()

    // Build up backpointers list by calculating viterbi scores for each subsequent observation
    val backpointers =
      (sequence.drop(1)).foldLeft((startViterbi, startBackpointers)) {
        case ((viterbi, backpointers), (currSym, currTags)) =>
          // for each possible tag, get the highest probability previous tag and its score
          val (nextViterbi, currBackpointers) =
            currTags.map { // each legal tag for the current symbol
              case (currTag, (currTagTransitions, currTagEmission)) =>
                val (bestPrev, bestPrevScore) =
                  viterbi.mapt { (prevTag, viterbiScore) =>
                    (prevTag, viterbiScore * currTagTransitions(prevTag))
                  }.maxBy(_._2)
                (currTag -> (bestPrevScore * currTagEmission), currTag -> bestPrev)
            }.unzip
          (nextViterbi, currBackpointers.toMap :: backpointers)
      }._2

    // Get the optimal tag sequence and map the tag indices back to their string values
    sequence.flatMap(_._1) zipSafe backtrack(backpointers)
  }

  /**
   * Backtrack through the backpointer maps to recover the optimal tag sequence.
   */
  private def backtrack(backpointers: List[Map[OTag, OTag]]): Vector[Tag] = {
    @tailrec def inner(backpointers: List[Map[OTag, OTag]], curTag: OTag, tags: List[Tag]): List[Tag] =
      backpointers match {
        case Nil => assert(curTag == None); tags
        case currPointers :: previousPointers => inner(previousPointers, currPointers(curTag), curTag.get :: tags)
      }
    val UMap(None -> lastTag) :: previousPointers = backpointers
    inner(previousPointers, lastTag, Nil).toVector
  }

}

//////////////////////////////
// HmmTagger Factories
//////////////////////////////

trait HmmTaggerFactory[Sym, Tag] {
  def apply(
    transitions: Option[Tag] => Option[Tag] => Double,
    emissions: Option[Tag] => Option[Sym] => Double): HmmTagger[Sym, Tag]
}

class UnconstrainedHmmTaggerFactory[Sym, Tag](tagSet: Set[Tag]) extends HmmTaggerFactory[Sym, Tag] {
  override def apply(
    transitions: Option[Tag] => Option[Tag] => Double,
    emissions: Option[Tag] => Option[Sym] => Double): HmmTagger[Sym, Tag] = {
    HmmTagger(transitions, emissions, SimpleTagDict(Map(), tagSet).opt)
  }
}

class HardTagDictConstraintHmmTaggerFactory[Sym, Tag](tagDict: OptionalTagDict[Sym, Tag]) extends HmmTaggerFactory[Sym, Tag] {
  override def apply(
    transitions: Option[Tag] => Option[Tag] => Double,
    emissions: Option[Tag] => Option[Sym] => Double): HmmTagger[Sym, Tag] = {
    HmmTagger(transitions, emissions, tagDict)
  }
}
