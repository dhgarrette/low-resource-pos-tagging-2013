package dhg.pos.minmodel

import scala.annotation.tailrec
import scala.collection.breakOut
import dhg.nlp.tag.OptionalTagDict
import dhg.nlp.tag.SimpleTagDict
import dhg.nlp.tag.TagDict._
import dhg.nlp.tag.Tagger
import dhg.nlp.util.CollectionUtils._
import dhg.util.CollectionUtil._
import dhg.util.Pattern._

/**
 * A generic implementation of the Viterbi algorithm for finding the most
 * likely tagging for the sequence.
 *
 * @param edgeScorer		class for calculating the probability of a symbol/tag transition
 * @param tagDict			tag dictionary indicating which words can be used with which tags
 * @param tagTransitions	valid tag-tag transitions
 */
class ModelMinViterbi[Sym, Tag](
  edgeScorer: ModelMinTagEdgeScorer[Sym, Tag],
  tagTransitions: Map[Option[Tag], Set[Option[Tag]]]) {

  /**
   * Find the most likely tagging for the sequence.
   *
   * @param sequence		sequence to be tagged
   */
  def tagSequence(sequence: Vector[(Option[Sym], Map[Option[Tag], Option[Double]])]): Option[Vector[Tag]] = {
    // viterbi(t)(j) = the probability of the most likely subsequence of states 
    // that accounts for the first t observations and ends in state j.

    // Set the initial values for the fold based on the initial observation
    val startViterbi = Vector[((Option[Tag], Option[Double]), Double)]((None, None) -> 1.0)
    val startBackpointers = List[Map[Option[Tag], Option[Tag]]]()
    val startSymbol: (Option[Sym], Map[Option[Tag], Option[Double]]) = (None, Map(None -> None))

    // Build up backpointers list by calculating viterbi scores for each subsequent observation
    val backpointers =
      (sequence.drop(1)).foldLeft(Option(startViterbi, startBackpointers, startSymbol)) {
        case (Some((viterbi, backpointers, (prevSym, prevTags))), (currSym, currTags)) =>
          // for each possible tag, get the highest probability previous tag and its score
          val transitionScores: Vector[((Option[Tag], Option[Double]), Vector[(Option[Tag], Double)])] =
            currTags.map {
              case (currTag, currTagP) => // each legal tag for the current symbol
                (currTag, currTagP) ->
                  viterbi.collect {
                    case ((prevTag, prevTagP), viterbtiScore) if tagTransitions.getOrElse(prevTag, Set())(currTag) => // if the transition is valid
                      (prevTag, viterbtiScore * edgeScorer(prevSym, prevTag, prevTagP, currSym, currTag, currTagP))
                  }
            }(breakOut)
          val bestTransitions =
            transitionScores
              .filter(_._2.nonEmpty) // remove tags that don't transition anywhere
              .mapVals(_.maxBy(_._2)) // get the previous tag with the highest probability (and its score)
          if (bestTransitions.nonEmpty)
            Some((
              bestTransitions.mapVals(_._2), // update viterbi for the next row
              bestTransitions.mapt((prev, curr) => prev._1 -> curr._1).toMap :: backpointers, // append new backpointers
              (currSym, currTags)))
          else
            None
        case (None, _) => None
      }.map { case (_, backpointers, _) => backpointers }

    // Get the optimal tag sequence and map the tag indices back to their string values
    backpointers.map(bp => backtrack(bp).flatten)
  }

  /**
   * Backtrack through the backpointer maps to recover the optimal tag sequence.
   */
  private def backtrack(backpointers: List[Map[Option[Tag], Option[Tag]]]): Vector[Option[Tag]] = {
    @tailrec def inner(backpointers: List[Map[Option[Tag], Option[Tag]]], curTag: Option[Tag], tags: List[Option[Tag]]): List[Option[Tag]] =
      backpointers match {
        case Nil => assert(curTag == None); tags
        case currPointers :: previousPointers => inner(previousPointers, currPointers(curTag), curTag :: tags)
      }
    val UMap(None -> lastTag) :: previousPointers = backpointers
    inner(previousPointers, lastTag, Nil).toVector
  }
}

////////////////////////////////
// TagEdgeScorer
////////////////////////////////

trait TagEdgeScorer[Sym, Tag] {
  /**
   * Calculate the value of a transition from the previous word/tag pair to the current word/tag pair.
   */
  def apply(prevSym: Option[Sym], prevTag: Option[Tag], currSym: Option[Sym], currTag: Option[Tag]): Double
}
