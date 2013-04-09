package dhg.pos.minmodel

import dhg.nlp.tag.WeightedTagDict

trait ModelMinTagEdgeScorer[Sym, Tag] {
  def apply(
    prevSym: Option[Sym], prevTag: Option[Tag], prevTagWeight: Option[Double],
    currSym: Option[Sym], currTag: Option[Tag], currTagWeight: Option[Double]): Double
}

class CurrentWeightModelMinTagEdgeScorer[Sym, Tag](noneWeight: Double = 1.0) extends ModelMinTagEdgeScorer[Sym, Tag] {
  override def apply(
    prevSym: Option[Sym], prevTag: Option[Tag], prevTagWeight: Option[Double],
    currSym: Option[Sym], currTag: Option[Tag], currTagWeight: Option[Double]) =
    currTagWeight.getOrElse(noneWeight)
}

class SummingModelMinTagEdgeScorer[Sym, Tag](noneWeight: Double = 0.0) extends ModelMinTagEdgeScorer[Sym, Tag] {
  override def apply(
    prevSym: Option[Sym], prevTag: Option[Tag], prevTagWeight: Option[Double],
    currSym: Option[Sym], currTag: Option[Tag], currTagWeight: Option[Double]) = {
    prevTagWeight.getOrElse(noneWeight) + currTagWeight.getOrElse(noneWeight)
  }
}

class MaxingModelMinTagEdgeScorer[Sym, Tag](noneWeight: Double = 0.0) extends ModelMinTagEdgeScorer[Sym, Tag] {
  override def apply(
    prevSym: Option[Sym], prevTag: Option[Tag], prevTagWeight: Option[Double],
    currSym: Option[Sym], currTag: Option[Tag], currTagWeight: Option[Double]) = {
    prevTagWeight.getOrElse(noneWeight) max currTagWeight.getOrElse(noneWeight)
  }
}
