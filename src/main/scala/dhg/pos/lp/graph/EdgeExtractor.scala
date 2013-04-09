package dhg.pos.lp.graph

import scala.collection.breakOut
import dhg.nlp.util.CollectionUtils._
import dhg.pos.dict.ExternalDictionary
import dhg.pos.lp.DictposFeat
import dhg.pos.lp.LpFeature
import dhg.pos.lp.LpGraphWord
import dhg.pos.lp.NextWordFeat
import dhg.pos.lp.PrefixFeat
import dhg.pos.lp.PrevWordFeat
import dhg.pos.lp.SuffixFeat
import dhg.pos.lp.TokenFeat
import dhg.pos.lp.WordFeat
import dhg.util.CollectionUtil._
import dhg.pos.fst.SimpleFst
import dhg.pos.lp.MorphFeat

trait EdgeExtractor[Sym] {
  def apply(rawData: Vector[Vector[Sym]]): Iterator[(LpFeature, Vector[LpFeature])]

  def rawEnded(rawData: Vector[Vector[Sym]]) = rawData.map(s => None +: s.map(Some(_)) :+ None)
  def rawTokens(rawData: Vector[Vector[Sym]]) = rawEnded(rawData).zipWithIndex.map { case (sent, sentIdx) => sent.zipWithIndex.map { case (tok, tokIdx) => TokenFeat(LpGraphWord(tok), sentIdx, tokIdx) } }
  def tokenBigrams(rawData: Vector[Vector[Sym]]) = rawTokens(rawData).map(_.sliding2).flatten
  def allSymbols(rawData: Vector[Vector[Sym]]): Set[Sym] = rawData.flatten.toSet
}

case class TokenWordEdgeExtractor[Sym]() extends EdgeExtractor[Sym] {
  def apply(rawData: Vector[Vector[Sym]]): Iterator[(LpFeature, Vector[LpFeature])] =
    rawTokens(rawData).flatten
      .groupBy(_.token).iterator
      .mapKeys(WordFeat(_))
}

case class TokenPrevEdgeExtractor[Sym]() extends EdgeExtractor[Sym] {
  def apply(rawData: Vector[Vector[Sym]]): Iterator[(LpFeature, Vector[LpFeature])] =
    tokenBigrams(rawData)
      .groupBy(_._1.token).iterator
      .map { case (word, tokPairs) => PrevWordFeat(word) -> tokPairs.map(_._2) }
}

case class TokenNextEdgeExtractor[Sym]() extends EdgeExtractor[Sym] {
  def apply(rawData: Vector[Vector[Sym]]): Iterator[(LpFeature, Vector[LpFeature])] =
    tokenBigrams(rawData)
      .groupBy(_._2.token).iterator
      .map { case (word, tokPairs) => NextWordFeat(word) -> tokPairs.map(_._1) }
}

case class WordPrefixEdgeExtractor[Sym](maxLength: Int) extends EdgeExtractor[Sym] {
  def apply(rawData: Vector[Vector[Sym]]): Iterator[(LpFeature, Vector[LpFeature])] =
    (1 to maxLength).flatMap(n => allSymbols(rawData).map(_.toString).collect { case w if w.size > n => w.takeRight(n) -> w }.toVector)
      .groupByKey.iterator
      .map { case (prefix, words) => PrefixFeat(prefix, prefix.length) -> words.map(w => WordFeat(Some(w)))(breakOut) }
}

case class WordSuffixEdgeExtractor[Sym](maxLength: Int) extends EdgeExtractor[Sym] {
  def apply(rawData: Vector[Vector[Sym]]): Iterator[(LpFeature, Vector[LpFeature])] =
    (1 to maxLength).flatMap(n => allSymbols(rawData).map(_.toString).collect { case w if w.size > n => w.takeRight(n) -> w }.toVector)
      .groupByKey.iterator
      .map { case (suffix, words) => SuffixFeat(suffix, suffix.length) -> words.map(w => WordFeat(Some(w)))(breakOut) }
}

case class WordDictposEdgeExtractor[Sym](dict: ExternalDictionary) extends EdgeExtractor[Sym] {
  def apply(rawData: Vector[Vector[Sym]]): Iterator[(LpFeature, Vector[LpFeature])] =
    dict.getTags(allSymbols(rawData).map(_.toString)).iterator
      .map { case (word, poss) => WordFeat(Some(word)) -> poss.map(p => DictposFeat(p))(breakOut) }
}

case class WordMorphEdgeExtractor[Sym](fst: SimpleFst[Sym, Vector[String]]) extends EdgeExtractor[Sym] {
  def apply(rawData: Vector[Vector[Sym]]): Iterator[(LpFeature, Vector[LpFeature])] = {
    fst.analyze(allSymbols(rawData)).iterator.mapVals(_.flatten.distinct)
      .map { case (word, analyses) => WordFeat(Some(word)) -> analyses.map(p => MorphFeat(p))(breakOut) }
  }
}
