package dhg.pos.lp
package graph

import scala.collection.breakOut
import dhg.util.CollectionUtil._
import dhg.nlp.util.CollectionUtils._
import dhg.util.FileUtil._
import dhg.util.Pattern.Range

/**
 * test 1
 * io.Source.fromFile("graph-fold-01.txt").getLines.filter{l => val Array(a,b,_) = l.split(" "); """x\d|_\d|X2|X3|<b>""".r.findFirstIn(a+b).isEmpty}.toSeq.sorted foreach println
 */
class TypeLpGraphBuilder[Sym]() extends LpGraphBuilder[Sym] {

  def makeGraph(rawData: Vector[Vector[Sym]]): Iterator[(LpFeature, LpFeature, Double)] = {
    val rawEnded = rawData.map(s => None +: s.map(Some(_)) :+ None)
    val allSymbols: Set[Sym] = rawData.flatten.toSet

    // Bigrams, word given next: (x, y) P(w_t = x | w_{t+1} = y)
    val nextWordBigrams = weightWithConditionalProbByCounts(rawEnded.map(_.sliding2).flatten.map(_.swap)) //Map[next,Map[word,P(word|next)]]
    val nextBigramEdges =
      for ((y, xs) <- nextWordBigrams; (x, p) <- xs) yield {
        // An edge from prev to word is weighted by the prob of prev given word.
        // Labels are pushed from previous_word feature according to how indicative they are
        // of the word.  If the P(prev | word) is high, then prev is a good indicator of the
        // word, so it can be used as a weight to ensure strong pushes from prev to word.
        val e1 = (PrevWordFeat(x), WordFeat(y), p)
        // An edge from word to next is weighted by the prob of word given next.
        val e2 = (WordFeat(x), NextWordFeat(y), p)
        Seq(e1, e2)
      }

    // Bigrams, word given prev: (x, y) P(w_t = y | w_{t-1} = x)
    val prevWordBigrams = weightWithConditionalProbByCounts(rawEnded.map(_.sliding2).flatten) //Map[prev,Map[word,P(word|prev)]]
    val prevBigramEdges =
      for ((x, ys) <- prevWordBigrams; (y, p) <- ys) yield {
        // An edge from next to word is weighted by the prob of next given word
        val e1 = (NextWordFeat(y), WordFeat(x), p)
        // edge from word to prev is weighted by the prob of word given prev
        val e2 = (WordFeat(y), PrevWordFeat(x), p)
        Seq(e1, e2)
      }

    val wordPrefixEdges =
      makeEdges((1 to 5).flatMap(n => allSymbols.map(_.toString).collect { case w if w.size > n => w.takeRight(n) -> w }.toVector)
        .groupByKey.iterator
        .map { case (prefix, words) => PrefixFeat(prefix, prefix.length) -> words.map(w => WordFeat(Some(w)))(breakOut) })

    val wordSuffixEdges =
      makeEdges((1 to 5).flatMap(n => allSymbols.map(_.toString).collect { case w if w.size > n => w.takeRight(n) -> w }.toVector)
        .groupByKey.iterator
        .map { case (suffix, words) => SuffixFeat(suffix, suffix.length) -> words.map(w => WordFeat(Some(w)))(breakOut) })

    (nextBigramEdges ++ prevBigramEdges).flatten ++ wordPrefixEdges ++ wordSuffixEdges
  }
}

object TypeLpGraphBuilder {
  def main(args: Array[String]) {
    val lpgb = new TypeLpGraphBuilder[String]()

    val Seq(langId, Range(foldRange)) = args.toSeq

    for (fold <- foldRange) {
      val rawCorpusFile = "data/prepared/pos/%s/raw/raw-fold-%02d.txt".format(langId, fold)
      val rawCorpus = File(rawCorpusFile).readLines("UTF-8").map(_.split("\\s+").toVector).toVector

      writeUsing(File("data/prepared/pos/%s/lp-graph".format(langId), "graph-fold-%02d.txt".format(fold))) { w =>
        for ((a, b, p) <- lpgb.makeGraph(rawCorpus))
          w.writeLine("%s %s %f".format(a, b, p))
      }
    }
  }

}
