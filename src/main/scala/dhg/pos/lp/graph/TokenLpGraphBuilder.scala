package dhg.pos.lp
package graph

import scala.collection.breakOut
import dhg.util.CollectionUtil._
import dhg.nlp.util.CollectionUtils._
import dhg.util.FileUtil._
import dhg.util.Pattern.Range
import dhg.pos.dict.ExternalDictionary
import dhg.pos.dict.EmptyExternalDictionary
import dhg.pos.dict.WiktionaryExternalDictionary

/**
 * test 1
 * io.Source.fromFile("graph-fold-01.txt").getLines.filter{l => val Array(a,b,_) = l.split(" "); """x\d|X2|X3|<b>""".r.findFirstIn(a+b).isEmpty}.toSeq.sorted foreach println
 */
class TokenLpGraphBuilder[Sym](edgeExtractors: Iterable[EdgeExtractor[Sym]]) extends LpGraphBuilder[Sym] {

  def makeGraph(rawData: Vector[Vector[Sym]]): Iterator[(LpFeature, LpFeature, Double)] = {
    edgeExtractors.iterator.flatMap(ee => makeEdges(ee.apply(rawData)))
  }

}
