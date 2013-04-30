package dhg.pos.lp

import dhg.util.CollectionUtil._
import dhg.nlp.util.CollectionUtils._
import dhg.util.FileUtil._
import dhg.nlp.tag.WeightedTagDict
import dhg.nlp.tag.TagDict
import dhg.nlp.tag.SimpleWeightedTagDict
import upenn.junto.config._
import upenn.junto.app.JuntoRunner
import upenn.junto.util.GraphStats
import java.util.ArrayList
import dhg.pos.lp.junto.OutputExtractor
import upenn.junto.graph.GraphIo
import dhg.pos.data.read.PtbReader
import com.typesafe.scalalogging.log4j.Logging

class JuntoTagger[Sym, Tag](
  lpGraphBuilder: LpGraphBuilder[Sym],
  maxIterations: Int = 10)
  extends Logging {

  def tagFromAnnotations(
    taggedSentences: Iterable[Vector[(Sym, Tag)]],
    tagdict: TagDict[Sym, Tag],
    rawCorpus: Vector[Vector[Sym]],
    threshold: Double,
    filename: Option[String]) = {

    val tokenSupervisionSeeds =
      for (
        (sent, sentIdx) <- taggedSentences.iterator.zipWithIndex;
        ((word, tag), tokIdx) <- sent.zipWithIndex if tag.toString != "X"
      ) yield (TokenFeat(LpGraphWord(Some(word)), sentIdx + rawCorpus.size, tokIdx + 1), TagFeat(tag.toString))

    val typeSupervisionSeeds =
      for ((node, tag) <- tagdict.setIterator.ungroup if tag.toString != "X")
        yield (WordFeat(Some(node)), TagFeat(tag.toString))

    tag(
      rawCorpus ++ taggedSentences.submap(_._1),
      tokenSupervisionSeeds ++ typeSupervisionSeeds,
      threshold,
      filename)
  }

  private[this] def tag(
    rawCorpus: Vector[Vector[Sym]],
    seeds: Iterator[(LpFeature, TagFeat)],
    threshold: Double,
    filename: Option[String]): Vector[Vector[(Sym, Map[Tag, Double])]] = {

    val edges = lpGraphBuilder.makeGraph(rawCorpus)

    filename.foreach { fn =>
      writeUsing(File(fn + ".lp")) { f =>
        val seedMap = seeds.map { case (w, t) => (w.toString, t.toString) }.groupByKey
        val edgeMap =
          edges
            .map { case (a, b, w) => (a.toString, (b.toString, w)) }
            .groupByKey
        for ((a, bs) <- edgeMap.toVector.sortBy(_._1)) {
          val tags = seedMap.getOrElse(a, Vector()).sorted
          val tagString = tags.map { t => "%s %s".format(t, 1.0) }.mkString(" ")
          val bString = bs.mapt { (t, w) => "%s %s".format(t, w) }.mkString(" ")
          f.writeLine("%s\t%s\t%s".format(a, tagString, bString))
        }
      }
    }

    //    writeUsing(pathjoin(System.getenv("HOME"), "ml/graph.dot")) { w =>
    //      val seedMap = seeds.map { case (node, label) => (node.toString, label.toString) }.toSet.groupByKey
    //      val undirectedEdges = edges
    //        .map {
    //          case (a: PrevWordFeat, b, _) => (a, b)
    //          case (a: NextWordFeat, b, _) => (a, b)
    //          case (a: TokenFeat, b, _) => (a, b)
    //          case (a, b: PrefixFeat, _) => (a, b)
    //          case (a, b: SuffixFeat, _) => (a, b)
    //          case (a, b, _) => (b, a)
    //        }
    //        .map { case (n1, n2) => (n1.toString, n2.toString) }
    //        .toSet
    //        .toVector
    //        .sorted
    //      w.writeLine("graph G {")
    //      w.writeLine("node [style=filled, fillcolor=grey]; %s;".format(seedMap.keys.map('"' + _ + '"').mkString(" ")))
    //      w.writeLine("node [style=filled, fillcolor=white];")
    //      for ((n1, n2) <- undirectedEdges) {
    //        w.writeLine(""""%s" -- "%s";""".format(n1, n2))
    //      }
    //      w.writeLine("}")
    //    }

    val graph = GraphBuilder(
      edges.map { case (node1, node2, weight) => new Edge(node1.toString, node2.toString, weight) },
      seeds.map { case (node, label) => new Label(node.toString, label.toString, 1.0) },
      testLabels = List[Label](),
      beta = 2.0,
      maxNeighbors = Int.MaxValue,
      maxSeedsPerClass = Int.MaxValue,
      setGaussianWeights = false,
      sigmaFactor = 0.0,
      pruneThreshold = "0",
      isDirected = true)

    logger.info("\n" + GraphStats.PrintStats(graph))

    val result = new ArrayList[Map[String, Double]]
    JuntoRunner(
      "mad",
      graph,
      maxIters = maxIterations,
      mu1 = 1.0,
      mu2 = 0.01,
      mu3 = 0.01,
      keepTopKLabels = Int.MaxValue,
      useBipartiteOptimization = false,
      verbose = false,
      resultList = result)

    val outputFile = mktemp("output_file")
    println(outputFile)
    GraphIo.saveEstimatedScores(graph, outputFile.path)
    val TagRe = """TAG_(.+)""".r
    val TokenRe = """TOKEN_(.+)_(\d+)_(\d+)""".r
    val outputExtractor = new OutputExtractor(typesToExtract = Set("TOKEN"), cutLowLabels = Some(false))
    val taggedTokens =
      outputExtractor.extractOutput(outputFile.path).collect {
        case (TokenFeat(Some(word), sidx, tidx), labels) =>
          (word, sidx, tidx) -> labels.map {
            case (TagRe(t), p) => t.asInstanceOf[Tag] -> p
          }.filter(_._2 >= threshold).normalizeValues
      }

    val taggedRaw = taggedTokens.groupBy(_._1._2)
    //outputFile.delete()

    verify(
      rawCorpus.zipWithIndex.map {
        case (rSent, sIdx) =>
          val tSent = taggedRaw.getOrElse(sIdx, Vector()).map { case ((w, _, tIdx), tags) => tIdx -> (w, tags) }.toMap
          rSent.zipWithIndex.map {
            case (rWord, tIdx) =>
              val (tWord, tags) = tSent.getOrElse(tIdx + 1, (rWord.toString, Map[Tag, Double]()))
              assert(rWord == tWord, "%s != %s".format(rWord, tWord))
              rWord -> tags
          }
      })
  }

  def verify(rawTokenTagDists: Vector[Vector[(Sym, Map[Tag, Double])]]) = {
    //
    // Print out some of the sentences
    //
    for (sent <- rawTokenTagDists.take(10)) {
      for ((word, tags) <- sent)
        println("%-20s %s".format(word, tags.toVector.sortBy(_._2).reverse.map { case (t, p) => "%s -> %.2f".format(t, p) }.mkString(", ")))
      println
    }

    rawTokenTagDists
  }

}
