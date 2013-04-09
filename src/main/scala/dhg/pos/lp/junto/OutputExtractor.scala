package dhg.pos.lp.junto

import dhg.util.CollectionUtil._
import dhg.nlp.util.CollectionUtils._
import dhg.util.FileUtil._

class OutputExtractor(typesToExtract: Set[String], cutLowLabels: Option[Boolean]) {
  val NodeRE = """([^_]+)_(.+)""".r

  def extractOutput(juntoOutputFile: String): Iterator[(String, Map[String, Double])] = {
    File(juntoOutputFile).readLines("UTF-8")
      .map(extractorMap).flatten
      .map(extractorReduce).flatten
  }

  protected def extractorMap(line: String): Option[(String, String, String)] = {
    Some(line)
      .map(_.split('\t'))
      .map {
        case Array(node @ NodeRE(nodetype, _), gold, injected, estimated, isTestNode, mrr) =>
          (nodetype, node, estimated)
      }
      .filter {
        case (nodetype, node, estimated) =>
          typesToExtract.isEmpty || (typesToExtract(nodetype) && estimated != "")
      }
  }

  protected def extractorReduce(line: (String, String, String)): Option[(String, Map[String, Double])] = {
    Some(line)
      .map {
        case (nodetype, node, estimated) =>
          val labelProbs = estimated
            .split(" ").grouped(2)
            .map { case Array(label, prob) => (label, prob.toDouble) }

          val (beforeDummy, dummyAndAfter) = labelProbs.span { case (label, prob) => label != "__DUMMY__" }
          val labels = cutLowLabels match {
            case Some(cut) =>
              val beforeDummyList = beforeDummy.toList
              cut match {
                case true => // keep only things before DUMMY, redo probs
                  val probSum = beforeDummyList.map(_._2).sum
                  beforeDummyList.map { case (label, prob) => (label, prob / probSum) }.toMap
                case false => // filter out DUMMY, redo probs
                  val afterDummyList = dummyAndAfter.drop(1).toList
                  val filteredList = beforeDummyList ++ afterDummyList
                  val probSum = filteredList.map(_._2).sum
                  filteredList.map { case (label, prob) => (label, prob / probSum) }.toMap
              }
            case None => // keep things before DUMMY, DUMMY node, and top thing after DUMMY; keep probs
              (beforeDummy ++ dummyAndAfter.take(2)).toMap
          }
          (node, labels)
      }
      .filter {
        case (node, labelProbs) => labelProbs.nonEmpty
      }
  }

}
