package dhg.pos.data.read

import java.io.BufferedReader
import dhg.pos.util.FileUtils
import java.io.FileReader
import scala.io.Source
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.Writer
import scala.collection.mutable.Buffer
import scala.util.Random
import dhg.util.CollectionUtil._
import dhg.nlp.util.CollectionUtils._
import dhg.util.FileUtil._
import scala.collection.breakOut
import scala.collection.mutable

trait PtbReader[Tag] {

  val COMB_DIR = "data/ptb3/combined" // 48934 sentences

  val FN_RE = """wsj_(\d\d)(\d\d)\.mrg""".r
  val TERMINAL_RE = """\((\S+) (\S+)\)""".r

  def fullTagSet: Set[String]
  def mapTag: Map[String, Tag]

  def humanAnnotationDataGoldTagged: Vector[(String, Iterator[Vector[(String, Tag)]])] = Vector("PTB section 1" -> read(1 to 1)) //  sentences that the annotator will tag
  def humanAnnotationRawData = humanAnnotationDataGoldTagged.mapVals(_.map(_.map(_._1)))
  def rawDataTagged = read(2 to 14) //             //  data from which raw training sets are sampled
  def rawData = rawDataTagged.map(_.map(_._1))
  def devData = read(19 to 21)
  def testData = read(22 to 24)
  def htdBasisData = humanAnnotationDataGoldTagged.iterator.flatMap(_._2) ++ rawDataTagged // data used to compute the frequency list for TagDict annotation  (01-14)
  def htdBasisRawData = htdBasisData.map(_.map(_._1))
  // FST-development data: 15-18

  def read(sections: Range): Iterator[Vector[(String, Tag)]] = {
    val sentenceIterator =
      new MultilineTreeSentenceIterator((
        for (
          f <- File(COMB_DIR).listFiles;
          fn = Some(f.getName);
          FN_RE(sec, _) <- fn if sections.contains(sec.toInt)
        ) yield f.getAbsolutePath).sorted.iterator)
    new TaggedSentenceMultilineTreeIterator(sentenceIterator)
  }

  class TaggedSentenceMultilineTreeIterator(private[this] val sentenceIterator: Iterator[String]) extends Iterator[Vector[(String, Tag)]] {
    override def next() =
      TERMINAL_RE.findAllIn(sentenceIterator.next).matchData
        .map(_.subgroups)
        .flatMap {
          case Seq("-NONE-", word) => None
          case Seq(pos, word) => Some(word.trim, mapTag(pos.trim))
        }.toVector
    override def hasNext() = sentenceIterator.hasNext
  }

  class MultilineTreeSentenceIterator(private[this] val files: Iterator[String], encoding: Option[String] = None) extends Iterator[String] {
    private[this] var linesIterator: Iterator[String] = Iterator.empty

    override def next(): String = {
      while (hasNext) {
        val line = linesIterator.next
        if (line.trim.startsWith("(")) {
          return line
        }
      }
      throw new RuntimeException()
    }

    override def hasNext(): Boolean = {
      while (true) {
        if (linesIterator.hasNext)
          return true
        else if (files.hasNext)
          linesIterator = {
            val f = encoding match {
              case Some(enc) => Source.fromFile(files.next, enc)
              case None => Source.fromFile(files.next)
            }
            val x = f.getLines.mkString(" ")
            var pc = 0
            var start = 0
            val sentences = Buffer[String]()
            for ((c, i) <- x.zipWithIndex) {
              if (c == '(') {
                if (pc == 0)
                  start = i
                pc += 1
              }
              if (c == ')') {
                pc -= 1
                if (pc == 0)
                  sentences.append(x.substring(start, i + 1))
              }
            }
            sentences.iterator
          }
        else
          return false
      }
      throw new AssertionError
    }
  }

  /**
   * Returns blocks of annotated sentences
   */
  def humanAnnotated(fn: String): Iterator[Vector[Vector[(String, Tag)]]] = {
    val TreeRe = """\(TOP (.+)\)""".r
    val TERMINAL_RE = """\((\S+) (\S+)\)""".r

    val allLines = File(fn).readLines("UTF-8")

    val blocked = allLines.splitWhere(_.trim.startsWith("%% After "))
    val cleanBlocked = blocked.map(_.map(_.trim).filter(_.nonEmpty)).filter(_.nonEmpty)
    cleanBlocked.map(_.flatMap {
      case TreeRe(line) =>
        TERMINAL_RE.findAllIn(line).matchData.map(_.subgroups)
          .map { case Seq(pos, word) => (word, if (pos == "x" || fullTagSet(pos)) pos else "X") }
          .map {
            case (word, "x") => None
            case (word, pos) => Some(word.trim, mapTag(pos.trim))
          }.toVector match {
            case s if s.forall(_.isDefined) => Some(s.flatten)
            case s if s.forall(_.isEmpty) => None
          }
      case line => /*println("could not parse: " + line);*/ None
    }).filter(_.nonEmpty)
  }

  def oracleCorrespondingToHumanAnnotated(fn: String, testTD: Set[(String, Tag)]): Iterator[Vector[Vector[(String, Tag)]]] = {
    val ptbFile = read(1 to 1).take(500).toVector

    var total = 0
    var correct = 0
    val annotationTD = mutable.Set[(String, Tag)]()

    humanAnnotated(fn).zipWithIndex.map {
      case (cleanBlock, blockNum) =>
        val oracleBlock =
          for (sent <- cleanBlock) yield {
            val sentWords = sent.map(_._1)
            ptbFile.find(_.map(_._1) == sentWords)
              .getOrElse(sys.error(s"could not locate sentence from file: $fn\n${sentWords.mkString(" ")}\n\n${ptbFile.map(_.map(_._1).mkString(" ")).mkString("\n")}"))
          }

        {
          (cleanBlock zipSafe oracleBlock).foreach { sentPair =>
            total += sentPair.zipSafe.size
            correct += sentPair.zipSafe.count { case (a, o) => a == o }
          }
          annotationTD ++= cleanBlock.flatten
          println(f"$fn, through block ${blockNum + 1}: ${correct / total.toDouble}%.3f, P=${(annotationTD & testTD).size / annotationTD.size.toDouble}%.2f, R=${(annotationTD & testTD).size / testTD.size.toDouble}%.2f")
        }

        oracleBlock
    }
  }
}

object FullTagsetPtbReader extends PtbReader[String] {
  // All 45 PTB tags:
  val fullTagSet = "# $ `` '' , -LRB- -RRB- . : CC CD DT EX FW IN JJ JJR JJS LS MD NN NNP NNPS NNS PDT POS PRP PRP$ RB RBR RBS RP SYM TO UH VB VBD VBG VBN VBP VBZ WDT WP WP$ WRB".split(" ").toSet
  val mapTag = fullTagSet.map(t => (t, t)).toMap + ("X" -> "X")

  def main(args: Array[String]) {
    println("ptb")
    println("raw data tokens = " + rawData.flatten.size)
  }
}

object UniversalTagsetPtbReader extends PtbReader[String] {
  val mappingFile = "data/universal-tagging-mappings/en-ptb.map"
  val mapTag = File(mappingFile).readLines.map(_.split("\t").toTuple2).toMap + ("X" -> "X")
  val fullTagSet = mapTag.values.toSet
}
