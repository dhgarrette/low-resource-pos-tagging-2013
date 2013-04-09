package dhg.pos.data.prep

import dhg.pos.util.FileUtils._
import dhg.nlp.util.CollectionUtils._
import dhg.util.CollectionUtil._
import dhg.util.FileUtil._
import dhg.pos.data.read.PtbReader
import dhg.pos.data.read.ReaderUtil._
import dhg.pos.data.read.FullTagsetPtbReader

object AnnotationTaskPrep {

  def topKWords(sentenceIterator: Iterator[Vector[String]], outputFilename: String) = {
    writeUsing(File(outputFilename)) { w =>
      val words = sentenceIterator.flatten
      for (word <- words.counts.toList.sortBy(-_._2).map(_._1).take(6000))
        w.write(s"$word\t\n")
    }
  }

  def prepSentencesForHumanTagging(sentenceIterator: Vector[(String, Iterator[Vector[String]])], outputFilename: String) = {
    writeUsing(File(outputFilename)) { f =>
      for ((filename, sentences) <- sentenceIterator) {
        f.writeLine(filename)
        for (s <- sentences) {
          val line = s.map(w => "(x %s)".format(punctRewrites.getOrElse(w, w)))
          f.writeLine(line.mkString("(TOP (S ", " ", " ) )"))
        }
        f.writeLine("")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val r = FullTagsetPtbReader
    val langId = "eng"
    topKWords(r.htdBasisRawData, s"data/$langId-most-frequent-words.txt")
    def trainToAnnotate = r.humanAnnotationRawData
    prepSentencesForHumanTagging(trainToAnnotate, s"data/$langId-sentences-to-annotate.txt")

    for (
      (langId, r) <- Vector(
        ("kin", MuriKinCleaner),
        ("mlg", MuriMlgCleaner))
    ) {

      def trainToAnnotate = r.humanAnnotatedFilesForTrain.toVector.sorted.mapTo(r.readRawFile)
      val allRaw = r.rawData.iterator.flatten ++ trainToAnnotate.iterator.flatMap(_._2)
      topKWords(allRaw, s"data/$langId-most-frequent-words.txt")
      prepSentencesForHumanTagging(trainToAnnotate, s"data/$langId-sentences-to-annotate.txt")
    }
  }

}
