package dhg.pos.data.prep

import dhg.nlp.util.CollectionUtils._
import dhg.util.CollectionUtil._
import dhg.util.FileUtil._
import dhg.pos.data.read._
import scala.collection.mutable

object ExtractPostags {

  val HumanAnnotatedDir = "data/human-annotated/2013acl"

  def main(args: Array[String]): Unit = {

    val generateRawData = args.contains("-r")

    def htd(htdFile: String, mapTag: Map[String, String], humorc: String, testTD: Set[(String, String)]): Iterator[Vector[Vector[(String, String)]]] = {
      val allLines = File(HumanAnnotatedDir, htdFile).readLines("UTF-8")
      val blocked = allLines.splitWhere(_.trim.startsWith("%% After "))
      val cleanBlocked = blocked.map(_.map(_.trim).filter(_.nonEmpty)).filter(_.nonEmpty)
      val annotationTD = mutable.Set[(String, String)]()
      cleanBlocked.zipWithIndex.map {
        case (cleanBlock, blockNum) =>
          val lines = cleanBlock.map(_.split("\\s+").toList).sortBy(_.head)
          val pairs = for (w :: tags <- lines; t <- tags if mapTag.isDefinedAt(t)) yield Vector((w, mapTag(t)))
          annotationTD ++= pairs.flatten
          println(f"$htdFile, $humorc, through block ${blockNum + 1}: P=${(annotationTD & testTD).size / annotationTD.size.toDouble}%.2f, R=${(annotationTD & testTD).size / testTD.size.toDouble}%.2f")

          pairs
      }.filter(_.nonEmpty)
    }

    def otd(htdFile: String, tagged: TraversableOnce[Vector[(String, String)]], mapTag: Map[String, String], testTD: Set[(String, String)]): Iterator[Vector[Vector[(String, String)]]] = {
      val topFromTagged = tagged.flatten.counts.toVector.sortBy(-_._2).map(_._1)
      htd(htdFile, mapTag, "otd", testTD).scanLeft(Vector[Vector[(String, String)]]())(_ ++ _).drop(1).map { block =>
        val numHtdPairs = block.flatten.toSet.size
        val topKPairs = topFromTagged.take(numHtdPairs)
        topKPairs.map(Vector(_))
      }
    }

    def otdWords(htdFile: String, tagged: TraversableOnce[Vector[(String, String)]], mapTag: Map[String, String], testTD: Set[(String, String)]): Iterator[Vector[Vector[(String, String)]]] = {
      val taggedGrouped = tagged.flatten.to[Set].groupByKey
      htd(htdFile, mapTag, "otdWords", testTD).scanLeft(Vector[Vector[(String, String)]]())(_ ++ _).drop(1).map { block =>
        val htdWord = block.flatten.map(_._1).toSet
        val allTagsForWords = taggedGrouped.filterKeys(htdWord).ungroup.toVector.sorted
        allTagsForWords.map(Vector(_))
      }
    }

    def dumpLcDictBlocks(outDir: String, outFile: String, blocks: Iterator[Vector[Vector[(String, String)]]], mapTag: Map[String, String]) {
      for ((data, i) <- blocks.scanLeft(Vector[Vector[(String, String)]]())(_ ++ _).zipWithIndex.drop(1)) {
        dump(outDir, s"$outFile-lc$i.txt", data, mapTag)
      }
    }

    //
    // PTB
    //
    {
      val ptb: PtbReader[String] = FullTagsetPtbReader
      val rootDir = "data/prepared/pos/ptb/"

      val mapTag = ptb.mapTag
      val testTD = ptb.testData.flatten.toSet

      println("    %-42s   %-6s   %-6s   %-6s".format("ptb", "sent", "toks", "unique"))

      dumpLcDictBlocks(rootDir + "train", "tagdict-ann2-human", htd("eng-tagdict-annotator2-4hr.txt", mapTag, "human", testTD), mapTag)
      dumpLcDictBlocks(rootDir + "train", "tagdict-ann2-oracle", otd("eng-tagdict-annotator2-4hr.txt", ptb.htdBasisData, mapTag, testTD), mapTag)
      //dumpLcDictBlocks(rootDir + "train", "tagdict-ann2-oracle-byWords", otdWords("eng-tagdict-annotator2-4hr.txt", ptb.htdBasisData, mapTag), mapTag)
      dumpLcDictBlocks(rootDir + "train", "tagdict-ann3-human", htd("eng-tagdict-annotator3-4hr.txt", mapTag, "human", testTD), mapTag)
      dumpLcDictBlocks(rootDir + "train", "tagdict-ann3-oracle", otd("eng-tagdict-annotator3-4hr.txt", ptb.htdBasisData, mapTag, testTD), mapTag)
      //dumpLcDictBlocks(rootDir + "train", "tagdict-ann3-oracle-byWords", otdWords("eng-tagdict-annotator3-4hr.txt", ptb.htdBasisData, mapTag), mapTag)

      dumpLcDictBlocks(rootDir + "train", "sentences-ann2-human", ptb.humanAnnotated(pathjoin(HumanAnnotatedDir, "eng-sentences-annotator2-4hr.txt")), mapTag)
      dumpLcDictBlocks(rootDir + "train", "sentences-ann2-oracle", ptb.oracleCorrespondingToHumanAnnotated(pathjoin(HumanAnnotatedDir, "eng-sentences-annotator2-4hr.txt"), testTD), mapTag)
      dumpLcDictBlocks(rootDir + "train", "sentences-ann3-human", ptb.humanAnnotated(pathjoin(HumanAnnotatedDir, "eng-sentences-annotator3-4hr.txt")), mapTag)
      dumpLcDictBlocks(rootDir + "train", "sentences-ann3-oracle", ptb.oracleCorrespondingToHumanAnnotated(pathjoin(HumanAnnotatedDir, "eng-sentences-annotator3-4hr.txt"), testTD), mapTag)

      dump(rootDir + "train", "tagdict-both-human-lc8.txt", (htd("eng-tagdict-annotator2-4hr.txt", mapTag, "human", testTD) ++ htd("eng-tagdict-annotator3-4hr.txt", mapTag, "human", testTD)).flatten.toVector, mapTag)
      dump(rootDir + "train", "sentences-both-human-lc8.txt", (ptb.humanAnnotated(pathjoin(HumanAnnotatedDir, "eng-sentences-annotator2-4hr.txt")) ++ ptb.humanAnnotated(pathjoin(HumanAnnotatedDir, "eng-sentences-annotator3-4hr.txt"))).flatten.toVector, mapTag)

      if (generateRawData) {
        val raw = ptb.rawData.toVector
        for (i <- 1 to 5)
          dumpRaw(rootDir + "raw", "raw-fold-%02d.txt".format(i), raw.shuffle.takeSub(1000000))
      }

      dump(rootDir + "eval", "dev.txt", ptb.devData.toVector, mapTag)
      dump(rootDir + "eval", "test.txt", ptb.testData.toVector, mapTag)

      println
    }

    //
    // PTB - Universal TagSet
    //
    {
      val ptb: PtbReader[String] = UniversalTagsetPtbReader
      val rootDir = "data/prepared/pos/enu/"

      val mapTag = ptb.mapTag
      val testTD = ptb.testData.flatten.toSet

      println("    %-42s   %-6s   %-6s   %-6s".format("ptb-universal", "sent", "toks", "unique"))

      dumpLcDictBlocks(rootDir + "train", "tagdict-ann2-human", htd("eng-tagdict-annotator2-4hr.txt", mapTag, "human", testTD), mapTag)
      dumpLcDictBlocks(rootDir + "train", "tagdict-ann2-oracle", otd("eng-tagdict-annotator2-4hr.txt", ptb.htdBasisData, mapTag, testTD), mapTag)
      //dumpLcDictBlocks(rootDir + "train", "tagdict-ann2-oracle-byWords", otdWords("eng-tagdict-annotator2-4hr.txt", ptb.htdBasisData, mapTag), mapTag)
      dumpLcDictBlocks(rootDir + "train", "tagdict-ann3-human", htd("eng-tagdict-annotator3-4hr.txt", mapTag, "human", testTD), mapTag)
      dumpLcDictBlocks(rootDir + "train", "tagdict-ann3-oracle", otd("eng-tagdict-annotator3-4hr.txt", ptb.htdBasisData, mapTag, testTD), mapTag)
      //dumpLcDictBlocks(rootDir + "train", "tagdict-ann3-oracle-byWords", otdWords("eng-tagdict-annotator3-4hr.txt", ptb.htdBasisData, mapTag), mapTag)

      dumpLcDictBlocks(rootDir + "train", "sentences-ann2-human", ptb.humanAnnotated(pathjoin(HumanAnnotatedDir, "eng-sentences-annotator2-4hr.txt")), mapTag)
      dumpLcDictBlocks(rootDir + "train", "sentences-ann2-oracle", ptb.oracleCorrespondingToHumanAnnotated(pathjoin(HumanAnnotatedDir, "eng-sentences-annotator2-4hr.txt"), testTD), mapTag)
      dumpLcDictBlocks(rootDir + "train", "sentences-ann3-human", ptb.humanAnnotated(pathjoin(HumanAnnotatedDir, "eng-sentences-annotator3-4hr.txt")), mapTag)
      dumpLcDictBlocks(rootDir + "train", "sentences-ann3-oracle", ptb.oracleCorrespondingToHumanAnnotated(pathjoin(HumanAnnotatedDir, "eng-sentences-annotator3-4hr.txt"), testTD), mapTag)

      dump(rootDir + "train", "tagdict-both-human-lc8.txt", (htd("eng-tagdict-annotator2-4hr.txt", mapTag, "human", testTD) ++ htd("eng-tagdict-annotator3-4hr.txt", mapTag, "human", testTD)).flatten.toVector, mapTag)
      dump(rootDir + "train", "sentences-both-human-lc8.txt", (ptb.humanAnnotated(pathjoin(HumanAnnotatedDir, "eng-sentences-annotator2-4hr.txt")) ++ ptb.humanAnnotated(pathjoin(HumanAnnotatedDir, "eng-sentences-annotator3-4hr.txt"))).flatten.toVector, mapTag)

      if (generateRawData) {
        val raw = ptb.rawData.toVector
        for (i <- 1 to 5)
          dumpRaw(rootDir + "raw", "raw-fold-%02d.txt".format(i), raw.shuffle.takeSub(1000000))
      }

      dump(rootDir + "eval", "dev.txt", ptb.devData.toVector, mapTag)
      dump(rootDir + "eval", "test.txt", ptb.testData.toVector, mapTag)

      println
    }

    //
    // Kin
    //
    {
      val kin = MuriKinCleaner
      val rootDir = "data/prepared/pos/kin/"

      val mapTag = kin.fullTagset.map(t => (t,t)).toMap

      println("    %-42s   %-6s   %-6s   %-6s".format("kin", "sent", "toks", "unique"))

      val humanTagdictFile = pathjoin(HumanAnnotatedDir, "kin-tagdict-annotator1-4hr.txt")
      val humanTaggedSentencesFile = pathjoin(HumanAnnotatedDir, "kin-sentences-annotator1-4hr.txt")
      dumpLcDictBlocks(rootDir + "train", "tagdict-ann1-human", kin.readHumanTagdictFile(humanTagdictFile).map(Vector(_)), mapTag)
      dumpLcDictBlocks(rootDir + "train", "sentences-ann1-human", kin.readHumanTaggedFile(humanTaggedSentencesFile), mapTag)

      if (generateRawData) {
        val raw = kin.rawData.flatten.toVector
        for (i <- 1 to 5)
          dumpRaw(rootDir + "raw", "raw-fold-%02d.txt".format(i), raw.shuffle.takeSub(1000000))
      }

      dump(rootDir + "eval", "dev.txt", kin.devData(Some(pathjoin(HumanAnnotatedDir, "kin-dev-supplement.txt"))).toVector, mapTag)
      dump(rootDir + "eval", "test.txt", kin.testData(pathjoin(HumanAnnotatedDir, "kin-testdata.txt")).toVector, mapTag)

      println
    }

    //
    // Mlg
    //
    {
      val mlg = MuriMlgCleaner
      val rootDir = "data/prepared/pos/mlg/"

      val mapTag = mlg.fullTagset.map(t => (t,t)).toMap

      println("    %-42s   %-6s   %-6s   %-6s".format("mlg", "sent", "toks", "unique"))

      val humanTagdictFile = pathjoin(HumanAnnotatedDir, "mlg-tagdict-annotator2-4hr.txt")
      val humanTaggedSentencesFile = pathjoin(HumanAnnotatedDir, "mlg-sentences-annotator2-4hr.txt")
      dumpLcDictBlocks(rootDir + "train", "tagdict-ann2-human", mlg.readHumanTagdictFile(humanTagdictFile).map(Vector(_)), mapTag)
      dumpLcDictBlocks(rootDir + "train", "sentences-ann2-human", mlg.readHumanTaggedFile(humanTaggedSentencesFile), mapTag)

      if (generateRawData) {
        val raw = mlg.rawData.flatten.toVector
        for (i <- 1 to 5)
          dumpRaw(rootDir + "raw", "raw-fold-%02d.txt".format(i), raw.shuffle.takeSub(1000000))
      }

      dump(rootDir + "eval", "dev.txt", mlg.devData(Some(pathjoin(HumanAnnotatedDir, "mlg-dev-supplement.txt"))).toVector, mapTag)
      dump(rootDir + "eval", "test.txt", mlg.testData(pathjoin(HumanAnnotatedDir, "mlg-testdata.txt")).toVector, mapTag)

      println
    }
  }

  def dumpRaw(dir: String, file: String, data: Vector[Vector[String]]) {
    val tokens = data.flatten
    println("    %-42s   %-6s   %-6s   %-6s".format(file, data.size, tokens.size, tokens.distinct.size))
    writeUsing(File(dir, file), "UTF-8") { w => data.foreach(s => w.writeLine(s.mkString(" "))) }
  }

  def dump(dir: String, file: String, data: Vector[Vector[(String, String)]], mapTag: Map[String, String]) {
    val tokens = data.flatten.filter(p => mapTag.isDefinedAt(p._2))
    val numSent = if (file.contains("tagdict")) "" else data.size.toString
    val numToks = if (file.contains("tagdict")) "" else tokens.size.toString
    println("    %-42s   %-6s   %-6s   %-6s".format(file, numSent, numToks, tokens.distinct.size))
    writeUsing(File(dir, file), "UTF-8") { w => data.foreach(s => w.writeLine(sent2str(s))) }
  }

  def sent2str(s: Vector[(String, String)]) = {
    s.map { case (w, t) => "%s|%s".format(w, t) }.mkString(" ")
  }

}
