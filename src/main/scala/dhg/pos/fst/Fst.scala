package dhg.pos.fst

import dhg.util.CollectionUtil._
import dhg.nlp.util.CollectionUtils._
import dhg.util.FileUtil._
import dhg.util.Subprocess
import scala.collection.breakOut

trait Fst[Sym, MorphAnalysis] {
  def analyze(symbols: Seq[Sym]): Vector[(Sym, Vector[MorphAnalysis])]
  def analysesOnly(symbols: Seq[Sym]): Vector[Vector[MorphAnalysis]]
  def analyzeAll(corpus: Seq[Seq[Sym]]): Vector[Vector[(Sym, Vector[MorphAnalysis])]]
  def analysesOnlyAll(corpus: Seq[Seq[Sym]]): Vector[Vector[Vector[MorphAnalysis]]]
}

case class FomaFst(fstFile: String) extends Fst[String, String] {
  val flookup = Subprocess(pathjoin(sys.env("FOMA_HOME"), "flookup")).args("-x", fstFile)

  override def analysesOnlyAll(corpus: Seq[Seq[String]]) = {
    corpus.grouped(100).flatMap { subcorpus =>
      val flat = subcorpus.map(_.mkString("\n")).mkString("\n\n")
      val stdout = flookup.call(flat)
      val analyses =
        stdout.trim.split("\n\n\n").toVector
          .map(_.trim.split("\n\n").toVector
            .map(_.trim.split("\n").toVector))
      analyses
    }.toVector
  }

  override def analyzeAll(corpus: Seq[Seq[String]]) = {
    val analyses = analysesOnlyAll(corpus)
    val bySentence = corpus zipSafe analyses
    bySentence.map(_.zipSafe.toVector)(breakOut)
  }

  override def analyze(symbols: Seq[String]) = symbols.zipSafe(analysesOnlyAll(Seq(symbols)).head)(breakOut)
  override def analysesOnly(symbols: Seq[String]) = analysesOnlyAll(Seq(symbols)).head
}

object FstRun {

  def main(args: Array[String]) {

    val fstFile = "data/fst/mlg-1.fst"

    val lines = """
		Ny famonjena antsika dia amin' ny anaran' i Jehovah , Mpanao ny lanitra sy ny tany .
		Koa amin' izany asehoy aminy eo imason' ny fiangonana ny famantarana mahamarina ny fitiavanareo sy ny reharehanay ny aminareo .
		Omeo rariny aho , Andriamanitra ô , ary alaharo ny teniko haharesy ny firenena tsy misy famindram @-@ po ;
		""".split("\n").map(_.trim.split("\\s+").toVector.filter(_.nonEmpty)).toVector.filter(_.nonEmpty)

    val fst = new FomaFst(fstFile)
    val analyses = fst.analyzeAll(lines)

    analyses foreach println

    //    val a = File("data/prepared/pos/kin/raw/raw-fold-01.txt").readLines.map(_.split("\\s+").toVector).toVector
    //    val b = fst.analyzeAll(a)
    //    val c = b.flatten
    //    val d = c.groupByKey.mapVals(_.distinct)
    //    d.filter(_._2.size != 1) foreach println

  }

}
