package dhg.pos.fst

import dhg.util.CollectionUtil._
import dhg.nlp.util.CollectionUtils._
import dhg.util.FileUtil._
import dhg.util.Subprocess
import scala.collection.breakOut
import scalaz._
import Scalaz._

trait SimpleFst[Sym, MorphAnalysis] {
  def analyze(symbols: Set[Sym]): Map[Sym, Vector[MorphAnalysis]]
}

case class FomaSimpleFst(fstFile: String) extends SimpleFst[String, Vector[String]] {
  val flookup = Subprocess(pathjoin(sys.env("HOME"), "bin/foma-fst/flookup")).args(fstFile)

  override def analyze(symbols: Set[String]): Map[String, Vector[Vector[String]]] = {
    symbols.grouped(1000).flatMap { subset =>
      flookup
        .call(subset.mkString("\n"))
        .split("\n").toVector
        .map(_.trim).filter(_.nonEmpty)
        .map(_.split("\\s+").toTuple2)
        .flatMap { case (a, b) => (b != "+?").option((a, b)) }
        .groupByKey
        .mapVals(_.map(_.split("\\+").toVector.filter(_.nonEmpty)))
    }.toMap
  }
}

case class XfstSimpleFst(fstFile: String) extends SimpleFst[String, Vector[String]] {
  val lookup = Subprocess(pathjoin(sys.env("HOME"), "bin/xfst/lookup")).args(fstFile)

  override def analyze(symbols: Set[String]): Map[String, Vector[Vector[String]]] = {
    symbols.grouped(1000).flatMap { subset =>
      lookup
        .call(subset.mkString("\n"))
        .split("\n").toVector
        .map(_.trim).filter(_.nonEmpty)
        .flatMap(_.split("\t") match {
          case Array(a, b, c) => (c != "+?").option((a, c))
          case Array(a, b) => None
        })
        .groupByKey
        .mapVals(_.map(_.split("\\+").toVector.filter(_.nonEmpty)))
    }.toMap
  }
}

object SimpleFstRun {

  def main(args: Array[String]) {

    val lines = """
        12 34.56 1997 
		Ny famonjena antsika dia .... amin' ny anaran' i Jehovah , Mpanao ny lanitra sy ny tany ?
		Koa amin' izany asehoy aminy eo imason' ny fiangonana ny famantarana mahamarina ny fitiavanareo sy ny reharehanay ny aminareo .
		Omeo rariny aho , Andriamanitra ô , ary alaharo ny teniko haharesy ny firenena tsy misy famindram @-@ po ;
		""".split("\n").map(_.trim.split("\\s+").toVector.filter(_.nonEmpty)).toVector.filter(_.nonEmpty)

    {
      val fstFile = "data/fst/mlg-1.fst"
      val fst = new FomaSimpleFst(fstFile)
      val analyses = fst.analyze(lines.flatten.toSet)
      analyses foreach println
      println
    }

    {
      val fstFile = "data/fst/mlg-2.fst"
      val fst = new XfstSimpleFst(fstFile)
      val analyses = fst.analyze(lines.flatten.toSet)
      analyses foreach println
      println
    }

    //    val a = File("data/prepared/pos/mlg/raw/raw-fold-01.txt").readLines.flatMap(_.split("\\s+")).toSet
    //    val b = fst.analyze(a)
    //    b foreach println

  }

}
