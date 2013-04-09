package dhg.pos.data.prep

import dhg.util.CollectionUtil._
import dhg.util.FileUtil._

object TdAlign {

  def main(args: Array[String]) {

    val Dir = "data/human-annotated/"

    for (
      (fn, lang) <- Seq(
        "human-tagdict-en-1hr-ann1.txt" -> "ptb",
        "human-tagdict-en-1hr-ann2.txt" -> "ptb",
        "human-tagdict-en-2hr-ann1.txt" -> "ptb",
        "human-tagdict-en-2hr-ann2.txt" -> "ptb",
        "human-tagdict-kin-2hr-ann1.txt" -> "kin",
        "human-tagdict-mlg-2hr-ann2.txt" -> "mlg")
    ) {

      val htd =
        File(Dir, fn).readLines.zipWithIndex
          .mapKeys { _.split("\\s+").toList }
          .map { case (w :: tags, i) => w -> tags }
          .toMap

      val mfw =
        File(s"data/most-frequent-words-$lang.txt").readLines.map(_.drop(1)).toVector

      val mfwSet = mfw.toSet
      for ((w, ts) <- htd; if !mfwSet(w) && ts.nonEmpty) println(w)

      writeUsing(File(Dir, "new-" + fn)) { f =>
        for (w <- mfw)
          f.write((w :: htd.getOrElse(w, Nil)).mkString("", "\t", "\n"))
      }

    }
  }
}
