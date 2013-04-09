package dhg.pos.run

import dhg.util.CollectionUtil._
import dhg.nlp.util.CollectionUtils._
import dhg.util.Pattern.{ UBoolean, UInt }
import dhg.util.FileUtil._
import dhg.util.Time._
import dhg.util.StringUtil._
import dhg.pos.hmm.MemmTagger
import dhg.pos.util.CorpusUtils.RawFile
import dhg.pos.data.read.ReaderUtil

/**
 * CLI for tagging sentences from a text file using a trained tagger model.
 * Tagged output is written to stdout.
 *
 * -taggerModelFile FILEPATH
 * The location of the trained tagger
 *
 * -fileToTag FILEPATH ...
 * The locations of tokenized files, one space-separated sentence per line
 * that should be tagged.
 */
object Tag {

  def main(args: Array[String]) {

    val optionNames = Set(
      "-taggerModelFile",
      "-fileToTag")

    val options: Map[String, Vector[String]] =
      args.foldLeft(Vector[Vector[String]]()) {
        case (front :+ last, arg) =>
          if (optionNames(arg)) (front :+ last) :+ Vector(arg)
          else front :+ (last :+ arg)
        case (Seq(), arg) =>
          assert(optionNames(arg), "first argument is not an option name")
          Vector(Vector(arg))
      }.map {
        case optName +: optArgs => optName -> optArgs
      }.toMap

    val taggerModelFile = options.get("-taggerModelFile").map { case Vector(file) => file }.getOrElse(sys.error("-taggerModelFile must be specified"))
    val filesToTag = options.get("-fileToTag").map { case Vector() => sys.error("-fileToTag must include at least one file"); case files => files }.getOrElse(sys.error("must specify -fileToTag"))

    val memmTagger = MemmTagger.fromFile(taggerModelFile)

    val sentencesToTag = filesToTag.iterator.flatMap(RawFile(_)).toVector
    for (sentence <- sentencesToTag) {
      val tagged = tag(memmTagger, sentence)
      println(tagged.mapt((w, t) => s"$w|$t").mkString(" "))
    }

  }

  def tag(memmTagger: MemmTagger[String, String], sentence: Vector[String]): Vector[(String, String)] = {
    val splitSent = ReaderUtil.splitRawOnPunct(sentence)
    val tagged = splitSent.flatMap(sentence => memmTagger.tagSequence(sentence))
    tagged
  }

}
