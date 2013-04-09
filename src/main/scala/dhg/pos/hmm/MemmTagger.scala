package dhg.pos.hmm

import java.io.BufferedReader
import java.io.StringReader
import dhg.nlp.tag.TagDict
import dhg.util.CollectionUtil._
import dhg.util.Arm._
import dhg.nlp.util.CollectionUtils._
import opennlp.tools.dictionary.Dictionary
import opennlp.tools.postag.POSDictionary
import opennlp.tools.postag.POSSample
import opennlp.tools.postag.POSTaggerME
import opennlp.tools.util.ObjectStreamUtils
import opennlp.tools.util.TrainingParameters
import opennlp.tools.util.model.ModelType
import java.io.FileInputStream
import opennlp.tools.postag.POSModel
import java.io.BufferedOutputStream
import java.io.FileOutputStream

/**
 * Train a MEMM from gold-labeled data.
 *
 * @param maxIterations
 * @param cutoff			"events" must occur at least this many times to be used during training
 */
class MemmTaggerTrainer[Sym, Tag](
  maxIterations: Int = 50,
  cutoff: Int = 100) {

  def trainFromGoldLabeled(
    labeledSequences: Vector[Vector[(Sym, Tag)]]) = {

    val samples = ObjectStreamUtils.createObjectStream(
      labeledSequences.map { s =>
        val (syms, tags) = s.unzip
        new POSSample(syms.map(_.toString).toArray, tags.map(_.toString).toArray)
      }: _*)

    val languageCode = "Uh... any language ???"

    val params = new TrainingParameters()
    params.put(TrainingParameters.ALGORITHM_PARAM, ModelType.MAXENT.toString)
    params.put(TrainingParameters.ITERATIONS_PARAM, maxIterations.toString)
    params.put(TrainingParameters.CUTOFF_PARAM, cutoff.toString)

    val tagDictionary: POSDictionary = null
    //    new POSDictionary(new BufferedReader(new StringReader({
    //      tagDict.setIterator.mapt((sym, tags) => "%s %s\n".format(sym, tags.mkString(" "))).mkString("\n")
    //    })), true)

    val ngramDictionary: Dictionary = null

    val model = POSTaggerME.train(
      languageCode,
      samples,
      params,
      tagDictionary,
      ngramDictionary)
    new MemmTagger[Sym, Tag](model, new POSTaggerME(model))
  }
}

case class MemmTagger[Sym, Tag](model: POSModel, meTagger: POSTaggerME) {
  def tagSequence(sequence: Vector[Sym]): Vector[(String, String)] = {
    val syms = sequence.map(_.toString)
    val tags = meTagger.tag(syms.toArray)
    syms zipSafe tags
  }

  def tag(sequences: Vector[Vector[Sym]]): Vector[Vector[(String, String)]] = {
    sequences.map(tagSequence)
  }
}

object MemmTagger {

  def persistToFile(tagger: MemmTagger[String, String], filepath: String) {
    val model: POSModel = tagger.model
    using(new BufferedOutputStream(new FileOutputStream(filepath))) { modelOut =>
      model.serialize(modelOut)
    }
  }

  def fromFile(filepath: String): MemmTagger[String, String] = {
    val model =
      using(new FileInputStream(filepath)) { modelIn =>
        new POSModel(modelIn)
      }
    new MemmTagger(model, new POSTaggerME(model))
  }

}
