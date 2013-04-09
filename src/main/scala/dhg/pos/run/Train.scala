package dhg.pos.run

import dhg.util.CollectionUtil._
import dhg.nlp.util.CollectionUtils._
import dhg.util.Pattern.{ UBoolean, UInt }
import dhg.util.FileUtil._
import dhg.util.Time._
import dhg.util.StringUtil._
import dhg.nlp.tag.hmm.HmmUtils
import dhg.nlp.tag.SimpleTagDict
import dhg.pos.fst.FomaSimpleFst
import dhg.pos.fst.XfstSimpleFst
import dhg.pos.dict.BasicExternalDictionary
import dhg.pos.lp.graph.WordMorphEdgeExtractor
import dhg.pos.lp.graph.TokenNextEdgeExtractor
import dhg.pos.lp.graph.TokenPrevEdgeExtractor
import dhg.pos.lp.graph.WordPrefixEdgeExtractor
import dhg.pos.lp.JuntoTagger
import dhg.pos.dict.ExternalDictionary
import dhg.pos.lp.graph.WordSuffixEdgeExtractor
import dhg.pos.lp.graph.TokenWordEdgeExtractor
import dhg.pos.fst.SimpleFst
import dhg.pos.lp.graph.WordDictposEdgeExtractor
import dhg.pos.lp.graph.TokenLpGraphBuilder
import dhg.nlp.tag.TagDict
import dhg.pos.minmodel.GreedyBigramPickingTagger
import dhg.pos.minmodel.TagBigramSelector
import dhg.pos.minmodel.NewWordTagPairMinimizingTagBigramSelector
import dhg.pos.minmodel.CurrentWeightModelMinTagEdgeScorer
import dhg.pos.minmodel.ModelMinTagEdgeScorer
import dhg.pos.minmodel.ModelMinimizerStage2Impl
import dhg.pos.minmodel.ModelMinimizerStage1Impl
import dhg.pos.minmodel.CoverageMaximizingTagBigramSelector
import dhg.pos.minmodel.HackyEdgeScoreSummingTagBigramSelector
import dhg.pos.minmodel.HackyNodeScoreSummingTagBigramSelector
import dhg.pos.minmodel.SummingModelMinTagEdgeScorer
import dhg.pos.minmodel.PassthroughTagBigramSelector
import dhg.pos.minmodel.SummingDoubleCoveredWordResolver
import dhg.nlp.tag.hmm.EmHmmTaggerTrainer
import dhg.nlp.tag.hmm.TransitionCountsTransformer
import dhg.nlp.tag.hmm.HardTagDictConstraintHmmTaggerFactory
import dhg.nlp.tag.hmm.EmissionCountsTransformer
import dhg.nlp.freq.EisnerSmoothingCondCountsTransformer
import dhg.nlp.freq.AddLambdaSmoothingCountsTransformer
import scalaz._
import Scalaz._
import dhg.nlp.tag.TaggerEvaluator
import dhg.pos.util.CorpusUtils.TaggedFile
import dhg.pos.util.CorpusUtils.RawFile
import dhg.pos.hmm.MemmTaggerTrainer
import dhg.pos.hmm.MemmTagger

/**
 * CLI for training a tagger model using a semi-supervised procedure that is
 * able to generalize from a small amount of annotation.
 *
 * -rawDataFile FILEPATH ...
 * Files of raw sentences.  Each file should be tokenized, with space-separated
 * tokens, and one sentence per line.
 *
 * -rawDataNumTokens NUMBER
 * Number of tokens to use from the raw data.  Token counting starts at the top of
 * the first raw file and stops after the last complete sentence such that the total
 * number of tokens is less than or equal to NUMBER.
 *
 * -typeAnnotationFile FILEPATH ..., default: not used
 * Files of type-supervised words.  The contents should be whitespace-separated
 * WORD|TAG pairs.
 *
 * -tokenAnnotationFile FILEPATH ..., default: not used
 * Files of token-supervised sentences.  Each line should contain a tokenized sentence,
 * with tokens separated by whitespace, and each token paired with its POS tag in the
 * form: WORD|TAG.
 *
 * -useAffixes (true|false), default: true
 * Use affix features in the LP graph?
 *
 * -fstFile FILEPATH (foma|xfst), default: not used
 * Point to either a Foma or XFST compile FST file and specify the application.
 *
 * -externalDictFile FILEPATH ..., default: not used
 * Any external dictionary that should be used in the LP graph.  File should be
 * a simple text file with words in the first column, each followed by one or more
 * potential POS tags.
 *
 * -useModelMin (true|false), default: true
 * Use the model minimization procedure after running label propagation?
 *
 * -evaluationDataFile FILEPATH ..., default: not used
 * Files of annotated sentences for evaluation.  Each line should contain a tokenized
 * sentence, with tokens separated by whitespace, and each token paired with its POS
 * tag in the form: WORD|TAG.
 *
 * -outputModelFile FILEPATH, default: not used
 * This is the location to which the trained model will be saved.
 *
 */
object Train {
  def main(args: Array[String]) {

    val optionNames = Set(
      "-rawDataFile",
      "-rawDataNumTokens",
      "-typeAnnotationFile",
      "-tokenAnnotationFile",
      "-useAffixes",
      "-fstFile",
      "-externalDictFile",
      "-useModelMin",
      "-evaluationDataFile",
      "-outputModelFile")

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

    println(options)

    //
    // Process the CLI options
    //

    val rawDataFiles = options.get("-rawDataFile").map { case Vector() => sys.error("-rawDataFile must include at least one file"); case files => files }.getOrElse(sys.error("must specify -rawDataFile"))
    val rawDataNumTokens = options.get("-rawDataNumTokens").map { case Vector(UInt(i)) => i }.getOrElse(sys.error("must specify -rawDataNumTokens"))
    val typeAnnotationFiles = options.getOrElse("-typeAnnotationFile", Vector())
    val tokenAnnotationFiles = options.getOrElse("-tokenAnnotationFile", Vector())
    val useAffix = options.get("-useAffixes").map { case Vector() => true; case Vector(UBoolean(b)) => b }.getOrElse(true)
    val fstFileAndType = options.get("-fstFile").map { case Vector(f, t) => f -> t; case _ => sys.error("-fstFile expects FILEPATH followed by TYPE ('foma' or 'xfst')") }
    val externalDictFiles = options.getOrElse("-externalDictFile", Vector())
    val useModelMin = options.get("-useModelMin").map { case Vector() => true; case Vector(UBoolean(b)) => b }.getOrElse(true)
    val evaluationDataFiles = options.getOrElse("-evaluationDataFile", Vector())
    val outputModelFile = options.get("-outputModelFile").map { case Vector(file) => file }
    assert(evaluationDataFiles.nonEmpty || outputModelFile.isDefined, "At least one of -evaluationDataFile or -outputModelFile should be defined")

    //
    // Load the data
    //

    val rawData = rawDataFiles.iterator.flatMap(RawFile(_)).takeSub(rawDataNumTokens).toVector
    val typeAnnotations = typeAnnotationFiles.flatMap(TaggedFile(_)).flatten.toSet
    val taggedSentences = tokenAnnotationFiles.flatMap(TaggedFile(_))
    val initialTagDictData = (typeAnnotations ++ taggedSentences.flatten).groupByKey

    {
      println(s"Raw data: ${rawData.size} sentences, ${rawData.flatten.size} tokens")
      val labeledTestData = evaluationDataFiles.flatMap(TaggedFile(_))
      val testTD = labeledTestData.flatten.toSet
      val typpr =
        if (labeledTestData.nonEmpty) {
          val annotationTD = typeAnnotations
          f"P=${(annotationTD & testTD).size / annotationTD.size.toDouble}%.2f, R=${(annotationTD & testTD).size / testTD.size.toDouble}%.2f"
        }
        else ""
      println(s"Type annotations: ${typeAnnotations.size} tagdict entries, $typpr")
      val tokpr =
        if (labeledTestData.nonEmpty) {
          val annotationTD = taggedSentences.flatten.toSet
          f"P=${(annotationTD & testTD).size / annotationTD.size.toDouble}%.2f, R=${(annotationTD & testTD).size / testTD.size.toDouble}%.2f"
        }
        else ""
      println(s"Token annotations: ${taggedSentences.size} sentences, ${taggedSentences.flatten.size} tokens, ${taggedSentences.flatten.toSet.size} tagdict entries, $tokpr")
    }

    //
    // Train a Model
    //

    val (transitionPseudocounts, emissionPseudocounts) = HmmUtils.getCountsFromTagged(taggedSentences)
    val initialTagDict = SimpleTagDict(initialTagDictData.ungroup.filter(_._2 != "X").groupByKey(Set.newBuilder[String]))
    val fullTagSet = initialTagDict.allTags

    val (binaryExpandedTagDict, rawTokenTagDistsWithDefaults) = time("Tag Dictionary Expansion", {
      val fst = fstFileAndType.map {
        case (fstFile, "foma") => new FomaSimpleFst(fstFile)
        case (fstFile, "xfst") => new XfstSimpleFst(fstFile)
      }
      val extDict = externalDictFiles match {
        case Vector() => None
        case fns => time("load external tag dict", {
          val dictEntries = fns.iterator.flatMap(fn => File(fn).readLines("UTF-8")).map(_.splitWhitespace.toVector).flatMap { case w +: ts => ts.map(w -> _) }.groupByKey(Set.newBuilder[String])
          Some(new BasicExternalDictionary(dictEntries))
        })
      }
      val rawTokenTagDists = time1("Finished label propagation", junto(fst, useAffix, extDict).tagFromAnnotations(taggedSentences, initialTagDict, rawData, threshold = 0.1, None))
      val lpOutputTdEntries = rawTokenTagDists.flatten.groupByKey.mapVals(_.flatten.map(_._1).toSet).filter(_._2.nonEmpty)
      val binaryExpandedTagDict: TagDict[String, String] = SimpleTagDict(lpOutputTdEntries ++ initialTagDict.setIterator, fullTagSet)
      val rawTokenTagDistsWithDefaults = addDefaultsForEmptyNodes(rawTokenTagDists, initialTagDict, fullTagSet)
      (binaryExpandedTagDict, rawTokenTagDistsWithDefaults)
    })

    val noisyLabeledRawCorpus =
      if (useModelMin) time("Finished model minimization", minimize(rawTokenTagDistsWithDefaults, taggedSentences))
      else rawTokenTagDistsWithDefaults.submap { case (word, tags) => word -> tags.maxBy(_._2)._1 }

    val memmTrainer = new MemmTaggerTrainer[String, String](maxIterations = 1000)
    val hmmTagger = time1("Finished HMM training", trainWithNoisilyLabeledSentences(binaryExpandedTagDict, fullTagSet, noisyLabeledRawCorpus, transitionPseudocounts, emissionPseudocounts, rawData))
    val autoTagged = hmmTagger.tag(rawData)
    val memmTagger = time1("Finished MEMM training", memmTrainer.trainFromGoldLabeled(taggedSentences ++ autoTagged))

    //
    //Perform Evaluation
    //

    val labeledTestData = evaluationDataFiles.flatMap(TaggedFile(_))
    if (labeledTestData.nonEmpty) {
      val rawTestData = labeledTestData.submap(_._1)
      val taggerOutput = memmTagger.tag(rawTestData)

      val inputTagDictEntries = binaryExpandedTagDict.setIterator.ungroup.toSet
      val outputTagDictEntries = taggerOutput.flatten.toSet
      val goldTagDictEntries = labeledTestData.flatten.toSet
      println("input tag dict precision  = %.2f (%d/%d)".format((inputTagDictEntries & goldTagDictEntries).size.toDouble * 100 / inputTagDictEntries.size, (inputTagDictEntries & goldTagDictEntries).size, inputTagDictEntries.size))
      println("input tag dict recall     = %.2f (%d/%d)".format((inputTagDictEntries & goldTagDictEntries).size.toDouble * 100 / goldTagDictEntries.size, (inputTagDictEntries & goldTagDictEntries).size, goldTagDictEntries.size))
      println("output tag dict precision = %.2f (%d/%d)".format((outputTagDictEntries & goldTagDictEntries).size.toDouble * 100 / outputTagDictEntries.size, (outputTagDictEntries & goldTagDictEntries).size, outputTagDictEntries.size))
      println("output tag dict recall    = %.2f (%d/%d)".format((outputTagDictEntries & goldTagDictEntries).size.toDouble * 100 / goldTagDictEntries.size, (outputTagDictEntries & goldTagDictEntries).size, goldTagDictEntries.size))

      val evaluator = new TaggerEvaluator[String, String]()
      val results = evaluator.evaluate(taggerOutput, labeledTestData, initialTagDict)
      println(results)
    }

    //
    // Save tagger to file for later use
    //

    outputModelFile.foreach { filepath =>
      MemmTagger.persistToFile(memmTagger, filepath)
    }
  }

  def junto(fst: Option[SimpleFst[String, Vector[String]]], useAffix: Boolean, dict: Option[ExternalDictionary]) = {
    val basicExtractors = Vector(
      TokenWordEdgeExtractor[String](),
      TokenPrevEdgeExtractor[String](),
      TokenNextEdgeExtractor[String]())
    val affixExtractors =
      if (useAffix) Vector(
        WordPrefixEdgeExtractor[String](5),
        WordSuffixEdgeExtractor[String](5))
      else Vector()
    val morphExtractors =
      fst.map(f => WordMorphEdgeExtractor[String](f))
    val dictExtractors =
      dict.map(d => WordDictposEdgeExtractor[String](d))

    new JuntoTagger[String, String](
      new TokenLpGraphBuilder(basicExtractors ++ affixExtractors ++ morphExtractors ++ dictExtractors),
      maxIterations = 200)
  }

  def addDefaultsForEmptyNodes(
    rawTokenTagDists: Vector[Vector[(String, Map[String, Double])]],
    smallBinaryTagDict: TagDict[String, String],
    fullTagset: Set[String]) = {

    println("Number of untagged tokens from LP = " + rawTokenTagDists.flatten.count(_._2.isEmpty))

    //val default: Map[String, Double] = fullTagset.mapToVal(1.0 / fullTagset.size).toMap
    val default: Map[String, Double] = {
      val wordCounts = rawTokenTagDists.flatten.map(_._1).counts
      val tagProbs =
        wordCounts.iterator
          .flatMap {
            case (word, count) =>
              val tags = smallBinaryTagDict.set(word)
              val perTagCount = count.toDouble / tags.size
              tags.mapToVal(perTagCount)
          }
          .groupByKey
          .mapVals(_.sum)
      val tagOpenness =
        smallBinaryTagDict.setIterator
          .flatMap(_._2)
          .counts
          .mapVals(x => x * x) // square the openness score
      tagProbs
        .map { case (tag, pTag) => (tag, tagOpenness(tag) * pTag) }
        .normalizeValues
    }
    //println("Default tag dist: " + default)

    rawTokenTagDists.map(_.mapt((word, tags) =>
      word -> {
        val ts = if (tags.nonEmpty) tags else default
        smallBinaryTagDict
          .doGetSet(word)
          .map { tde =>
            val filtered = ts.filterKeys(tde)
            if (filtered.nonEmpty) filtered.normalizeValues
            else default.filterKeys(tde).normalizeValues
          }
          .getOrElse(ts)
      }))
  }

  def minimize(
    rawTokenTagDists: Vector[Vector[(String, Map[String, Double])]],
    labeledData: Vector[Vector[(String, String)]]): Vector[Vector[(String, String)]] = {

    // 6: Greedy Bigram Selection: weightSum/(numNewWordTagPairs+1)

    val getHolesOptimization = true
    val pctSentToComplete = 100
    val findPathAtEnd = false

    val viterbiEdgeScorer: ModelMinTagEdgeScorer[String, String] = new CurrentWeightModelMinTagEdgeScorer(1.0)
    val stage1BigramSelector: TagBigramSelector[String, String] =
      new NewWordTagPairMinimizingTagBigramSelector(
        new CoverageMaximizingTagBigramSelector(
          new HackyNodeScoreSummingTagBigramSelector(new SummingDoubleCoveredWordResolver, divisorScaleFactor = 1.0,
            new PassthroughTagBigramSelector, noneWeight = 1.0)))
    val stage2BigramSelector: TagBigramSelector[String, String] =
      new NewWordTagPairMinimizingTagBigramSelector(
        new CoverageMaximizingTagBigramSelector(
          new HackyEdgeScoreSummingTagBigramSelector(new SummingModelMinTagEdgeScorer(noneWeight = 1.0), divisorScaleFactor = 1.0,
            new PassthroughTagBigramSelector)))

    val minimizer =
      new GreedyBigramPickingTagger(
        new ModelMinimizerStage1Impl(stage1BigramSelector, viterbiEdgeScorer, labeledData),
        new ModelMinimizerStage2Impl(stage2BigramSelector, viterbiEdgeScorer, getHolesOptimization, pctSentToComplete),
        viterbiEdgeScorer, findPathAtEnd)

    minimizer.tag(rawTokenTagDists)
  }

  def trainWithNoisilyLabeledSentences(
    binaryTagDict: TagDict[String, String],
    tagset: Set[String],
    noisyTaggedSequences: Vector[Vector[(String, String)]],
    priorTransitionCounts: Map[Option[String], Map[Option[String], Double]], priorEmissionCounts: Map[Option[String], Map[Option[String], Double]],
    rawCorpus: Vector[Vector[String]]) = {

    val trainer =
      new EmHmmTaggerTrainer[String, String](
        transitionCountsTransformer =
          new TransitionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))),
        emissionCountsTransformer =
          new EmissionCountsTransformer(
            EisnerSmoothingCondCountsTransformer(lambda = 1.0, backoffCountsTransformer = AddLambdaSmoothingCountsTransformer(lambda = 1.0))),
        hmmTaggerFactory = new HardTagDictConstraintHmmTaggerFactory(binaryTagDict.opt),
        maxIterations = 50,
        minAvgLogProbChangeForEM = 1.0E-10)

    val (transitionNoisyCounts, emissionNoisyCounts) = HmmUtils.getCountsFromTagged(noisyTaggedSequences)
    val initialHmm = trainer.supervisedHmmTaggerTrainer.makeTagger(
      transitionNoisyCounts |+| priorTransitionCounts,
      emissionNoisyCounts |+| priorEmissionCounts)
    trainer.trainFromInitialHmm(rawCorpus, initialHmm, binaryTagDict, priorTransitionCounts, priorEmissionCounts)
  }

}
