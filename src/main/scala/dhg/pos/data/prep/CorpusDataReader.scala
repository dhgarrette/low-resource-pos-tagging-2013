package dhg.pos.data.prep

import dhg.util.FileUtil._
import dhg.util.CollectionUtil._
import dhg.nlp.util.CollectionUtils._
import dhg.util.Pattern._
import dhg.pos.data.read.ReaderUtil._
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.annotation.tailrec

trait MuriCorpusDataReader {
  val DataDir = "data/muri-data/"

  def LangPrefix: String
  def humanAnnotatedFilesForTest: Set[String]
  def humanAnnotatedFilesForTrain: Set[String]
  def secondTierCorpora = Set[String]()
  def rewriteTag(word: String, tag: String): String

  def fullTagset: Set[String]

  //
  //
  //

  def rawData: IndexedSeq[Iterator[Vector[String]]] = {
    allTokFiles.map { tokFiles =>
      val rawFiles = tokFiles -- allParsedFiles -- humanAnnotatedFilesForTest -- humanAnnotatedFilesForTrain
      rawFiles.iterator.flatMap(readRawFile)
    }
  }

  def devData(supplementFile: Option[String]): Iterator[Vector[(String, String)]] = {
    val devFiles = allParsedFiles
    val parsed = devFiles.iterator.flatMap(readParsedFile)
    val supplement = supplementFile.map(readHumanTaggedFile(_).flatten).getOrElse(Iterator())
    parsed ++ supplement
  }

  def testData(testFile: String): Iterator[Vector[(String, String)]] = {
    val data = readHumanTaggedFile(testFile).flatten.toVector

    for ((w, t) <- data.flatten)
      assert(t != "X" && t != "x", "Found %s".format((w, t)))

    data.iterator
  }

  //
  // Get sets of filenames
  //

  def allParsedFiles = {
    val FnRe = """(.+)\.%s\.tree""".format(LangPrefix).r
    for (
      corpusDir <- File(DataDir, LangPrefix, "parsed").listFiles;
      FnRe(file) <- corpusDir.listFiles.map(_.getName)
    ) yield file
  }.toSet

  def allTokFiles: IndexedSeq[Set[String]] = {
    val FnRe = """(.+)\.%s\.tok""".format(LangPrefix).r
    val allFiles =
      for (
        corpusDir <- File(DataDir, LangPrefix, "tok").listFiles.iterator;
        tier = if (secondTierCorpora contains corpusDir.getName) 2 else 1;
        FnRe(file) <- corpusDir.listFiles.map(_.getName)
      ) yield (tier, file)
    val byTier = allFiles.groupByKey
    (1 to 2).map(tier => byTier.getOrElse(tier, Set()).toSet)
  }

  def allOrigFiles = {
    val FnRe = """(.+)\.xml""".r
    for (
      corpusDir <- File(DataDir, LangPrefix, "orig").listFiles;
      FnRe(file) <- corpusDir.listFiles.map(_.getName)
    ) yield file
  }.toSet

  //
  // Read files
  //

  def findFile(dir: String, filename: String) = {
    File(DataDir, LangPrefix, dir).listFiles.flatMap(_.listFiles).find(_.getName == filename).getOrElse {
      sys.error("could not find file [%s] in [%s]".format(filename, pathjoin(DataDir, LangPrefix, dir)))
    }
  }

  // Raw

  def readRawFile(filename: String) = {
    val file = findFile("tok", "%s.%s.tok".format(filename, LangPrefix))
    file.readLines("UTF-8").flatMap(processRawLine)
  }

  def processRawLine(line: String) = {
    splitRawOnPunct(line.split("\\s+").toVector)
  }

  // Tagged

  def readParsedFile(filename: String) = {
    val file = findFile("parsed", "%s.%s.tree".format(filename, LangPrefix))
    file.readLines("UTF-8").flatMap(processTreeLine)
  }

  def processTreeLine(line: String) = {
    SyntaxTreeFastParser(line).flatMap(splitTaggedOnPunct(_, rewriteTag))
  }

  def readHumanTaggedFile(file: String): Iterator[Vector[Vector[(String, String)]]] = {
    val allLines = File(file).readLines("UTF-8")
    val blocked = allLines.splitWhere(_.trim.startsWith("%% After "))
    val cleanBlocked = blocked.map(_.map(_.trim).filter(_.nonEmpty)).filter(_.nonEmpty)
    cleanBlocked.map { cleanBlock =>
      (for (
        line <- cleanBlock if line.startsWith("(TOP ");
        sent <- processTreeLine(line)
      ) yield {
        sent
          .map { case (w, t) => (w, rewriteTag(w, t)) }
          .mapVals(pos => if (pos == "x" || fullTagset(pos)) pos else "X")
          .map {
            case (word, "x") => None
            case tok => Some(tok)
          }.toVector match {
            case s if s.forall(_.isDefined) => Some(s.flatten)
            case s if s.forall(_.isEmpty) => None
          }
      }).flatten
    }.filter(_.nonEmpty)
  }

  def readHumanTagdictFile(file: String): Iterator[Vector[(String, String)]] = {
    val allLines = File(file).readLines("UTF-8")
    val blocked = allLines.splitWhere(_.trim.startsWith("%% After "))
    val cleanBlocked = blocked.map(_.map(_.trim).filter(_.nonEmpty)).filter(_.nonEmpty)
    cleanBlocked.map { cleanBlock =>
      val lines = cleanBlock.map(_.split("\\s+").toList).sortBy(_.head)
      for (
        w :: tags <- lines;
        t <- tags;
        rt = rewriteTag(w, t) if fullTagset(rt)
      ) yield (w, rt)
    }.filter(_.nonEmpty)
  }

  //  def checkHumanAnnotated() = {
  //    val OrigFnRe = """(.+_\d{4}\..+\.tok)""".r // kgmc_0004.kin.tok
  //    val TreeRe = """\(TOP (.+)\)""".r
  //    val TERMINAL_RE = """\((\S+) (\S+)\)""".r
  //    var origFile = ""
  //    import collection.mutable.Buffer
  //    val m = collection.mutable.Map[String, Buffer[Vector[Option[(String, String)]]]]()
  //    for (line <- readLines(humanAnnotatedFile, "UTF-8")) {
  //      line.trim match {
  //        case OrigFnRe(fn) => origFile = fn
  //        case "" =>
  //        case TreeRe(line) =>
  //          val s =
  //            TERMINAL_RE.findAllIn(line).matchData.map(_.subgroups).map {
  //              case Seq("x", word) => None
  //              case Seq(pos, word) => Some(word.trim, pos.trim)
  //            }.toVector
  //          m.getOrElseUpdate(origFile, Buffer()) += s
  //      }
  //    }
  //    for ((ofn, lines) <- m.toVector.sortBy(_._1)) {
  //      val a = lines.flatten.size
  //      val b = lines.count(_.forall(_.isDefined))
  //      val c = lines.count(_.exists(_.isDefined))
  //      val d = lines.size
  //      println("%s %s %s %s %s %s".format(ofn, a, b, c, d, if (b + c > 0) "***" else ""))
  //    }
  //  }
}

object SyntaxTreeParser extends RegexParsers {
  private def label = regex("""\S+""".r)
  private def subtree: Parser[Vector[(String, String)]] = ("(" ~ label ~> subtreeSeq <~ ")") | ("(" ~> label ~ label <~ ")").map { case t ~ w => Vector((w, t)) }
  private def subtreeSeq: Parser[Vector[(String, String)]] = (subtree ~ subtreeSeq).map { case st ~ sts => st ++ sts } | subtree
  private def sentLabel = regex("""SFRAG|S""".r)
  private def sentTree: Parser[Vector[(String, String)]] = ("(" ~> sentLabel ~> subtreeSeq <~ ")")
  private def sentTreeSeq: Parser[Vector[Vector[(String, String)]]] = (sentTree ~ sentTreeSeq).map { case st ~ sts => st +: sts } | sentTree.map(Vector(_))
  private def toplevel: Parser[Vector[Vector[(String, String)]]] = ("(" ~> "TOP" ~> sentTreeSeq <~ ")") | sentTreeSeq
  def parse(str: String): ParseResult[Vector[Vector[(String, String)]]] = parseAll(toplevel, str.replaceAll("\\(", " ( ").replaceAll("\\)", " ) "))
  def apply(str: String): Vector[Vector[(String, String)]] = SyntaxTreeParser.parse(str) match {
    case SyntaxTreeParser.Success(result, _) => result
    case _ => sys.error("Could not parse the input string: " + str)
  }
}

object SyntaxTreeFastParser extends (String => Vector[Vector[(String, String)]]) {
  val TerminalRe = """(\S+)""".r

  def apply(str: String): Vector[Vector[(String, String)]] = {

    val fixed = str match {
      case "(TOP (S (NP (NP (DT Ny) (NP (N 29) (N septambra) ) ) (ADJP (ADJ teo) ) ) (VP (FOCP (FOC no) (VP (NP (NP (NP (N andro) ) (ADJP (ADJ iraisam) (@-@ @-@) (NP (N pirenena) ) ) ) (CP (VP (VP (V natokana) ) (PP (P hoan') (NP (DT ny) (N haromontana) ) ) ) ) ) ) ) ) ) (. .) )" =>
        "(TOP (S (NP (NP (DT Ny) (NP (N 29) (N septambra) ) ) (ADJP (ADJ teo) ) ) (VP (FOCP (FOC no) (VP (NP (NP (NP (N andro) ) (ADJP (ADJ iraisam) (@-@ @-@) (NP (N pirenena) ) ) ) (CP (VP (VP (V natokana) ) (PP (P hoan') (NP (DT ny) (N haromontana) ) ) ) ) ) ) ) ) (. .) ) )"
      case "(TOP (S (VP (NP (N Malagasy) ) (PCL ve) ) (NP (DT ianao) ) (? ?) ) ) )" =>
        "(TOP (S (VP (NP (N Malagasy) ) (PCL ve) ) (NP (DT ianao) ) (? ?) ) )"
      case "(TOP (S (VP (VP (V Mipetraka) (PP (P akaikin) (@-@ @-@) (NP (N dRabe) ) (NP (N Ranaivo) ) (. .) ) )" =>
        "(TOP (S (VP (VP (V Mipetraka) (PP (P akaikin) (@-@ @-@) (NP (N dRabe) ) (NP (N Ranaivo) ) (. .) ) ) ) ) )"
      case "(TOP (S (VP (V Tokony) (VP (V hijery) (NP (DT ny) (NP (N toe) (@-@ @-@) (N batany) ) ) ) ) (NP (NP (DT ny) (N olona) ) (CP (S (VP (ADVP (ADV efa) ) (VP (V mananika) (NP (DT ny) (NP (ADJP (ADJ 40) ) (N taona) ) ) ) ) ) ) ) (, ,) ) (S (S (VP (NEGP (NEG tsy) ) (VP (V midika) (ADVP (ADV akory) ) ) ) (NP (N izany) ) ) (CP (C fa) (S (VP (VP (V atao) (NP (N tsirambina) ) ) (NP (NP (DT ny) (N an') ) (NP (DT ny) (N hafa) ) ) ) ) ) ) (. .) )" =>
        "(TOP (S (VP (V Tokony) (VP (V hijery) (NP (DT ny) (NP (N toe) (@-@ @-@) (N batany) ) ) ) ) (NP (NP (DT ny) (N olona) ) (CP (S (VP (ADVP (ADV efa) ) (VP (V mananika) (NP (DT ny) (NP (ADJP (ADJ 40) ) (N taona) ) ) ) ) ) ) ) (, ,) ) (S (S (VP (NEGP (NEG tsy) ) (VP (V midika) (ADVP (ADV akory) ) ) ) (NP (N izany) ) ) (CP (C fa) (S (VP (VP (V atao) (NP (N tsirambina) ) ) (NP (NP (DT ny) (N an') ) (NP (DT ny) (N hafa) ) ) ) ) ) (. .) ) )"
      case _ => str
    }

    val tokens = fixed.replaceAll("\\(", " ( ").replaceAll("\\)", " ) ").split("\\s+").map(_.trim).filter(_.nonEmpty).toList

    def parseToplevel(tokens: List[String]) = {
      tokens match {
        case "(" :: "TOP" :: tokens2 =>
          val (sentSeq, tokens3) = parseSentenceSeq(tokens2)
          val ")" :: Nil = tokens3
          sentSeq
        case _ =>
          val (sentSeq, Nil) = parseSentenceSeq(tokens)
          sentSeq
      }
    }

    def parseSentenceSeq(tokens: List[String]): (Vector[Vector[(String, String)]], List[String]) = {
      val (head, tokens1) = parseSentence(tokens)
      tokens1 match {
        case "(" :: _ =>
          val (tail, tokens2) = parseSentenceSeq(tokens1)
          (head +: tail, tokens2)
        case _ =>
          (Vector(head), tokens1)
      }
    }

    def parseSentence(tokens: List[String]): (Vector[(String, String)], List[String]) = {
      val pairs = Vector[(String, String)]()
      def doIteration(tokens: List[String], pairs: Vector[(String, String)], parenCount: Int): (Vector[(String, String)], List[String]) = {
        (tokens, parenCount) match {
          case (")" :: tokens2, 1) =>
            (pairs, tokens2)
          case (")" :: tokens2, _) =>
            doIteration(tokens2, pairs, parenCount - 1)
          case ("(" :: TerminalRe(t) :: TerminalRe(w) :: ")" :: tokens2, 0) =>
            (Vector((w, t)), tokens2)
          case ("(" :: TerminalRe(t) :: TerminalRe(w) :: ")" :: tokens2, _) =>
            doIteration(tokens2, pairs :+ (w, t), parenCount)
          case ("(" :: TerminalRe(n) :: tokens2, _) =>
            doIteration(tokens2, pairs, parenCount + 1)
        } // (A (x x)) (B (y y))
      }
      doIteration(tokens, pairs, 0)
    }

    try {
      parseToplevel(tokens)
    }
    catch {
      case e: MatchError => throw new RuntimeException(s"Could not parse: $str", e)
    }
  }
}

object MuriKinCleaner extends MuriCorpusDataReader {
  override val LangPrefix = "kin"
  override val fullTagset = ", . ADJ ADV C CC DT N PREP RP V".split("\\s+").toSet

  override val humanAnnotatedFilesForTest = Set(
    "kgmc_0004", // definitely used (kin-testdata.txt)
    "kgmc_0011" // definitely used (kin-testdata.txt)
    )

  override val humanAnnotatedFilesForTrain = Set(
    "kgmc_0013", // candidate (kin-sentences-annotator1-4hr.txt)
    "kgmc_0360", // candidate (kin-sentences-annotator1-4hr.txt)
    "kgmc_0363", // candidate (kin-sentences-annotator1-4hr.txt)
    "kgmc_0364", // candidate (kin-sentences-annotator1-4hr.txt)
    "kgmc_0365", // candidate (kin-sentences-annotator1-4hr.txt)
    "kgmc_0369" //  candidate (kin-sentences-annotator1-4hr.txt)
    )

  override def rewriteTag(word: String, tag: String) = (word, tag) match {
    case ("kuri", "RP") => "PREP"
    case ("ntashobora", "I") => "V"
    case ("arabiyi", "I") => "N"
    case (_, "CONJ") => "CC"
    case (_, "D") => "DT"
    case (_, "P") => "PREP"
    case (_, "VP") => "V"
    case (_, "!") => "."
    case (_, "?") => "."
    case (_, ";") => "."
    case (_, "...") => "."
    case (w, t) => t
  }
}

object MuriMlgCleaner extends MuriCorpusDataReader {
  override val LangPrefix = "mlg"
  override val secondTierCorpora = Set("mlg_bible")
  override val fullTagset = "\" , -LRB- -RRB- . ... : @-@ ADJ ADV C CONJ DT FOC N NEG PCL PN PREP PRO T V".split("\\s+").toSet

  override val humanAnnotatedFilesForTest = Set(
    "gv_0001", // definitely used (mlg-testdata.txt)
    "gv_0002", // definitely used (mlg-testdata.txt)
    "gv_0003", // definitely used (mlg-testdata.txt)
    "gv_0004", // definitely used (mlg-testdata.txt)
    "gv_0005", // definitely used (mlg-testdata.txt)
    "gv_0006", // definitely used (mlg-testdata.txt)
    "gv_0007", // definitely used (mlg-testdata.txt)
    "gv_0008" //  definitely used (mlg-testdata.txt)
    )

  override val humanAnnotatedFilesForTrain = Set(
    "gv_0009", // candidate (mlg-sentences-annotator2-4hr.txt)
    "gv_0010", // candidate (mlg-sentences-annotator2-4hr.txt)
    "gv_0011", // candidate (mlg-sentences-annotator2-4hr.txt)
    "gv_0012", // candidate (mlg-sentences-annotator2-4hr.txt)
    "gv_0013", // candidate (mlg-sentences-annotator2-4hr.txt)
    "gv_0014", // candidate (mlg-sentences-annotator2-4hr.txt)
    "gv_0015", // candidate (mlg-sentences-annotator2-4hr.txt)
    "gv_0016", // candidate (mlg-sentences-annotator2-4hr.txt)
    "gv_0017", // candidate (mlg-sentences-annotator2-4hr.txt)
    "gv_0018", // candidate (mlg-sentences-annotator2-4hr.txt)
    "gv_0019", // candidate (mlg-sentences-annotator2-4hr.txt)
    "gv_0020" //  candidate (mlg-sentences-annotator2-4hr.txt)
    )

  override def rewriteTag(word: String, tag: String) = (word, tag) match {
    case (_, "D") => "DT"
    case (_, "NN") => "N"
    case (_, "NNP") => "N"
    case (_, "P") => "PREP"
    case (_, "VP") => "V"
    case (_, "!") => "."
    case (_, "?") => "."
    case (_, ";") => "."
    case (w, t) => t
  }
}

object MuriCorpusDataReaderRun {
  def main(args: Array[String]) {
    // kin
    // total raw1 tokens = 206492
    // total raw2 tokens = 0
    // total dev tokens = 3294

    // mlg
    // total raw1 tokens = 6489
    // total raw2 tokens = 773999
    // total dev tokens = 2407

    for (
      (langId, r: MuriCorpusDataReader) <- Vector(
        ("kin", MuriKinCleaner),
        ("mlg", MuriMlgCleaner))
    ) {

      println(langId)

      //val origFiles = r.allOrigFiles
      val Seq(tok1Files, tok2Files) = r.allTokFiles
      val parsedFiles = r.allParsedFiles

      assert(tok1Files.nonEmpty)
      assert((tok1Files intersect tok2Files).isEmpty)
      //assert(origFiles == (tok1Files union tok2Files))
      assert(parsedFiles.subsetOf(tok1Files union tok2Files))

      println("eval data:     " + r.fullTagset.toVector.sorted.mkString(" "))
      //      println("human tagdict: " + r.humanTagdict.map(_._2).toSet.toVector.sorted.mkString(" "))
      //      println("human tagged:  " + r.humanTaggedSentences.flatten.map(_._2).toSet.toVector.sorted.mkString(" "))

      //      val Seq(raw1, raw2) = r.rawData
      //      println("total raw1 tokens = " + raw1.flatten.size)
      //      println("total raw2 tokens = " + raw2.flatten.size)
      //      println("total dev tokens = " + r.devData.flatten.size)

      println
    }

  }
}
