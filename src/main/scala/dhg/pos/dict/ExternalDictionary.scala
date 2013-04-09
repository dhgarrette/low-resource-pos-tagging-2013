package dhg.pos.dict

import dhg.util.CollectionUtil._
import dhg.nlp.util.CollectionUtils._
import dhg.util.Pattern._
import dhg.util.StringUtil._
import dhg.util.FileUtil._
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.BufferedReader
import java.io.InputStreamReader

trait ExternalDictionary {
  def getTags(words: Set[String]): Map[String, Set[String]] = {
    dictIterator.filter { case (dictWord, dictPos) => words(dictWord) }.to[Set].groupByKey
  }

  def dictIterator: Iterator[(String, String)]
}

class BasicExternalDictionary(entries: Map[String, Set[String]]) extends ExternalDictionary {
  override def dictIterator = entries.ungroup
}

class LazyBasicExternalDictionary(path: String) extends ExternalDictionary {
  override def dictIterator = File(path).readLines("UTF-8").map(_.trim.splitWhitespace).flatMap {
    case Array(word, tags @ _*) => tags.map(tag => word -> tag)
    case line => sys.error(s"""Cannot parse dictionary line "$line".  Format must be "WORD TAG TAG ..." (amount of whitespace, spaces or tabs, is fine)""")
  }
}

object EmptyExternalDictionary extends ExternalDictionary {
  override def dictIterator = Iterator()
}

class WiktionaryExternalDictionary(path: String, languageName: String) extends ExternalDictionary {
  // English	aardvark	Noun	# The
  override def dictIterator =
    for (
      line <- GzFileIterator(File(path), "UTF-8");
      Array(language, word, pos, desc) = line.split("\t");
      if (language == languageName)
    ) yield (word, pos.replaceAll("\\s+", "_"))
}

class MuriLexiconExternalDictionary(file: String) extends ExternalDictionary {
  // ::lex guhinda ::synt verb ::inf-prefix gu ::stem-a hinda ::past-mod -nze ::english to [thunder] ::verified false
  val LabelRe = """::(\S+)""".r

  override def dictIterator = {
    File(file).readLines("UTF-8")
      .filterNot(_.startsWith("#"))
      .map { line =>
        val features =
          line.split("\\s+").toSeq
            .flatMap {
              case x @ LabelRe(_) => Seq("", x)
              case x => Seq(x)
            }
            .split("")
            .map(_.filter(_.nonEmpty))
            .filter(_.nonEmpty)
            .map { case LabelRe(l) +: content => (l, content.mkString(" ")) }
            .toMap
        (features("lex"), features("synt"))
      }
  }
}

//class KinyarwandaExternalDictionary extends ExternalDictionary {
//  val file = "data/muri-data/kin/dict/definitions-2012-11-11.xml"
//    
//    import scala.xml._
//    val doc = XML.loadFile(file)
//    
//}

class MalagasyExternalDictionary(path: String) extends ExternalDictionary {
  val EntryLineRe = """<entry word="(\S+).*" pos="([^"]*)">""".r
  // <entry word="tsiboroantantely" pos="noun">

  override def dictIterator = {
    for (
      line <- GzFileIterator(File(path), "UTF-8") if line.startsWith("<entry");
      EntryLineRe(word, posEntry) = line;
      if (posEntry.nonEmpty);
      pos <- extractPosSet(posEntry)
    ) yield (word, pos.replaceAll("\\s+", "_"))
  }

  def extractPosSet(posEntry: String): Set[String] = {
    val ProperName = """proper name \(\S+\)?""".r
    val Noun = """noun \S+""".r
    val Verb = """verb \S+""".r
    val Adj = """adjective \S+""".r

    val splitEntry = posEntry.split(" or | and ").toSet
    val all =
      splitEntry.map(_.trim)
        .map(_
          .replaceAll("feminine ", "")
          .replaceAll("masculine ", "")
          .replaceAll("past of ", "")
          .replaceAll("future of ", "")
          .replaceAll("imperative of ", "")
          .replaceAll("active ", "")
          .replaceAll("relative ", "")
          .replaceAll(", past form", "")
          .replaceAll("first ", "")
          .replaceAll(" (acronym)", "")
          .replaceAll(" (symbol)", "")
          .replaceAll("passive ", ""))
        .map { e =>
          if (Set(
            "noun",
            "verb",
            "adjective",
            "adverb",
            "conjunction",
            "circumfix",
            "particule",
            "pronoun",
            "interjection",
            "prefix",
            "infix",
            "suffix",
            "article",
            "preposition")(e)) Set(e)
          else if (Set(
            "expression",
            "Unspecified")(e)) Set()
          else
            e match {
              case Noun() => Set("noun")
              case Verb() => Set("verb")
              case Adj() => Set("adjective")
              case ProperName() => Set("proper name")
              case "name" => Set("proper name")
              case _ => Set()
            }
        }
    all.flatten
  }

}

object ExternalDictionaryRun {
  def main(args: Array[String]) {
    //    //    //val d = new MalagasyExternalDictionary
    //    //    val d = new MuriLexiconExternalDictionary("data/muri-data/data/Kinyarwanda/prelim/kin-lexicon-02.txt")
    //    //    d.dictIterator foreach println
    //
    //    val kinMuriDict = new MuriLexiconExternalDictionary("data/muri-data/data/Kinyarwanda/prelim/kin-lexicon-02.txt")
    //    val mlgMuriDict = new MalagasyExternalDictionary
    //
    //    val engWikiDict = new WiktionaryExternalDictionary("English")
    //    val mlgWikiDict = new WiktionaryExternalDictionary("Malagasy")
    //
    //    writeUsing(File("data/external-dictionaries/kin.dict"), "UTF-8") { f =>
    //      for ((w, t) <- kinMuriDict.dictIterator)
    //        f.writeLine(s"$w\t$t")
    //    }
    //
    //    writeUsing(File("data/external-dictionaries/mlg.dict"), "UTF-8") { f =>
    //      for ((w, t) <- mlgMuriDict.dictIterator ++ mlgWikiDict.dictIterator)
    //        f.writeLine(s"$w\t$t")
    //    }
    //
    //    writeUsing(File("data/external-dictionaries/eng.dict"), "UTF-8") { f =>
    //      for ((w, t) <- engWikiDict.dictIterator)
    //        f.writeLine(s"$w\t$t")
    //    }

  }
}
