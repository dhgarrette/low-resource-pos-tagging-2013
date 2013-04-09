package dhg.pos.lp

import scala.util.matching.Regex
import dhg.util.Pattern.UInt

class LpGraphWord(val word: Option[String]) {
  override def toString = word.map(_.toString).getOrElse("<b>")
  override def hashCode = "LpGraphWord(%s)".format(this.toString).hashCode
  override def equals(other: Any) = other match {
    case o: LpGraphWord => word == o.word
    case _ => false
  }
}
object LpGraphWord {
  def apply[Sym](word: Option[Sym]) = new LpGraphWord(word.map(_.toString))
  def unapply(word: String) = Some(word match { case "<b>" => None; case _ => Some(word) })
}

trait LpFeature

class WordFeat(word: LpGraphWord) extends LpFeature {
  override def toString = "WORD_%s".format(word)
}
object WordFeat {
  val Re = """WORD_([^_]+)""".r
  def apply(word: LpGraphWord) = new WordFeat(word)
  def apply[Sym](word: Option[Sym]) = new WordFeat(LpGraphWord(word))
  def unapply(word: String) = word match {
    case Re(LpGraphWord(word)) => Some(WordFeat(word))
    case _ => None
  }
}

class TokenFeat(val token: LpGraphWord, val sentIdx: Int, val tokenIdx: Int) extends LpFeature {
  override def toString = "TOKEN_%s_%s_%s".format(token, sentIdx, tokenIdx)
}
object TokenFeat {
  val Re = """TOKEN_([^_]+)_(\d+)_(\d+)""".r
  def apply(token: LpGraphWord, sentIdx: Int, tokenIdx: Int) = new TokenFeat(token, sentIdx, tokenIdx)
  //def apply[Sym](word: Option[Sym]) = new TokenFeat(LpGraphWord(word), sentIdx, tokenIdx)
  def unapply(word: String) = word match {
    case Re(LpGraphWord(word), UInt(sentIdx), UInt(tokenIdx)) => Some(word, sentIdx, tokenIdx)
    case _ => None
  }
}

class TagFeat(tag: String) extends LpFeature {
  override def toString = "TAG_%s".format(tag)
}
object TagFeat {
  val Re = """TAG_([^_]+)""".r
  def apply(tag: String) = new TagFeat(tag)
  def unapply(tag: String) = tag match {
    case Re(tag) => Some(TagFeat(tag))
    case _ => None
  }
}

class PrevWordFeat(prevWord: LpGraphWord) extends LpFeature {
  override def toString = "PREVWORD_%s".format(prevWord)
}
object PrevWordFeat {
  val Re = """PREVWORD_([^_]+)""".r
  def apply(word: LpGraphWord) = new PrevWordFeat(word)
  def apply[Sym](word: Option[Sym]) = new PrevWordFeat(LpGraphWord(word))
  def unapply(word: String) = word match {
    case Re(LpGraphWord(word)) => Some(PrevWordFeat(word))
    case _ => None
  }
}

class NextWordFeat(nextWord: LpGraphWord) extends LpFeature {
  override def toString = "NEXTWORD_%s".format(nextWord)
}
object NextWordFeat {
  val Re = """NEXTWORD_([^_]+)""".r
  def apply(word: LpGraphWord) = new NextWordFeat(word)
  def apply[Sym](word: Option[Sym]) = new NextWordFeat(LpGraphWord(word))
  def unapply(word: String) = word match {
    case Re(LpGraphWord(word)) => Some(NextWordFeat(word))
    case _ => None
  }
}

class PrefixFeat(prefix: String, n: Int) extends LpFeature {
  override def toString = "PREFIX%s_%s".format(n, prefix)
}
object PrefixFeat {
  val Re = """PREFIX(\d+)_([^_]+)""".r
  def apply(prefix: String, n: Int) = new PrefixFeat(prefix, n)
  def unapply(word: String) = word match {
    case Re(n, prefix) => Some(PrefixFeat(prefix, n.toInt))
    case _ => None
  }
}

class SuffixFeat(suffix: String, n: Int) extends LpFeature {
  override def toString = "SUFFIX%s_%s".format(n, suffix)
}
object SuffixFeat {
  val Re = """SUFFIX(\d+)_([^_]+)""".r
  def apply(suffix: String, n: Int) = new SuffixFeat(suffix, n)
  def unapply(word: String) = word match {
    case Re(n, suffix) => Some(SuffixFeat(suffix, n.toInt))
    case _ => None
  }
}

class DictposFeat(dictpos: String) extends LpFeature {
  override def toString = "DICTPOS_%s".format(dictpos)
}
object DictposFeat {
  val Re = """DICTPOS_([^_]+)""".r
  def apply(dictpos: String) = new DictposFeat(dictpos)
  def unapply(word: String) = word match {
    case Re(dictpos) => Some(DictposFeat(dictpos))
    case _ => None
  }
}

class MorphFeat(morph: String) extends LpFeature {
  override def toString = "MORPH_%s".format(morph)
}
object MorphFeat {
  val Re = """MORPH_([^_]+)""".r
  def apply(morph: String) = new MorphFeat(morph)
  def unapply(word: String) = word match {
    case Re(morph) => Some(MorphFeat(morph))
    case _ => None
  }
}
