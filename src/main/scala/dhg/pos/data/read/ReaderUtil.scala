package dhg.pos.data.read

import dhg.util.CollectionUtil._

object ReaderUtil {

  val punctRewrites = Map(
    "(" -> "-LRB-",
    ")" -> "-RRB-",
    "[" -> "-LRB-",
    "]" -> "-RRB-",
    "{" -> "-LRB-",
    "}" -> "-RRB-")

  val eosTokens: Set[String] = Set(".", ";", "?", "!")

  def splitRawOnPunct(sentence: Vector[String]): Vector[Vector[String]] = {
    sentence
      .filter(_.nonEmpty)
      .map(_.trim)
      .map(tok => ReaderUtil.punctRewrites.getOrElse(tok, tok))
      .flatMap {
        case tok if eosTokens(tok) => Iterator(tok, "<EOS>")
        case tok => Iterator(tok)
      }
      .iterator.split("<EOS>")
      .filter(_.nonEmpty)
      .toVector
  }

  def splitTaggedOnPunct(sentence: Vector[(String, String)], rewriteTag: (String, String) => String): Vector[Vector[(String, String)]] = {
    sentence
      .map { case (word, pos) => (word.trim, pos.trim) }
      .map {
        case tok @ (w, t) =>
          punctRewrites.get(w).map(newSym => (newSym, newSym))
            .getOrElse((w, rewriteTag(w, t)))
      }
      .flatMap {
        case tok @ (w, t) if eosTokens(w) => Iterator(tok, ("<EOS>", "<EOS>"))
        case tok => Iterator(tok)
      }
      .iterator.split(("<EOS>", "<EOS>"))
      .filter(_.nonEmpty)
      .toVector
  }

}
