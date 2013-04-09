package dhg.pos.util

import dhg.nlp.util.CollectionUtils._
import dhg.util.CollectionUtil._

object CollectionUtils {

  /**
   * Return 'tokens', unchanged, if it is longer than 'minLength'.
   * Otherwise, return an empty iterator.
   */
  def getIfValidLength(tokens: Iterator[String], minLength: Int): Iterator[String] = {
    val (pre, suf) = tokens.splitAt(minLength)
    val buf = pre.toIndexedSeq
    if (buf.size == minLength)
      buf.iterator ++ suf
    else
      Iterator()
  }

}
