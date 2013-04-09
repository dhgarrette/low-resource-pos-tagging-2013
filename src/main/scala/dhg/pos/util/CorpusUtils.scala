package dhg.pos.util

import dhg.util.CollectionUtil._
import dhg.util.FileUtil._
import dhg.util.StringUtil._

object CorpusUtils {

  object TaggedFile {
    def apply(filename: String): Iterable[Vector[(String, String)]] = {
      val WordTagRe = """^(.+)\|([^|]+)$""".r
      new Iterable[Vector[(String, String)]] {
        override def iterator =
          File(filename).readLines("UTF-8")
            .map(_.trim
              .splitWhitespace
              .map { case WordTagRe(word, tag) => (word, tag) }
              .toVector)
      }
    }
  }

  object AsRawFile {
    def apply(filename: String): Iterable[Vector[String]] = {
      TaggedFile(filename).submap(_._1)
    }
  }

  object RawFile {
    def apply(filename: String): Iterable[Vector[String]] = {
      new Iterable[Vector[String]] {
        override def iterator =
          File(filename).readLines("UTF-8").map(_.trim.splitWhitespace.toVector)
      }
    }
  }

}
