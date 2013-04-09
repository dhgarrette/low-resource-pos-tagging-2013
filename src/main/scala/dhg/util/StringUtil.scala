package dhg.util

object StringUtil {

  val WhitespaceRe = """\s*""".r
  val RTrimRe = """(.*\S)\s*""".r
  implicit class EnrichedString(val self: String) extends AnyVal {
    def rtrim = self match {
      case WhitespaceRe() => ""
      case RTrimRe(trimmed) => trimmed
    }

    def splitlines = self.split("\n")
    def splitWhitespace = self.split("\\s+")

    def rsplit(str: String, n: Int) = {
      val parts = self.split(str)
      val (front, back) = parts.splitAt(parts.size - n + 1)
      if (front.nonEmpty)
        front.mkString(str) +: back
      else
        back
    }
  }

}
