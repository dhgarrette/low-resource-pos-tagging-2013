package dhg.pos.data.prep

import java.io.File
import java.io.BufferedReader
import dhg.pos.util.FileUtils._
import java.io.FileReader
import scala.io.Source
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.Writer
import scala.collection.mutable.Buffer
import dhg.util.FileUtil._

object WordPerLine2SentPerLine {

  def main(args: Array[String]): Unit = {
    var wordCount = 0
    var sentCount = 0
    val Array(wpl, spl) = args
    writeUsing(spl) { w =>
      val b = Buffer[String]()
      for (line <- Source.fromFile(wpl).getLines.map(_.trim))
        line match {
          case "" => {
            if (b.nonEmpty) {
              w.write(b.mkString(" ") + "\n")
              b.clear()
              sentCount += 1
            }
          }
          case _ => {
            b.append(line)
            wordCount += 1
          }
        }
      if (b.nonEmpty) {
        w.write(b.mkString(" ") + "\n")
        sentCount += 1
      }
    }

    println("words = " + wordCount)
    println("sents = " + sentCount)
  }
}
