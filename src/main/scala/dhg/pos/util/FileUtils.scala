package dhg.pos.util

import scala.util.Random
import java.io._
import scala.collection.mutable.Buffer
import dhg.util.FileUtil._

object FileUtils {

  def insertTempSubdir(file: String, mkDir: Boolean = false) = {
    val (origPath, hadoopGraphFilename) = getPathAndFile(file)
    val path =
      if (new File(origPath).getName != "temp") {
        val newPath = pathjoin(origPath, "temp")
        val pathObj = new File(newPath)
        if (mkDir && !pathObj.exists)
          pathObj.mkdirs()
        newPath
      }
      else
        origPath
    pathjoin(path, hadoopGraphFilename)
  }

  def getPathAndFile(pathAndFile: String) = {
    val absFile = new File(pathAndFile).getAbsoluteFile
    (absFile.getParent, absFile.getName)
  }

  implicit def string2file(s: String) = new File(new File(s).getAbsolutePath)

}
