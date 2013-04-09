package dhg.util

import java.io.BufferedWriter
import java.io.File
import java.io.File.createTempFile
import java.io.File.separator
import java.io.FileOutputStream
import java.io.FileWriter
import java.io.OutputStreamWriter
import java.io.Writer
import java.net.URI
import scala.collection.breakOut
import scala.io.BufferedSource
import scala.io.Source
import dhg.util.Arm._
import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.zip.GZIPInputStream
import java.io.FileInputStream

object FileUtil {

  object File {
    def apply(parent: File, child: String) = new File(parent, child)
    def apply(path: String) = new File(path)
    def apply(path: String*) = new File(pathjoin(path))
    def apply(uri: URI) = new File(uri)
  }

  def file(parent: File, child: String) = new File(parent, child)
  def file(path: String) = new File(path)
  def file(path: String*) = new File(pathjoin(path))
  def file(uri: URI) = new File(uri)

  def pathjoin(part: String, parts: String*): String = {
    pathjoin(part +: parts)
  }

  def pathjoin(parts: Seq[String]): String = {
    val start = if (parts.head.startsWith(separator)) separator else ""
    parts.flatMap(_.split(separator)).filter(_.nonEmpty).mkString(start, separator, "")
  }

  /**
   * Generate a temporary filename but do not actually create the file in the
   * filesystem.
   */
  def mktemp(prefix: String = "temp-", suffix: String = ""): File = {
    val f = createTempFile(prefix, suffix)
    f.delete()
    f
  }

  implicit class EnhancedFile(val self: File) extends AnyVal {

    def path = {
      self.getPath
    }

    def name = {
      self.getName
    }

    def parent = {
      Option(self.getParentFile)
    }

    /**
     * Separate the filename from the parent directory.
     * Return Some(parentDir, filename) if there is a parent directory,
     * and None otherwise.
     */
    def parentFilename = {
      (self.parent, self.name)
    }

    /**
     * Get the path as a sequence of strings.
     */
    def pathSeq: Vector[String] = {
      val (parent, file) = parentFilename
      parent.map(_.pathSeq).getOrElse(Vector()) :+ file
    }

    /**
     * Delete this file, even if it is a non-empty directory.
     */
    def deleteRecursive(): Boolean = {
      if (self.isDirectory)
        self.listFiles.filter(_ != null).foreach(_.deleteRecursive())
      self.delete()
    }

    /**
     * List all files (but not directories), searching recursively through sub-directories.
     */
    def listFilesRecursive: Set[File] = {
      self.listFiles.flatMap { f =>
        if (f.isDirectory)
          f.listFilesRecursive
        else
          Set(f)
      }(breakOut)
    }

    /**
     * Return a path to this relative to the given directory.
     */
    def relativeTo(dir: File): Option[File] = {
      val dirPath = dir.getAbsolutePath
      val selfPath = self.getAbsolutePath
      if (selfPath.startsWith(dirPath)) {
        val a = selfPath.drop(dirPath.length)
        Some(File(if (a.startsWith(separator)) a.drop(1) else a))
      }
      else
        None
    }

    /**
     * Read the contents of this file, making sure to close the file after all
     * lines have been read.
     */
    def readLines: Iterator[String] = {
      Arm.readLines(self)
    }

    /**
     * Read the contents of this file, making sure to close the file after all
     * lines have been read.
     */
    def readLines(encoding: String): Iterator[String] = {
      Arm.readLines(self, encoding)
    }

  }

  case class BufferedReaderIterator(reader: BufferedReader) extends Iterator[String] {
    override def hasNext() = reader.ready
    override def next() = reader.readLine()
  }

  object GzFileIterator {
    def apply(file: File, encoding: String) = {
      new BufferedReaderIterator(
        new BufferedReader(
          new InputStreamReader(
            new GZIPInputStream(
              new FileInputStream(file)), encoding)))
    }
    def apply(file: File) = {
      new BufferedReaderIterator(
        new BufferedReader(
          new InputStreamReader(
            new GZIPInputStream(
              new FileInputStream(file)))))
    }
  }

  def findBinary(name: String, binDir: Option[String] = None, envar: Option[String] = None): String = {
    val checked = collection.mutable.Buffer[String]()

    for (d <- binDir) {
      val path = pathjoin(d, name)
      if (File(path).exists)
        return path
      else
        checked += path
    }

    for (ev <- envar; envpath <- Option(System.getenv(ev))) {
      val path = envpath + "/" + name
      if (File(path).exists)
        return path
      else
        checked += path
    }

    try {
      val found = scala.sys.process.Process(List("which", name)).!!
      return found.trim
    }
    catch {
      case _: Throwable => checked += s"which $name"
    }

    throw new RuntimeException("No binary found.  Checked the following:\n" + checked.map((" ") * 16 + _).mkString("\n"))
  }

  /**
   * Open a file for reading, execute a block of code, and ensure that the
   * file is closed when finished.
   */
  def readUsing[R](file: File)(block: BufferedSource => R): R = {
    using(Source.fromFile(file))(block)
  }

  /**
   * Open a file for writing (creating the containing directory structure if
   * necessary), execute a block of code, and ensure that the file is closed
   * when finished.
   */
  def writeUsing[R](file: File)(block: BufferedWriter => R): R = {
    file.parent.foreach(_.mkdirs())
    using(new BufferedWriter(new FileWriter(file)))(block)
  }

  /**
   * Open a file for writing (creating the containing directory structure if
   * necessary), execute a block of code, and ensure that the file is closed
   * when finished.
   */
  def writeUsing[R](file: File, encoding: String)(block: BufferedWriter => R): R = {
    file.parent.foreach(_.mkdirs())
    using(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), encoding)))(block)
  }

  /**
   * Add a method `writeLine` to Writer classes
   */
  implicit class WriterWithWriteLine(self: Writer) {
    def writeLine(line: String) { self.write(line + "\n") }
  }

}
