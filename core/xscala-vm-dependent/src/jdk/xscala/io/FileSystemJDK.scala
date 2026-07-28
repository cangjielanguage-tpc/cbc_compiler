/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.file.{StandardCopyOption, Files as JFiles}
import scala.collection.mutable.ArrayBuffer

private[xscala] final class FileSystemJDK extends FileSystemVMDependent {
  def isInvalid(path: String): Boolean = false
  def isDirectory(path: String): Boolean = asJavaFile(path).isDirectory
  def canExecute(path: String): Boolean = asJavaFile(path).canExecute
  def isFile(path: String): Boolean = asJavaFile(path).isFile
  def exists(path: String): Boolean = asJavaFile(path).exists
  def delete(path: String): Boolean = asJavaFile(path).delete

  def setExecutable(path: String, executable: Boolean, ownerOnly: Boolean): Boolean =
    asJavaFile(path).setExecutable(executable, ownerOnly)

  def rename(from: String, to: String): Boolean =
    asJavaFile(from).renameTo(asJavaFile(to))

  def makeDir(path: String): Boolean = asJavaFile(path).mkdir()
  def makeDirs(path: String): Boolean = asJavaFile(path).mkdirs()
  def makeFile(path: String): Boolean = asJavaFile(path).createNewFile()
  def lastModified(path: String): Long = asJavaFile(path).lastModified
  def size(path: String): Long = asJavaFile(path).length
  def abs(path: String): String = asJavaFile(path).getAbsolutePath
  def canonical(path: String): String = asJavaFile(path).getCanonicalPath

  def list(path: String): Array[String] = asJavaFile(path).list()

  def copy(source: String, target: String, replaceTarget: Boolean): Unit = {
    val options = if (replaceTarget) {
      Seq(StandardCopyOption.REPLACE_EXISTING)
    } else {
      Seq()
    }

    JFiles.copy(asJavaFile(source).toPath, asJavaFile(target).toPath, options*)
  }

  private def asJavaFile(path: String) = new java.io.File(path)

  override def newFileInputStream(path: String): InputStream = StreamWrappers.inputStream(new FileInputStream(path))
  override def newFileOutputStream(path: String, append: Boolean): OutputStream = StreamWrappers.outputStream(new FileOutputStream(path, append))

  override def readAllLines(path: String): Seq[String] = {
    val result = ArrayBuffer.empty[String]
    val lines = JFiles.readAllLines(new File(path).toPath).iterator()
    while (lines.hasNext) result += lines.next()
    result.toSeq
  }

  override def write(path: String, bytes: Array[Byte]): Unit = JFiles.write(new File(path).toPath, bytes)
}

object StreamWrappers {
  def inputStream(stream: java.io.InputStream): InputStream = new InputStream() {
    override def read() = stream.read()
    override def read(buffer: Array[Byte]) = stream.read(buffer)
    override def read(buffer: Array[Byte], offset: Int, length: Int) = stream.read(buffer, offset, length)
    override def skip(n: Int) = stream.skip(n).toInt
    override def available() = stream.available()
    override def close(): Unit = stream.close()
  }
  def outputStream(stream: java.io.OutputStream): OutputStream = new OutputStream() {
    override def write(b: Int): Unit = stream.write(b)
    override def write(buffer: Array[Byte]): Unit = stream.write(buffer)
    override def write(buffer: Array[Byte], offset: Int, length: Int): Unit = stream.write(buffer, offset, length)
    override def flush(): Unit = stream.flush()
    override def close(): Unit = stream.close()
  }
}
