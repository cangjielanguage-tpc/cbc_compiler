/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import xscala.internal.ForeignRef0
import xscala.io.FileInputStream.*
import xscala.io.FileOutputStream.*
import xscala.io.FileSystemJET.*

import scala.annotation.static
import scala.collection.mutable.ArrayBuffer

private[xscala] final class FileSystemJET extends FileSystemVMDependent {
  def isInvalid(path: String): Boolean = isInvalid0(path)
  def isDirectory(path: String): Boolean = isDirectory0(path)
  def canExecute(path: String): Boolean = canExecute0(path)
  def isFile(path: String): Boolean = isFile0(path)
  def exists(path: String): Boolean = exists0(path)
  def delete(path: String): Boolean = delete0(path)
  def setExecutable(path: String, executable: Boolean, ownerOnly: Boolean): Boolean = setExecutable0(path, executable, ownerOnly)
  def rename(from: String, to: String): Boolean = rename0(from, to)
  def makeDir(path: String): Boolean = makeDir0(path)
  def makeDirs(path: String): Boolean = makeDirs0(path)
  def makeFile(path: String): Boolean = makeFile0(path)
  def lastModified(path: String): Long = lastModified0(path)
  def size(path: String): Long = size0(path)
  def canonical(path: String): String = canonical0(path)
  def list(path: String): Array[String] = list0(path)
  def copy(source: String, target: String, replaceTarget: Boolean): Unit = copy0(source, target, replaceTarget)

  override def newFileInputStream(path: String): InputStream = new FileInputStream(path)
  override def newFileOutputStream(path: String, append: Boolean): OutputStream = new FileOutputStream(path, append)

  override def readAllLines(path: String): Seq[String] = {
    // TODO: use buffered stream
    val stream = newFileInputStream(path)
    // TODO: use Unicode readers

    // FIXME: Java dependency inside!
    val result = ArrayBuffer.empty[String]
    val builder = new StringBuilder()

    // TODO: encapsulate as `Reader`
    var changed = true
    while (changed) {
      val c = stream.read()
      if (c < 0) {
        changed = false
      } else {
        if (c.asInstanceOf[Byte] >= 0) {
          if (c == '\n' || c == '\r') {
            result.append(builder.toString())
            builder.clear()
          } else {
            builder.append((c & 0xFF).toChar)
          }
        } else {
          throw new UnsupportedOperationException("Only ASCII characters are supported")
        }
      }
    }

    if (builder.nonEmpty) {
      result.append(builder.toString())
    }
    result.toSeq
  }

  override def write(path: String, bytes: Array[Byte]): Unit = {
    newFileOutputStream(path, false).write(bytes)
  }
}

private object FileSystemJET {
  @native @static private def isInvalid0(path: String): Boolean
  @native @static private def isDirectory0(path: String): Boolean
  @native @static private def canExecute0(path: String): Boolean
  @native @static private def isFile0(path: String): Boolean
  @native @static private def exists0(path: String): Boolean
  @native @static private def delete0(path: String): Boolean
  @native @static private def setExecutable0(path: String, executable: Boolean, ownerOnly: Boolean): Boolean
  @native @static private def rename0(from: String, to: String): Boolean
  @native @static private def makeDir0(path: String): Boolean
  @native @static private def makeDirs0(path: String): Boolean
  @native @static private def makeFile0(path: String): Boolean
  @native @static private def lastModified0(path: String): Long
  @native @static private def size0(path: String): Long
  @native @static private def canonical0(path: String): String
  @native @static private def list0(path: String): Array[String]
  @native @static private def copy0(source: String, target: String, replaceTarget: Boolean): Unit
}

private opaque type Handle = ForeignRef0

private final class FileInputStream(path: String) extends InputStream {
  private val handle: Handle = StreamFactory.createInputStream(path)

  override def read(): Int = read0(handle)
  override def read(buffer: Array[Byte]): Int = read(buffer, 0, buffer.length)
  override def read(buffer: Array[Byte], offset: Int, length: Int): Int = read0(handle, buffer, offset, length)
  override def available(): Int = available0(handle)
  override def skip(n: Int): Int = skip0(handle, n)
  override def close(): Unit = close0(handle)
}

private object FileInputStream {
  @native @static private def read0(handle: Handle): Int
  @native @static private def read0(handle: Handle, buffer: Array[Byte], offset: Int, length: Int): Int
  @native @static private def available0(handle: Handle): Int
  @native @static private def skip0(handle: Handle, n: Int): Int
  @native @static private def close0(handle: Handle): Unit
}

private final class FileOutputStream(path: String, append: Boolean) extends OutputStream {
  private val handle: Handle = StreamFactory.createOutputStream(path, append)

  override def write(b: Int): Unit = write0(handle, b)
  override def write(buffer: Array[Byte]): Unit = write(buffer, 0, buffer.length)
  override def write(buffer: Array[Byte], offset: Int, length: Int): Unit = write0(handle, buffer, offset, length)
  override def close(): Unit = close0(handle)
}

private object FileOutputStream {
  @native @static private def write0(handle: Handle, b: Int): Unit
  @native @static private def write0(handle: Handle, buffer: Array[Byte], offset: Int, length: Int): Unit
  @native @static private def close0(handle: Handle): Unit
}

class StreamFactory
private object StreamFactory {
  @native @static def createInputStream(path: String): Handle
  @native @static def createOutputStream(path: String, append: Boolean): Handle
}
