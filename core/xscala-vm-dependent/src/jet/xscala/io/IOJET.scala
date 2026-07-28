/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import xscala.internal.ForeignRef0

import scala.annotation.static

private[xscala] final class IOJET extends IOVMDependent {
  def printStackTrace(ex: Throwable, output: TextOutput): Unit =
    ex.printStackTrace(new PrintStream(output).asInstanceOf[java.io.PrintStream])

  def createRandomAccessFile(path: String, readOnly: Boolean): RandomAccess = {
    RandomAccessJET(path, readOnly)
  }
}

private final class RandomAccessJET(path: String, readOnly: Boolean) extends RandomAccess {
  import xscala.io.RandomAccessJET.*

  private val file = createFile(path, readOnly)

  def cursor: Long = getCursor(file)
  def cursor_=(pos: Long): Unit = setCursor(file, pos)

  def size: Long = getSize(file)
  def size_=(length: Long): Unit = setSize(file, length)

  override def getByte(): Int = getByte0(file)
  override def getBytes(data: Array[Byte], offset: Int, size: Int): Int = getBytes0(file, data, offset, size)

  override def putByte(b: Int): Unit = putByte0(file, b)
  override def putBytes(data: Array[Byte], offset: Int, size: Int): Unit = putBytes0(file, data, offset, size)

  override def close(): Unit = closeFile(file)
}

private object RandomAccessJET {

  opaque type FileHandle = ForeignRef0

  // TODO: consider implementing FileInputStream/FileOutputStream using RandomAccess and unify replacements with FileSystemJET 

  @native @static private def createFile(path: String, readOnly: Boolean): FileHandle
  @native @static private def closeFile(file: FileHandle): Unit

  @native @static private def getCursor(file: FileHandle): Long
  @native @static private def setCursor(file: FileHandle, pos: Long): Unit

  @native @static private def getSize(file: FileHandle): Long
  @native @static private def setSize(file: FileHandle, length: Long): Unit

  @native @static private def getByte0(file: FileHandle): Int
  @native @static private def getBytes0(file: FileHandle, data: Array[Byte], offset: Int, size: Int): Int

  @native @static private def putByte0(file: FileHandle, b: Int): Unit
  @native @static private def putBytes0(file: FileHandle, data: Array[Byte], offset: Int, size: Int): Unit
}
