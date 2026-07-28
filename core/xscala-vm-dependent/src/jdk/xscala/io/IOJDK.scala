/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import java.io.{ByteArrayOutputStream, PrintStream, RandomAccessFile}

private[xscala] final class IOJDK extends IOVMDependent {
  def printStackTrace(ex: Throwable, out: TextOutput): Unit = {
    val baos = new ByteArrayOutputStream
    ex.printStackTrace(new PrintStream(baos, true))
    out.print(baos.toString)
  }

  def createRandomAccessFile(path: String, readOnly: Boolean): RandomAccess = {
    new RandomAccess {
      private val impl = new RandomAccessFile(path, if (readOnly) "r" else "rw")

      def cursor: Long = impl.getFilePointer
      def cursor_=(pos: Long): Unit = impl.seek(pos)

      def size: Long = impl.length()
      def size_=(length: Long): Unit = impl.setLength(length)

      override def getByte(): Int = impl.read()
      override def getBytes(data: Array[Byte], offset: Int, size: Int): Int = impl.read(data, offset, size)

      override def putByte(b: Int): Unit = impl.write(b)
      override def putBytes(data: Array[Byte], offset: Int, size: Int): Unit = impl.write(data, offset, size)

      override def close(): Unit = impl.close()
    }
  }
}
