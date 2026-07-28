/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule
import xscala.io.{Path, RandomAccess}

import java.io.{EOFException, IOException}

object RawFileModule {
  private class RawFile(underlying: RandomAccess) extends xiFilesModule.RawFile {
    private def wrapIO[R](action: R): R = {
      try action catch {
        case e: IOException => throw new Error(e)
      }
    }

    override def close(): Unit = wrapIO { underlying.close() }

    override def closeNew(): Unit = close()

    override def setPos(pos: Long): Unit = wrapIO { underlying.cursor = pos }

    override def getPos: Long = wrapIO { underlying.cursor }

    override def length: Long = try {
      underlying.size
    } catch {
      case _: IOException => 0
    }

    override def writeBlock(x: Array[Byte], pos: Int, len: Int): Unit = wrapIO { underlying.putBytes(x, pos, len) }

    private def readBlockImpl(x: Array[Byte], pos: Int, len: Int): Int = {
      var n = 0
      while (n < len) {
        val count = underlying.getBytes(x, pos + n, len - n)
        if (count < 0) {
          return n
        }
        n += count
      }
      n
    }

    override def readBlock(x: Array[Byte], pos: Int, len: Int): Int = wrapIO {
      val res = readBlockImpl(x, pos, len)
      val n = if (res == 0) -1 else res
      val r = if (res == 0) xiFilesModule.endOfInput else xiFilesModule.allRight
      this.setReadRes(r.toShort)
      this.setReadLen(n)
      n
    }
  }

  private class Raw extends xiFilesModule.Manager[xiFilesModule.RawFile] {
    override def open0(name: XString, writeable: Boolean, append: Boolean): xiFilesModule.RawFile = {
      val rewriteAll = writeable && !append // rewrite existing file or create new one
      try {
        val path = Path(FSModule.HOST.toPlatform(name).toString)

        if (rewriteAll) DirsModule.mkdirs(FSModule.getPath(name))
        val underlying = RandomAccess(path, !writeable)
        if (rewriteAll) underlying.size = 0

        val f = new RawFile(underlying)
        f.init3(name, writeable, rewriteAll)
        f
      } catch {
        case e: IOException =>
          errmsg = XString(e.getMessage)
          null
      }
    }
  }

  def setManagers(): Unit = {
    xiFilesModule.setRawManager(new Raw)
  }
}
