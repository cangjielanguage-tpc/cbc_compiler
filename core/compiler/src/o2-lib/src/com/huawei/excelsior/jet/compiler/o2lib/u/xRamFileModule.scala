/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.u.xiFilesModule as xfs

object xRamFileModule {

  class RamFile extends xfs.RawFile {

    private[xRamFileModule] var pos: Int = _
    private[xRamFileModule] var len: Int = _
    private[xRamFileModule] var buf: Array[Byte] = _

    override def close(): Unit = {
      this.rewriteAll = false // TODO: set CLOSED status instead
    }

    override def closeNew(): Unit = {
      this.rewriteAll = false // TODO: set CLOSED status instead
      this.pos = 0
    }

    override def setPos(pos: Long): Unit = {
      this.pos = xfs.posToInt(pos)
    }

    override def getPos: Long = this.pos

    override def length: Long = this.len

    override def writeBlock(x: Array[Byte], pos: Int, len: Int): Unit = {
      // nothing to write? 
      if (len == 0) {
        return
      }

      assert(pos + len <= x.length)

      if (buf == null) {
        buf = new Array[Byte](len)
        this.len = 0
        this.pos = 0
      } else if (this.len + len > buf.length) {
        setBufSize((buf.length * 2) max (this.len + len))
      }

      this.readRes = xfs.allRight
      this.readLen = len
      Array.copy(x, pos, buf, this.pos, len)
      this.pos += len
      this.len += len
    }

    override def readBlock(buf: Array[Byte], pos: Int, len: Int): Int = {
      assert(pos + len <= buf.length)

      val rest = this.len - this.pos

      val r = if (rest <= 0) xfs.endOfInput else xfs.allRight
      this.readRes = r

      val n = len min rest
      this.readLen = n

      if (n > 0) {
        Array.copy(this.buf, this.pos, buf, pos, n)
      }
      this.pos += n

      if (rest <= 0) -1 else n
    }

    def getBytes: Array[Byte] = {
      assert(this.buf != null)
      this.setBufSize(this.len)
      this.buf
    }

    def setBufSize(size: Int): Unit = {
      assert(size >= this.len)
      if (size == this.buf.length) {
        return
      }
      val b = new Array[Byte](size)
      Array.copy(this.buf, 0, b, 0, this.len)
      this.buf = b
    }

    /** Open Ram file for reading. */
    def open(): Unit = {
      this.pos = 0
    }
  }

  /** Creates new Ram file. */
  def newRamFile(name: XString, buf: Array[Byte] = null, len: Int = 0): RamFile = {
    val f = new RamFile()
    f.init3(name, writeable = true, rewriteAll = true)
    f.pos = 0
    f.len = len
    f.buf = buf
    f
  }
}
