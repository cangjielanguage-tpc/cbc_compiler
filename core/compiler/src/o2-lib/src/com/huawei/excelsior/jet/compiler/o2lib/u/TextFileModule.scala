/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xiFilesModule as xfs}
import com.huawei.excelsior.o2j.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*

object TextFileModule {

  private class TextFile extends xfs.TextFile {

    private[TextFileModule] var raw: xfs.RawFile = _
    private[TextFileModule] var pos: Int = _
    private[TextFileModule] var col: Int = _
    private[TextFileModule] var buf: Array[Byte] = _

    override def closeNew(): Unit = {
      this.raw.closeNew()
    }

    override def close(): Unit = {
      this.raw.close()
    }

    override def readLine(): XString = {
      this.readRes = xfs.allRight
      var i = 0
      val sb = new js.StringBuffer()
      var ch = this.getchar
      while (ch != '\u0000') {
        sb.appendChar(ch)
        i += 1
        this.col += 1
        ch = this.getchar
      }
      this.readLen = i

      if (this.readRes == xfs.endOfLine) {
        if (byteToChar(buf(pos)) == '\r' && pos + 1 < buf.length && byteToChar(buf(pos + 1)) == '\n') {
          pos += 2
        } else {
          pos += 1
        }
      } else if (this.readRes == xfs.endOfInput) {
        if (i == 0) {
          return null
        }
      }
      sb.fromPlatform
    }

    private def getchar: Char = {
      if (pos >= buf.length) {
        this.readRes = xfs.endOfInput
        return '\u0000'
      }

      val ch = byteToChar(buf(pos))

      if (ch < ' ') {
        if (ch == '\u001e' || ch == '\n' || ch == '\u0000' || ch == '\r') {
          this.readRes = xfs.endOfLine
          this.col = 0
          return '\u0000'
        }
      }

      pos += 1

      ch
    }

    override def print(format: String, args: Any*): Unit = {
      val s = js.format(format, args: _*)
      val ds = s.toPlatformBytes
      this.raw.writeBlock(ds, 0, ds.length)
    }

    def preload(): Unit = {
      buf = raw.readFully()
    }
  }


  private class Text extends xfs.Manager[xfs.TextFile] {
    override def open0(name: XString, writeable: Boolean, append: Boolean): xfs.TextFile = {
      val rewriteAll = writeable && !append
      val rawFile = xfs.raw.open0(name, writeable, append)
      if (rawFile == null) {
        this.errmsg = xfs.raw.errmsg
        return null
      }
      assert(!append) // feel free to add support for it
      val f = newTextFile(rawFile, !writeable)
      f.init3(name, writeable, rewriteAll)
      f
    }
  }


  private def byteToChar(b: Byte): Char = O2JSupport.convIntToChar(b.toUByte.toInt)

  def newTextFile(raw: xfs.RawFile, read: Boolean): xfs.TextFile = {
    if (raw == null) {
      return null
    }
    val text = new TextFile()
    text.init3(raw.getName, writeable = !read, rewriteAll = false) // TODO: should be `rewriteAll = true` ??
    text.raw = raw
    if (read) {
      text.preload()
    }
    text
  }

  def setManagers(): Unit = {
    xfs.text = new Text()
  }

}