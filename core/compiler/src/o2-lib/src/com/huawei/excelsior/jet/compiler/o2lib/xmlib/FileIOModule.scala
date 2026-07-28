/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.xmlib

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.o2lib.u.JStringsModule
import xscala.io.{DataOutput, Path}

import java.io.IOException

object FileIOModule {
  class FileOutputStream (name: XString, out: DataOutput) {
    private var written: Int = 0

    def fprintf(fmt: String, args: Any*): Unit = {
      val str = JStringsModule.format(fmt, args: _*)
      val data = new Array[Byte](str.length)
      str.getChars(data, 0)
      writeBlock(data, 0, data.length)
    }

    def close(): Unit = try
      out.close() catch {
      case e: IOException => setErrorMessage(e)
    }

    def length = written

    def writeBlock(x: Array[Byte], pos: Int, len: Int): Unit = try {
        out.putBytes(x, pos, len)
        written += len
      } catch {
        case e: IOException => setErrorMessage(e)
      }
  }

  private var errorMessage: XString = null

  def getErrorMessage = errorMessage

  def setErrorMessage(e: IOException): Unit = {
    errorMessage = JStringsModule.newJString(e.toString)
  }

  def newFileOutputStream(fileName: XString): FileOutputStream = {
    val out = try {
      DataOutput.from(Path(fileName.toString))
    } catch {
      case e: IOException =>
        setErrorMessage(e)
        return null
    }
    new FileOutputStream(fileName, out)
  }
}
