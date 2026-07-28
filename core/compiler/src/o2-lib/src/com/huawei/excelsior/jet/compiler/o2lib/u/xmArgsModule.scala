/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.common.JetDirs
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule

import scala.collection.mutable.ArrayBuffer


object xmArgsModule {
  private var args: Array[String] = _

  def setArgs(args: Array[String]): Unit = {
    xmArgsModule.args = args
  }

  private class Args extends xiEnvModule.Args {
    private var argList: ArrayBuffer[String] = _
    private var parsed = false

    override def parse(): Unit = {
      if (parsed) return
      argList = ArrayBuffer.from(args)
      parsed = true
    }

    /** Returns path to jc. */
    override lazy val programName = {
      val name = FSModule.addExt2(XString("jc"), FSModule.HOST.exeLikeExtension)
      FSModule.addPath(FSModule.HOST.fromPlatform(XString(JetDirs.bin.toString)), name)
    }

    override def deleteArg(i: Int): Unit = argList.remove(i)

    override def getArg(i: Int) = XString(argList(i))

    override def number(): Int = argList.size
  }

  def setManagers(): Unit = {
    val args = new xmArgsModule.Args
    args.parse()
    xiEnvModule.setArgs(args)
  }
}
