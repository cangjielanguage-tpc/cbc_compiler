/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.xmlib

import com.huawei.excelsior.common.ProcessUtils.sanitizeCommand
import com.huawei.excelsior.common.XProcess
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.o2lib.u.xiEnvModule
import xscala.io.*
import xscala.sync.XThread

import java.io.IOException
import scala.collection.*
import scala.util.Using

object PortableProgExecModule {
  private def processOutput(input: TextInput, out: TextOutput): Unit = {
    Using.resource(input) { in =>
      for (line <- in.getLines()) {
        out.println(line)
      }
    }
  }

  private def exec(command: Seq[String]): Int = {
    try {
      val p = XProcess.start(sanitizeCommand(command))
      p.stdin.close() // started process doesn't expect an input
      val outReader: Thread = XThread {
        try {
          processOutput(p.stdout, stdout)
        } catch {
          case _: IOException =>
        }
      }
      outReader.start()
      val errReader: Thread = XThread {
        try {
          processOutput(p.stderr, stderr)
        } catch {
          case _: IOException =>
        }
      }
      errReader.start()
      outReader.join()
      errReader.join()
      p.waitFor()
    } catch {
      case e @ (_: IOException | _: InterruptedException) =>
        throw new Error(e)
    }
  }

  def execute(program: String, args: Seq[String]): Int = try {
    exec(Seq(program) ++ args)
  } catch {
    case _: Error => -1
  }

  def executeCommand(command: String, args: Seq[String]): Int = {
    val program = FSModule.HOST.toPlatform(
      FSModule.addPath(
        FSModule.getPath(xiEnvModule.args.programName),
        FSModule.addExt(XString(command), xiEnvModule.config.equation("exeext_host"))))

    execute(program.toString, args)
  }
}
