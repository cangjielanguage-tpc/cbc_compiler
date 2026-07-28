/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.projectsystem

import com.huawei.excelsior.jet.compiler.driver.ProjectLogic
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.DecorParser.parseDecor
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.ErrorMessages.{configResToMsg, msg_error_in_command_line, msg_syntax_error}
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.MemorySizeParser.parseMemorySize
import com.huawei.excelsior.jet.compiler.o2lib.u.{xcModesModule as xcModes, xiEnvModule as env, xiFilesModule as xfs}

object CommandLineParser {

  private val LOOKUP: String = "LOOKUP"
  val HEAPLIMIT: String = "HEAPLIMIT"
  val COMPILERHEAP: String = "COMPILERHEAP"

  def parseCommandLine(): Unit = {
    xfs.sys.saveRed()
    env.args.parse()
    var i = 0

    while (i < env.args.number()) {
      val s = env.args.getArg(i)
      if (env.config.isValidTag(s)) {
        var name = env.config.parse(s)
        if (env.config.res > env.ok) {
          configResToMsg(env.config.res)
        } else if (env.config.res == env.isEquation && name.equals2(LOOKUP)) {
          name = env.config.equation(LOOKUP)
          xfs.sys.parseRed(name)
        } else if (env.config.res == env.isEquation && name.equals2(COMPILERHEAP)) {
          val compilerheap = parseMemorySize(env.config.equation(COMPILERHEAP))
          if (compilerheap >= 0) {
            // we have no convenient way to control compiler heap yet
          } else {
            val msg = env.errors.getMsg(msg_syntax_error)
            env.errors.fault(msg_error_in_command_line, s, msg)
          }
        } else if (env.config.res == env.isEquation && name.equals2(HEAPLIMIT)) {
          val heaplimit = parseMemorySize(env.config.equation(HEAPLIMIT))
          if (heaplimit < 0) {
            val msg = env.errors.getMsg(msg_syntax_error)
            env.errors.fault(msg_error_in_command_line, s, msg)
          }
        }
        env.args.deleteArg(i)
      } else {
        i += 1
      }
    }
    parseDecor()
  }

}
