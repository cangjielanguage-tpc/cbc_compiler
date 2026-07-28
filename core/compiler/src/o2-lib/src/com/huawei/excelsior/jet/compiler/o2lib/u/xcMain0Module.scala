/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.common.Language
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.TypeMetaInfoGenerator
import com.huawei.excelsior.jet.compiler.o2lib.fe.{ExtraPassModule, NumerateModule, pc, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.u.xiEnvModule as env
import com.huawei.excelsior.jet.compiler.options.Option.SmartKind.*
import xscala.util.StringOps.*

object xcMain0Module {

  def declareOptions(): Unit = {
    val equs: String = "JAVABC=class;SYM=sym;PRJEXT=prj;OBJEXT=obj;EFSEXT=efs;ERRORLEVEL;" //TODO: kill extEquations

    env.equationList(equs)
    env.config.newEquation("PROJECT", Unchecked)
    env.config.newEquation("LOOKUP", Unchecked)
    env.config.newEquation("COMPILERHEAP")
    env.config.newEquation("vcode")
    env.config.newEquation("jre_version")
    env.config.newEquation("errfmt", Unchecked)
    env.config.setEquation("errfmt", "\"\\r* [ %s\",file;\" %d\",line;\".%02d\",column;\" %.1s\",mode;\"%03d ]                                                 \\n\",errno;\"%S\",inlinecontext;\"*  %s\\n\",errmsg")

    env.config.newEquation("link", Unchecked)
    env.config.setEquation("link", "xlink -NoMessages")
    env.config.newEquation("decor", Unchecked)
    env.config.setEquation("decor", if (languagePack.supports(Language.CANGJIE)) "s" else "rhtp")
    env.config.newEquation("environments")
    env.config.setEquation("environments", "")

    // target_platform_cpu / target_platform_os / languagePack equations still required for jc.tem
    // TODO: remove them

    env.config.newEquation("target_platform_cpu")
    env.config.setEquation("target_platform_cpu", targetArch.toString.asciiToLowerCase)

    env.config.newEquation("target_platform_os")
    env.config.setEquation("target_platform_os", targetOS.toString.asciiToLowerCase)

    env.config.newEquation("languagePack")
    env.config.setEquation("languagePack", languagePack.toString.asciiToLowerCase)
  }

  def compilationExit(): Unit = {
    if (isWorkMode) {
      TypeMetaInfoGenerator.Utils.printStatistics()
    }

    //  env.info.Reset;
    pcO.exi()
  }
}
