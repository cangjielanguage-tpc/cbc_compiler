/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.common.Language.JAVA
import com.huawei.excelsior.common.{QuotedStringTokenizer, XProcess}
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.{Env, Stage}
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.be_386.opAttrsModule
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcNamesModule as pcNames, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.o2.CharClassModule as cc
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.scanners
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.{xLookupModule as xLookup, xPDBModule as xPDB}
import com.huawei.excelsior.jet.compiler.o2lib.u.template.LinkerTemplate
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xcMainModule as xcMain, xcMakeModule as mk, xcResourcesModule as xcResources, xiEnvModule as env, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.{FSModule as FS, MemoryManagementModule as mm, PortableProgExecModule as ProgExec}
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*

/* Make file generator */
object xcFModule { /* Ned 11-Feb-93. */
  def makeProject(p: mk.Project): Unit = {
    def openOut(p: mk.Project, makefile: xPDB.Placeholder): xfs.TextFile = {
      val f = makefile.openAsTextForWrite()

      if (f == null) {
        env.errors.envError(xfs.MSG_FILE_CREATE_ERROR, xfs.text.errmsg)
        p.errs += 1
      }

      val fd = makefile.getFileDescriptor
      env.config.setEquation2("RSPFILENAME", fd.getName)
      f
    }

    def getMakefilePlace(p: mk.Project): xPDB.Placeholder = {
      val outname = env.config.equation("OutputName")
      val fn = FS.getBaseName(if (outname != null) outname else p.fileName)
      assert(fn.nonEmpty)
      xPDB.findPlaceToWriteTo(fn, xPDB.ContentType.RSP)
    }

    if (p.errs != 0) {
      return
    }
    if (!O2Env.env.enabled(BoolOption.BuildXKRN) || Env.languagePack.supports(JAVA)) {
      val makefile = getMakefilePlace(p)
      val ou = openOut(p, makefile)
      val text = if (O2Env.env.enabled(BoolOption.BuildXKRN) && Env.languagePack.supports(JAVA)) {
        LinkerTemplate.writeXKRNTemplate(p)
      } else {
        LinkerTemplate.writeTemplate(p)
      }
      ou.print("%s", text)
      ou.closeNew()
      println("File created")
    } else {
      println("File not created")
    }
  }

  def runLinker(linkstr: XString): Unit = O2Env.stage(Stage.Linking) {
    var res: Int = 0

    val commandWithArgs = new QuotedStringTokenizer(linkstr.toString).filterNot(_ == null).toSeq

    if (!(commandWithArgs.head equalsIgnoreCase "xlink")) {
      throw new IllegalArgumentException(s"linkstr must start with xlink: $linkstr")
    }

    if (!pcO.isCangjie) {
      mm.compactHeap()
      mm.printMem()
    }

    res = ProgExec.executeCommand(commandWithArgs.head, commandWithArgs.tail)
    if (res > 0) {
      env.errors.envError(ErrMsg439, res, linkstr) // INC(p.errs);
    } else if (res < 0) {
      env.errors.envError(ErrMsg447, linkstr) // INC(p.errs);
    }
    if (env.errors.errDetected) {
      env.errDetected = true;
    }
  }
}
