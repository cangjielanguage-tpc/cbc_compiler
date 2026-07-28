/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.projectsystem.cangjie

import com.huawei.excelsior.common.CodeHelpers
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.cangjie.Errors.*
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.scanners.ScanPro
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.{AbstractProject, CPEntryModes}
import com.huawei.excelsior.jet.compiler.o2lib.u.{xiEnvModule as env, xiFilesModule as xfs}

import scala.collection.mutable.ArrayBuffer

class CangjieProject extends AbstractProject[Unit] {

  val files: ArrayBuffer[XString] = ArrayBuffer()

  override def appendFile(fileName:  XString): Unit = {
    if (!fileName.toString.endsWith(".bc") && !fileName.toString.endsWith(".chir")) {
      error(WRONG_EXTENSION_ERROR, fileName)
    }

    val module = xfs.sys.createFileDescriptor(fileName)
    if (!module.exists) {
      error(MODULE_NOT_FOUND_ERROR, fileName)
    }

    files.addOne(fileName)
  }

  private def notSupported(msg: String): Unit = {
    Errors.error(NOT_SUPPORTED_DIRECTIVE_FOR_CANGJIE_PROJECT, msg)
  }

  override def appendClassloaderEntry(cpentry:  XString, cpentrymode:  CPEntryModes.CPEntryMode, bidInInternalForm:  Boolean, userDef:  Boolean): Unit =
    notSupported("!classloaderentry")
  override def appendClasspathEntry(cpentry:  XString): Unit = notSupported("!classpathentry")
  override def findClassAndAppend(class0:  XString, doError:  Boolean): Unit = CodeHelpers.shouldNotCallThis("CangjieProject.findClassAndAppendToProject")
  override def openPDB(): Unit = {}
  override def setErr(): Unit = env.exit(3)

}

object CangjieProject {

  def openProject(name: XString): CangjieProject = {
    if (!name.toString.endsWith(".prj")) {
      error(NOT_A_PROJECT_ERROR, name)
    }
    val fd = xfs.sys.lookup(name)
    val file = fd.openTextFile()
    val p = new CangjieProject
    if (!new ScanPro(p).readText(file)) {
      p
    } else {
      env.exit(3)
    }
  }

  def createProjectFromArgs(): CangjieProject = {
    val p = new CangjieProject()
    for (i <- 0 until env.args.number()) {
      p.appendFile(env.args.getArg(i))
    }
    p
  }

}
