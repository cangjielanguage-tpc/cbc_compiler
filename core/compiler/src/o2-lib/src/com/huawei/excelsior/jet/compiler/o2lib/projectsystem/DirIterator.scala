/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.projectsystem

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcNamesModule as pcNames
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.{FSModule as FS, PortableRegCompModule as RegComp}

class DirIterator extends xfs.DirIterator {

  private[projectsystem] var p: AbstractProject[_] = _
  private[projectsystem] var dir: xfs.FileDescriptor = _
  private[projectsystem] var name: XString = _
  private[projectsystem] var pat: RegComp.Expr = _
  private[projectsystem] var recurse: Boolean = _
  private[projectsystem] var mode: Int = _ // 0/1/2 - package / fus *.sym / fus *.class

  override def entry(name: XString, dir: Boolean): Boolean = {
    var i: DirIterator = new DirIterator()

    if (name.equals(js.jstrDot) || name.equals(js.jstrTwoDots) || !this.recurse && name.indexOf('/') != -1) {
      return false
    }
    i.name = FS.addPath(this.name, name)
    if (dir) {
      if (this.recurse) {
        i.p = this.p
        i.pat = this.pat
        i.recurse = this.recurse
        i.mode = this.mode
        i.dir = this.dir.getDir(name)
        if (i.dir.iterateDir(i)) {
        }
      }
    } else if (RegComp.Match(this.pat, name, 0)) {
      if (this.mode == 0) {
        this.p.appendFile(i.name)
      } else {
        var class0 = FS.cutExt(i.name)
        if (this.mode == 1) {
          class0 = pcNames.demangleJavaName(class0)
        }
        this.p.findClassAndAppend(class0, doError = true)
      }
    }
    false
  }

}
