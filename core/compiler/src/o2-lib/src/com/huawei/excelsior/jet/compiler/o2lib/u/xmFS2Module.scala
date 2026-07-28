/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.XString
import xscala.io.{Files, Path}

import java.io.IOException
import scala.collection.mutable.ArrayBuffer

object xmFS2Module {
  private class FileSys extends xmFSModule.FileSys {
    override def remove(name: XString): Boolean = Files.delete(Path(name.toString))

    override def rename(name: XString, newname: XString): Boolean = {
      val oldFile = Path(name.toString)
      val newFile = Path(newname.toString)
      Files.delete(newFile)
      Files.rename(source = oldFile, target = newFile)
    }

    override def getCanonicalPath(name: XString): XString = {
      val f = Path(name.toString)
      val absPath = try {
        f.canonicalPath
      } catch {
        case _: IOException => f.absolutePath
      }
      XString(absPath.toString)
    }

    override def exists(fnm: XString): Boolean = Path(fnm.toString).exists

    override def modifyTime(fnm: XString): Int = (Files.getLastModifiedTime(Path(fnm.toString)) / 1000).toInt

    override def makeExecutable(name: XString): Boolean = Files.setExecutable(Path(name.toString), true, false)

    override def listFiles(path: XString): ArrayBuffer[xiFilesModule.DirEntry] = {
      val list = new ArrayBuffer[xiFilesModule.DirEntry]
      val files = Path(path.toString).listFiles
      for (f <- files) {
        list.append(new xiFilesModule.DirEntry(XString(f.name), f.isDirectory))
      }
      list
    }

    override def iterateDir(name: XString, i: xiFilesModule.DirIterator): Boolean = {
      val files = Path(name.toString).listFiles
      for (f <- files) {
        if (i.entry(XString(f.name), f.isDirectory)) return true
      }
      true
    }

    override def createDir(name: XString): Boolean = Files.makeDir(Path(name.toString), withParents = false)
  }

  def setManagers(): Unit = {
    val sys = new xmFS2Module.FileSys
    sys.init()
    xiFilesModule.setFileSys(sys)
  }
}
