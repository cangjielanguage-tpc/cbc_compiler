/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u.PDB

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xPDBModule as xPDB
import com.huawei.excelsior.jet.compiler.o2lib.u.{xiEnvModule as env, xiFilesModule as xfs}

object xFSPlaceholdersModule {
  /**
   * This module contains implementations of xPDB.EntryDescriptor interface
   * which represent real files.
   */

  private class FilenamePlaceholder(val pdb: xPDB.PDB, filename: XString) extends xPDB.Placeholder {
    override def iterateDir(prefix: XString, i: xfs.DirIterator): Boolean = ???

    override def delete(): Boolean = xfs.sys.remove(filename)

    override def exists: Boolean = xfs.sys.exists(filename)

    override def getFileDescriptor: xfs.FileDescriptor = xfs.sys.createFileDescriptor(filename)

    override def fullName: XString = filename

    override def getModifyTime: Int = xfs.sys.modifyTime(filename)

    override def openAsRawForRead(): xfs.RawFile = {
      val file = xfs.raw.openToRead(filename)
      if (file == null) {
        env.errors.fault(xfs.MSG_FILE_OPEN_ERROR, xfs.raw.errmsg)
      }
      file
    }

    override def openAsRawForWrite(): xfs.RawFile = {
      val file = xfs.raw.openToWrite(filename)
      if (file == null) {
        env.errors.fault(xfs.MSG_FILE_CREATE_ERROR, xfs.raw.errmsg)
      }
      file
    }
  }


  private class FDPlaceholder(val pdb: xPDB.PDB, fd: xfs.FileDescriptor) extends xPDB.Placeholder {
    override def delete(): Boolean = ???
    override def iterateDir(prefix: XString, i: xfs.DirIterator): Boolean = ???

    override def getFileDescriptor: xfs.FileDescriptor = fd

    override def getModifyTime: Int = fd.modifyTime()

    override def exists: Boolean = fd.exists

    override def fullName: XString = fd.getName

    override def openAsRawForWrite(): xfs.RawFile = {
      throw new AssertionError
    }

    override def openAsRawForRead(): xfs.RawFile = fd.openRawFile()
  }


  def newFilenamePlaceholder(pdb: xPDB.PDB, filename: XString): xPDB.Placeholder = {
    new FilenamePlaceholder(pdb, filename)
  }

  def newFDPlaceholder(pdb: xPDB.PDB, fd: xfs.FileDescriptor): xPDB.Placeholder = {
    new FDPlaceholder(pdb, fd)
  }

  /* --------------------------------------------------------------------- */
  def getDefaultPlaceToWriteTo(pdb: xPDB.PDB, name: XString): xPDB.Placeholder = newFilenamePlaceholder(pdb, xfs.sys.useFirst(name))
}
