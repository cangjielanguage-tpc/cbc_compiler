/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u.PDB

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xPDBModule.PDBKind
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.{xFSPlaceholdersModule as xFSPlaceholders, xPDBModule as xPDB}
import com.huawei.excelsior.jet.compiler.o2lib.u.xiFilesModule as xfs
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule as FS

import scala.collection.mutable.ArrayBuffer

object xDirectoryPDBModule {

  /**
   * This module implements xPDB.PDB interface.
   * Placeholders represent files
   */
  private class PDB(_kind: PDBKind, _rootDir: XString, types: xPDB.ContentType.Set)
    extends xPDB.PDB(_kind, _rootDir) {

    override def close(): Unit = {}

    private def accepts(ctype: xPDB.ContentType) = types contains ctype

    private def fullName(name: XString): XString = FS.addPath(rootDir, name)

    override def findPlaceToWriteTo(namePar: XString, type0: xPDB.ContentType): xPDB.Placeholder = {
      if (!accepts(type0)) {
        return null
      }
      val name = xPDB.createPlaceName(namePar, type0)
      xFSPlaceholders.newFilenamePlaceholder(this, fullName(name))
    }

    override def findPlaceToReadFrom(namePar: XString, type0: xPDB.ContentType): xPDB.Placeholder = {
      if (!accepts(type0)) {
        return null
      }
      val name = xPDB.createPlaceName(namePar, type0)

      var place = getPlaceOrNull(name)
      if (place != null && place.exists) {
        return place
      }

      val fname = fullName(name)
      if (!xfs.sys.exists(fname)) {
        return null
      }

      place = xFSPlaceholders.newFilenamePlaceholder(this, fname)
      addPlace(name, place)
    }

    override def iterateAll(type0: xPDB.ContentType, callback: XString => Unit): Unit = ???

    override protected def repackByOrderImpl(type0: xPDB.ContentType, orderedEntriesNames: Option[Iterator[XString]]): Unit = ???

    override def mergeFromWorkers(): Unit = ???

    override def flush(): Unit = ???

    override def findDirectory(name: XString, type0: xPDB.ContentType) = ???

    override def getContentHolder(type0: xPDB.ContentType) = ???
  }

  def openDirectoryPDB(kind: PDBKind, dir: XString, types: xPDB.ContentType.Set): xPDB.PDB = {
    new PDB(kind, dir, types)
  }
}
