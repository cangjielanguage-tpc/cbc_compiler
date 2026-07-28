/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u.PDB

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xPDBModule.{PDB, PDBKind}
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.{xArchivePDBModule as xArchivePDB, xPDBModule as xPDB}

import scala.collection.mutable.ArrayBuffer

object xPDBManagerModule {
  private class Manager extends xPDB.Manager {
    private val allPDBs = ArrayBuffer.empty[PDB]

    private var _profilePDB: PDB = _
    private var _libraryPDB: PDB = _
    private var _mainPDBDir: XString = _

    override def mainPDB = allPDBs.head
    override def profilePDB = _profilePDB
    override def libraryPDB = _libraryPDB

    override def isMainPDBOpened = allPDBs.nonEmpty
    override def mainPDBDir = _mainPDBDir

    override def findPlaceToWriteTo(name: XString, type0: xPDB.ContentType): xPDB.Placeholder = mainPDB.findPlaceToWriteTo(name, type0)

    override def findPlaceToReadFrom(name: XString, type0: xPDB.ContentType, skipProfile: Boolean): xPDB.Placeholder = {
      val toSearch = if (skipProfile) allPDBs.filterNot(_.isProfile) else allPDBs
      for (pdb <- toSearch) {
        val place = pdb.findPlaceToReadFrom(name, type0)
        if (place != null) {
          return place
        }
      }
      null
    }

    override def findDirectory(name: XString, type0: xPDB.ContentType): xPDB.Placeholder = {
      allPDBs.iterator.map(_.findDirectory(name, type0)).find(_ != null).orNull
    }

    override def closeAll(): Unit = {
      allPDBs foreach { _.close() }
      allPDBs.clear()
      _profilePDB = null
      _libraryPDB = null
    }

    override def openMainPDB(name: XString): Boolean = {
      assert(allPDBs.isEmpty)
      val main = xArchivePDB.ctor.open(PDBKind.Main, name)
      if (main != null) {
        allPDBs += main
        _mainPDBDir = mainPDB.rootDir
      }
      main != null
    }

    override def createMainPDB(name: XString, reset: Boolean): Unit = {
      assert(reset == isMainPDBOpened)
      if (isMainPDBOpened) {
        mainPDB.close()
        _mainPDBDir = null
      } else {
        allPDBs += null
      }
      allPDBs(0) = xArchivePDB.ctor.createMain(name)
      _mainPDBDir = mainPDB.rootDir
    }

    override def makePDBName(outputname: XString, projectfile: XString): XString =
      xArchivePDB.ctor.makePDBName(outputname, projectfile)

    override def registerAuxPDB(pdb: xPDB.PDB): Unit = {
      assert(pdb != null && !pdb.isMain)
      assert(isMainPDBOpened)
      allPDBs += pdb

      pdb.kind match {
        case PDBKind.Profile =>
          assert(_profilePDB == null)
          _profilePDB = pdb
        case PDBKind.Library =>
          // assert(_libraryPDB == null)
          // In case of multiple library pdbs we see only the latest one here
          // TODO: fix it somehow
          _libraryPDB = pdb
        case _ =>
      }
    }
  }

  /* -------------------------------------------------------------- */
  def initManager(): Unit = {
    xArchivePDB.initCtor()
    xPDB.manager = new Manager()
  }
}
