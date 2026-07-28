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
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object xArchiveModule {

  abstract class Archive {

    /*RO*/ var name: XString = _
    /*RO*/ var db: xPDB.PDB = _
    /*RO*/ var readonly: Boolean = _
    var mergedOrCleanedUp: Boolean = _

    /**
     * openFileForRead does not check anything,
     * be sure to check both archive & file existance before calling openFileForRead
     * Returned RawFile is _currently_ is lazy RamFile
     */
    def openFileForRead(entry: XString): xfs.RawFile

    /**
     * openFileForWrite doesn't check anything
     * be sure to check that Archive doesn't contain entry before calling openFileForWrite
     */
    def openFileForWrite(entry: XString): xfs.RawFile

    /**
     * Replaces this archive with the one created during merge. 
    */
    def completeMergeOrCleanup(): Unit = {
      if (this.mergedOrCleanedUp) {
        val done = xfs.sys.rename(this.getMergeOrCleanupArchiveName, this.name)
        assert(done)
      }
    }

    /**
     * Returns temporal name used as a target archive during merge or cleanup. 
    */
    def getMergeOrCleanupArchiveName: XString = this.name.concat(js.newJString(".tmp"))

    /**
     * If we do not compile anything the archive may still have outdated entries
     * inside. In this case we need to create a new archive that has no outdated entries
     * that will be renamed to this one on completeMergeOrCleanup operation.
     */
    def cleanupAndClose(): Unit

    def copyEntriesByOrder(entries: Iterator[XString], target: Archive): Unit

    def mergeAndDelete(second: Archive): Unit

    /**
     * Merges two archives into third one with the name given by getMergeOrCleanuoArchiveName()
     * via copying entries from the original archive then from target one, 
     * skipping entries from the original one.
     * Finally, this archive is replaced by temporally one via completeMergeOrCleanup operation.
    
     * Merge is divided by two operations to make final step (completeMergeOrCleanup) after
     * all merge operations of individual PDB archives to reduce risk of leaving PDB
     * in inconsistent state. 
     */
    def mergeAndCloseBoth(target: Archive): Unit

    /**
     * Returns iterator.
     * Note: Usage of the method is unsafe! Do it with care
     *
     * TODO: The best solution is to introduce some locking API to ensure that
     * Archive wont be closed or reopened for write during Iterator usage
     */
    def getIterator: Iterator[ArchiveEntry]

    /**
     * If archive is not opened or opened in write mode, tries to open it in read mode
     */
    def openForRead(): Boolean

    /**
     * Creates new archive, existing archive with the same name will be overwritten
     * If archive is already opened behavior is undefined
     */
    def new0(): Unit

    /**
     * Constructor
     */
    def init(name: XString, db: xPDB.PDB): Unit = {
      this.name = name
      this.readonly = db == null
      this.db = db
      this.mergedOrCleanedUp = false
    }

    def flush(): Unit

    /* ------------------------------- Archive ------------------------------- */
    def close(): Unit

  }


  abstract class ArchiveEntry {

    def getTime: Int

    def getName: XString

  }
}
