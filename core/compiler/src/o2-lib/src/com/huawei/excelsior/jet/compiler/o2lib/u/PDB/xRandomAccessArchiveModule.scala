/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u.PDB

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.{xArchiveModule as xArchive, xPDBModule as xPDB}
import com.huawei.excelsior.jet.compiler.o2lib.u.{xRamFileModule as xRamFile, xiEnvModule as env, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.TimeModule as Time

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object xRandomAccessArchiveModule {

  /**
   *  PDB archive has the following format:
   *
   *   0: MAGIC   (4 bytes)
   *   4: VERSION (4 bytes)
   *   8: Index position (8 bytes)
   *  f1: file1 (n1 bytes)
   *  f2: file2 (n2 bytes)
   *    ...
   *  fN: fileN  (nN bytes)
   *   L:Index (L = Index position)
   *
   *   , where fileI is contents of I-th file in the archive.
   *
   *    Index has the following format:
   *
   *   L+0: MAGIC
   *   L+4: N (number of files)
   *   L+8: ARRAY N OF ArchiveEntry
   *
   *   ArchiveEntry has the following format:
   *
   *   name (zero-terminated UTF8 string)
   *   time of modification in seconds (4 bytes)
   *   position of the contents of the I-th file in the archive -- fI (8 bytes)
   *   length of the contents of the I-th file in the archive -- nI (4 bytes)
   *
   *   NOTE: files more than 2G inside PDB archives are not supported yet.
   */
  class Archive extends xArchive.Archive {
    private[xRandomAccessArchiveModule] var rFile: xfs.RawFile = _ // archieve file used for read
    private[xRandomAccessArchiveModule] var wFile: xfs.RawFile = _ // archieve file used for write
    private[xRandomAccessArchiveModule] var newF: Boolean = _ // whether the file was opened as new originally
    private[xRandomAccessArchiveModule] var entries: mutable.HashMap[XString, ArchiveEntry] = _
    private[xRandomAccessArchiveModule] var indexPos: Long = _ // the position where we should write the index
    private[xRandomAccessArchiveModule] var flushIndex: Boolean = _ // whether it is needed to write index
    private[xRandomAccessArchiveModule] var lastOpened: EntryFile = _

    /** In ram mode we read each entry as a whole on open from the archive and store the content
      * of the entry in RamFile.
      * We switch to ram mode when two different archive entries are opened simultaneously for read.
      * No two entries can be opened for write simultaneously.
      */
    private[xRandomAccessArchiveModule] var ramMode: Boolean = _

    /** `openFileForRead` does not check anything,
      * be sure to check both archive & file existence before calling openFileForRead.
      */
    override def openFileForRead(name: XString): xfs.RawFile = entries.get(name) match {
      case None =>
        shouldNotReachHere(s"openFileForRead failed: $name")

      case Some(entry) =>
        var f: xfs.RawFile = null

        if (this.lastOpened != null) {
          this.switchToRamMode()
        }
        if (!this.ramMode) {
          f = this.getOrOpenArchiveFile(onWrite = false)
          f.setPos(entry.pos)
        }
        newRawFile(f, entry, writable = false)
    }

    /**
     * openFileForWrite doesn't check anything
     * be sure to check that Archive doesn't contain entry before calling openFileForWrite
     */
    override def openFileForWrite(name: XString): xfs.RawFile = {
      var time: Int = 0

      if (this.lastOpened != null) {
        this.switchToRamMode()
      }
      val f = this.getOrOpenArchiveFile(onWrite = true)
      if (xPDB.stableBuild) {
        // disable timestamps for profile build to stabilize JET distros.
        time = 0
      } else {
        time = Time.getTime
      }
      val entry = newArchiveEntry(this, name, time, this.indexPos, 0)
      f.setPos(entry.pos)
      assert(!this.entries.contains(name)) // double write is not supported
      this.entries.put(name, entry)
      newRawFile(f, entry, writable = true)
    }

    def switchToRamMode(): Unit = {
      assert(this.lastOpened != null)
      assert(!this.lastOpened.rewriteAll) // can not read file if last opened on write // TODO: ...if lastOpened not yet closed
      val pos = this.lastOpened.getPos
      this.lastOpened.orig = this.lastOpened.entry.readEntryToRam(this.lastOpened.orig)
      this.lastOpened.startPos = 0
      this.lastOpened.setPos(pos)
      this.lastOpened = null
      this.ramMode = true
    }

    def getOrOpenArchiveFile(onWrite: Boolean): xfs.RawFile = {
      if (onWrite) {
        if (this.wFile != null) {
          return this.wFile
        }
      } else if (this.rFile != null) {
        return this.rFile
      }
      val file = if (onWrite) xfs.raw.openToAppend(this.name) else xfs.raw.openToRead(this.name)
      if (file == null) {
        env.errors.fault(xfs.MSG_FILE_OPEN_ERROR, xfs.raw.errmsg)
      }
      if (onWrite) {
        this.wFile = file
      } else {
        this.rFile = file
      }
      file
    }

    override def flush(): Unit = {
      this.flushIndexIfNeeded()
      this.flushInternalFiles()
    }

    override def close(): Unit = {
      this.flushIndexIfNeeded()
      this.closeInternalFiles()
    }

    def flushIndexIfNeeded(): Unit = {
      if (flushIndex) {
        assert(wFile != null)
        val f = xfs.sym.wrapForAppend(wFile)
        f.setPos(8)
        f.writeLong(indexPos)
        f.setPos(indexPos)
        f.writeInt(MAGIC)
        f.writeInt(entries.size)
        val iter = getIterator
        while (iter.hasNext) {
          val entry = iter.next().asInstanceOf[ArchiveEntry]
          f.writeJString(entry.name)
          f.writeInt(entry.time)
          f.writeLong(entry.pos)
          f.writeInt(entry.size)
        }
        f.flush()
      }
    }

    def flushInternalFiles(): Unit = {
      if (this.wFile != null) {
        this.wFile.flush()
      }
      if (this.rFile != null) {
        this.rFile.flush()
      }
    }

    def closeInternalFiles(): Unit = {
      if (this.newF) {
        this.wFile.closeNew()
      } else if (this.wFile != null) {
        this.wFile.close()
      }
      if (this.rFile != null) {
        this.rFile.close()
      }
    }

    override def cleanupAndClose(): Unit = {
      assert(!this.newF)

      val target = newArchive(this.getMergeOrCleanupArchiveName, this.db)
      val iter = this.getIterator
      while (iter.hasNext) {
        val entry = iter.next().asInstanceOf[ArchiveEntry]
        if (xPDB.isResourceAlive(entry.getName)) {
          target.copyEntry(this, entry)
        }
      }
      target.close()
      this.close()

      this.mergedOrCleanedUp = true
    }

    override def mergeAndDelete(second: xArchive.Archive): Unit = {
      val i = second.getIterator
      while (i.hasNext) {
        val entry = i.next().asInstanceOf[ArchiveEntry]
        this.copyEntry(second.asInstanceOf[Archive], entry)
      }

      second.close()
      xfs.sys.remove(second.name)
    }

    override def copyEntriesByOrder(entries: Iterator[XString], target: xArchive.Archive): Unit = {
      for (entry <- entries) {
        target.asInstanceOf[Archive].copyEntry(this, this.entries(entry))
      }
    }

    override def mergeAndCloseBoth(second: xArchive.Archive): Unit = {
      val target = newArchive(this.getMergeOrCleanupArchiveName, this.db)

      var i = second.getIterator
      while (i.hasNext) {
        val entry = i.next().asInstanceOf[ArchiveEntry]
        target.copyEntry(second.asInstanceOf[Archive], entry)
      }

      i = this.getIterator
      while (i.hasNext) {
        val entry = i.next().asInstanceOf[ArchiveEntry]
        if (!target.entries.contains(entry.getName) && xPDB.isResourceAlive(entry.getName)) {
          target.copyEntry(this, entry)
        }
      }

      second.close()
      this.close()
      target.close()

      xfs.sys.remove(second.name)

      this.mergedOrCleanedUp = true
    }

    private def copyEntry(source: Archive, entry: ArchiveEntry): Unit = {
      val sourceFile = source.openFileForRead(entry.name)
      val targetFile = this.openFileForWrite(entry.name)

      xfs.copy(sourceFile, targetFile)

      this.entries(entry.name).time = entry.time
    }

    override def getIterator: Iterator[xArchive.ArchiveEntry] = entries.valuesIterator

    /** If archive is not opened or opened in write mode, tries to open it in read mode. */
    override def openForRead(): Boolean = {
      assert(entries == null)
      // read index
      entries = mutable.HashMap.empty[XString, ArchiveEntry]
      val rawFile = xfs.raw.openToRead(name)
      if (rawFile == null) {
        return false
      }
      rFile = rawFile
      val f = xfs.sym.wrapForRead(rFile)
      if (f.readInt() != MAGIC) {
        f.close()
        return false
      }
      if (f.readInt() != VERSION) {
        f.close()
        return false
      }
      indexPos = f.readLong()
      f.setPos(indexPos)
      if (f.readInt() != MAGIC) {
        f.close()
        return false
      }
      val indexLength = f.readInt()
      for (_ <- 0 until indexLength) {
        val name = f.readJString()
        val time = f.readInt()
        val pos = f.readLong()
        val size = f.readInt()
        assert(!entries.contains(name)) // check for uniqueness of records
        entries.put(name, newArchiveEntry(this, name, time, pos, size))
      }
      assert(indexLength == entries.size)
      flushIndex = false
      newF = false
      true
    }

    /** Creates new archive, existing archive with the same name will be overwritten.
      * If archive is already opened behavior is undefined.
      */
    override def new0(): Unit = {
      entries =  mutable.HashMap.empty[XString, ArchiveEntry]
      val file = xfs.raw.openToWrite(name)
      if (file == null) {
        env.errors.fault(xfs.MSG_FILE_CREATE_ERROR, xfs.raw.errmsg)
      }
      wFile = file
      val f = xfs.sym.wrapForWrite(wFile)
      // Write an empty archive
      indexPos = 16
      f.writeInt(MAGIC)
      f.writeInt(VERSION)
      f.writeLong(indexPos) // index pos;
      f.writeInt(MAGIC)
      f.writeInt(0)
      f.flush()
      newF = true
      flushIndex = false
      ramMode = false
      lastOpened = null
    }
  }


  private class ArchiveEntry extends xArchive.ArchiveEntry {

    private[xRandomAccessArchiveModule] var archive: Archive = _
    private[xRandomAccessArchiveModule] var name: XString = _
    private[xRandomAccessArchiveModule] var time: Int = _
    private[xRandomAccessArchiveModule] var pos: Long = _
    private[xRandomAccessArchiveModule] var size: Int = _

    override def getTime: Int = this.time

    override def getName: XString = this.name

    def readEntryToRam(archiveFile: xfs.RawFile): xfs.RawFile = {
      archiveFile.setPos(pos)
      val buf = archiveFile.readFully(size)
      xRamFile.newRamFile(name, buf, size)
    }

  }

  class EntryFile extends xfs.RawFile {

    private[xRandomAccessArchiveModule] var entry: ArchiveEntry = _
    private[xRandomAccessArchiveModule] var orig: xfs.RawFile = _
    private[xRandomAccessArchiveModule] var startPos: Long = _

    override def close(): Unit = {
      entry.archive.lastOpened = null
      orig = null
      entry = null
    }

    override def closeNew(): Unit = {
      orig.flush()
      entry.size = getPosAsInt
      entry.archive.indexPos = orig.getPos
      entry.archive.flushIndex = true
      entry.archive.lastOpened = null
      entry.archive.db.onWriteFile(entry.name)
      entry.time = entry.archive.db.getPlace(entry.name).getModifyTime
      orig = null
      entry = null
    }

    override def setPos(pos: Long): Unit = {
      ensureOpened()
      assert(pos <= entry.size)
      orig.setPos(pos + startPos)
    }

    override def getPos: Long = {
      ensureOpened()
      orig.getPos - startPos
    }

    override def length: Long = entry.size

    override def writeBlock(x: Array[Byte], pos: Int, len: Int): Unit = {
      orig.writeBlock(x, pos, len)
    }

    override def readBlock(buf: Array[Byte], pos: Int, lenPar: Int): Int = {
      var len = lenPar
      ensureOpened()
      if (getPos + len > entry.size) {
        len = entry.size - getPosAsInt
      }
      val n = orig.readBlock(buf, pos, len)
      readRes = if (n < 0) xfs.endOfInput else xfs.allRight
      readLen = n max 0
      n
    }

    //----------------- EntryFile (RawFile implementation) ----------------
    def ensureOpened(): Unit = {
      if (orig == null) {
        assert(!rewriteAll)
        assert(entry.archive.ramMode)
        val f = entry.archive.getOrOpenArchiveFile(onWrite = false)
        orig = entry.readEntryToRam(f)
      }
    }

  }

  private val MAGIC: Int = 0x504442DB
  private val VERSION: Int = 2

  private def newArchiveEntry(archive: Archive, name: XString, time: Int, pos: Long, size: Int): ArchiveEntry = {
    val entry = new ArchiveEntry()
    entry.name = name
    entry.time = time
    entry.archive = archive
    entry.pos = pos
    entry.size = size
    entry
  }

  private def newRawFile(archiveFile: xfs.RawFile, entry: ArchiveEntry, writable: Boolean): xfs.RawFile = {
    val f = new EntryFile()
    f.init3(entry.name, writable, rewriteAll = writable)
    f.entry = entry
    if (!writable && entry.archive.ramMode) {
      f.orig = null
      f.startPos = 0
    } else {
      f.orig = archiveFile
      f.startPos = entry.pos
      entry.archive.lastOpened = f
    }
    f
  }

  /* --------------------------------------------------------------- */
  def openArchive(name: XString, db: xPDB.PDB): Archive = {
    val a = new Archive()
    a.init(name, db)
    if (a.openForRead()) {
      return a
    }
    null
  }

  /**
   * Creates Archive attached to given PDB
   */
  def newArchive(name: XString, db: xPDB.PDB): Archive = {
    val a = new Archive()
    a.init(name, db)
    a.new0()
    a
  }
}
