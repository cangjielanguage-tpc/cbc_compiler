/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u.PDB

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.common.XString.xstr
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xArchiveModule.ArchiveEntry
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.{xArchiveModule as xArchive, xPDBModule as xPDB}
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xRamFileModule as xRamFile, xiEnvModule as env, xiFilesModule as xfs, xmErrorsModule as xmErrors}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.JZip.{ZipEntry, ZipFileModule as ZipFile}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.MemoryManagementModule as mm
import com.huawei.excelsior.jet.compiler.xminizip.Minizip
import com.huawei.excelsior.o2s.runtime.*
import xscala.io.Path
import xscala.util.UByte

import java.io.IOException
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Using

object xZipArchiveModule {
  type Mode = UByte
  val Read: Mode = UByte(0)
  val Write: Mode = UByte(1)

  /** Archive implementation based on zip format.
    *
    * NOTE: files more than 2G inside zip archives are not supported yet.
    */
  class Archive extends xArchive.Archive {

    private[xZipArchiveModule] var mode: Mode = _
    private[xZipArchiveModule] var zf: ZipFile.ZipFile = _
    private[xZipArchiveModule] var zw: Minizip.Writer = _

    /**
     * Writes contents of f to archive, it is invoked by WOFile::CloseNew
     */
    def writeFile(f: WOFile): Unit = {
      openForAppend()
      db.onWriteFile(f.getName)
      val mtime = db.getPlace(f.getName).getModifyTime
      try {
        zw.putBytesAndExtraToArchive(f.f.getBytes, null, Path.rel(f.getName.utf8ToString), mtime)
      } catch {
        case _: IOException => env.errors.fault(xPDB.MSG_CANNOT_WRITE_ZIPENTRY, f.getName, name)
      }
    }

    /**
     * openFileForRead does not check anything, 
     * be sure to check both archive & file existance before calling openFileForRead
     * Returned RawFile is _currently_ is lazy RamFile
     */
    override def openFileForRead(entry: XString): xfs.RawFile = {
      this.openForReadLocal()
      var ze = this.zf.getEntry(entry)
      if (ze == null) {
        mm.compactHeap()
        ze = this.zf.getEntry(entry)
        if (ze == null) {
          xmErrors.printMem(doPrintErr = false)
          env.errors.fault(xPDB.MSG_CANNOT_READ_ZIPENTRY, entry, this.name)
        }
      }

      val rf = new ROFile()
      rf.init3(entry, writeable = false, rewriteAll = false)
      rf.f = null
      rf.archive = this
      rf.len = ze.getSize
      rf.pos = 0
      rf
    }

    def loadFileIntoMemory(entry: XString): xRamFile.RamFile = {
      this.openForReadLocal()
      val ze = this.zf.getEntry(entry)

      if (ze == null) {
        env.errors.fault(xPDB.MSG_CANNOT_FIND_ZIPENTRY, entry, this.name)
      }

      val buf = new Array[Byte](ze.getSize)
      val is = this.zf.getInputStream(ze)
      if (!is.read(buf)) {
        env.errors.fault(xPDB.MSG_CANNOT_READ_ZIPENTRY, entry, this.name)
      }
      xRamFile.newRamFile(entry, buf, ze.getSize)
    }

    /**
     * openFileForWrite doesn't check anything
     * be sure to check that Archive doesn't contain entry before calling openFileForWrite
     */
    override def openFileForWrite(entry: XString): xfs.RawFile = {
      val wf = new WOFile()
      wf.init3(entry, writeable = true, rewriteAll = false) //TODO: should be `rewriteAll = true` ??
      wf.archive = this
      wf.f = xRamFile.newRamFile(entry)
      wf
    }

    override def close(): Unit = {
      if (this.mode == Write) {
        assert(this.zf == null)
        if (this.zw != null) {
          this.zw.close()
          this.zw = null
        }
      } else {
        // this.mode = Read 
        assert(this.zw == null)
        if (this.zf != null) {
          this.zf.close()
          this.zf = null
        }
      }
    }

    override def cleanupAndClose(): Unit = {
      val target = newArchive(this.name.concat(js.newJString(".tmp")), this.db)

      Minizip.copyEntriesToWriter(this.zf.name, ze => xPDB.isResourceAlive(xstr(ze)), target.zw)

      this.close()
      target.close()

      this.mergedOrCleanedUp = true
    }

    override def copyEntriesByOrder(entries: Iterator[XString], target: xArchive.Archive): Unit = {
      this.close()
      Using.resource(Minizip.openReader(this.name.toString)) { zfile =>
        val allEntries = zfile.allEntries() // do not load entries with bytes here, it may be too memory exhaustive
        for (entry <- entries; zipEntry <- allEntries.get(entry.utf8ToString)) {
          zfile.copyEntryAtPosToWriter(zipEntry.pos, target.asInstanceOf[Archive].zw)
        }
      }
    }

    override def mergeAndDelete(second: xArchive.Archive): Unit = {
      second.close()
      this.zw.putAllFromAnotherZipToArchive(Path(second.name.toString))
      xfs.sys.remove(second.name)
    }

    override def mergeAndCloseBoth(second: xArchive.Archive): Unit = {
      val targetZW = try {
        Minizip.openWriter(this.getMergeOrCleanupArchiveName.toString)
      } catch {
        case _: IOException => env.errors.fault(xPDB.MSG_CANNOT_CREATE_SMTH, "zip archive", this.name)
      }

      // copy all entries from second
      second.close()
      targetZW.putAllFromAnotherZipToArchive(Path(second.name.toString))
      val secondEntries = Using.resource(Minizip.openReader(second.name.toString)) { zr => zr.allEntries().keySet }
      xfs.sys.remove(second.name)

      // copy this entries which are not in second
      this.close()
      def notInSecond(ze: String): Boolean = !secondEntries.contains(ze) && xPDB.isResourceAlive(xstr(ze))
      Minizip.copyEntriesToWriter(this.name.toString, notInSecond, targetZW)

      targetZW.close()
      this.mergedOrCleanedUp = true
    }

    /**
     * Returns ZipFile Enumeration, which allows iterating through zip contents
     * Note: Usage of the method is unsafe! Do it with care
     *
     * TODO: The best solution is to introduce some locking API to ensure that
     * Archive wont be closed or reopened for write during Iterator usage
     */
    override def getIterator: Iterator[xArchive.ArchiveEntry] = {
      this.openForReadLocal()
      assert(this.zf != null)

      zf.entries.map(new ArchiveEntry(_))
    }

    def openForReadLocal(): Unit = {
      if (!this.openForRead()) {
        env.errors.fault(xPDB.MSG_CANNOT_OPEN_ZIP, this.name)
      }
    }

    /**
     * If archive is not opened or opened in write mode, tries to open it in read mode
     */
    override def openForRead(): Boolean = {
      if (this.zf != null && this.mode == Read) {
        return true
      }
      this.close()

      this.zf = ZipFile.newZipFile(this.name)
      this.mode = Read

      if (this.zf == null) {
        this.zf = ZipFile.newZipFile(this.name)
      }

      this.zf != null
    }

    /**
     * If archive is not opened or opened in read mode, openes it in write mode
     * Existing archive will be preserved and opened in appending mode
     */
    def openForAppend(): Unit = {
      assert(!this.readonly)
      if (this.zw != null && this.mode == Write) {
        return
      }
      this.close()

      this.zw = try {
        Minizip.openWriter(this.name.toString, true)
      } catch {
        case _: IOException => null
      }
        
      this.mode = Write

      if (this.zw == null) { // open failed?
        this.new0()
      }
    }

    /* ------------------------------- Archive ------------------------------- */
    /**
     * Creates new archive, existing archive with the same name will be overwritten
     * If archive is already opened behavior is undefined
     */
    override def new0(): Unit = {
      assert(!this.readonly)
      assert(this.zf == null)

      this.zw = try {
        Minizip.openWriter(this.name.toString)
      } catch {
        case _: IOException => env.errors.fault(xPDB.MSG_CANNOT_CREATE_SMTH, "zip archive", this.name)
      }
      this.mode = Write
    }

    override def flush(): Unit = ???
  }

  private class ArchiveEntry(private val entry: ZipEntry) extends xArchive.ArchiveEntry {
    override def getTime: Int = entry.getTime
    override def getName: XString = entry.getName
  }

  class WOFile extends xfs.RawFile {

    private[xZipArchiveModule] var archive: Archive = _
    private[xZipArchiveModule] var f: xRamFile.RamFile = _

    override def closeNew(): Unit = {
      assert(this.f != null)
      this.archive.writeFile(this)
      this.f = null
    }

    override def close(): Unit = {
      throw new AssertionError
    }

    override def setPos(pos: Long): Unit = {
      this.f.setPos(pos)
    }

    override def getPos: Long = this.f.getPos

    override def length: Long = this.f.length

    override def writeBlock(x: Array[Byte], pos: Int, len: Int): Unit = {
      this.f.writeBlock(x, pos, len)
    }

    override def readBlock( x: Array[Byte], pos: Int, len: Int): Int = {
      throw new AssertionError
    }
  }

  /** ROFile is in fact lazy RamFile,
   ** that is it loads everything into memory but only
   ** when ReadBlock is invoked for the first time
   */
  class ROFile extends xfs.RawFile {

    private[xZipArchiveModule] var archive: Archive = _
    private[xZipArchiveModule] var f: xRamFile.RamFile = _
    private[xZipArchiveModule] var len: Int = _
    private[xZipArchiveModule] var pos: Int = _

    override def close(): Unit = {
      if (this.f != null) {
      }
      this.f = null
      this.archive = null
    }

    override def closeNew(): Unit = {
      throw new AssertionError
    }

    override def setPos(pos: Long): Unit = {
      assert(pos < Integer.MAX_VALUE)
      if (this.f == null) {
        this.pos = pos.toInt
        return
      }
      this.f.setPos(pos)
    }

    override def getPos: Long = {
      if (this.f == null) {
        return this.pos
      }
      this.f.getPos
    }

    override def length: Long = this.len

    override def writeBlock(x: Array[Byte], pos: Int, len: Int): Unit = {
      throw new AssertionError
    }

    override def readBlock(buf: Array[Byte], pos: Int, len: Int): Int = {
      if (f == null) {
        assert(archive != null)
        f = archive.loadFileIntoMemory(getName)
        archive = null
        if (pos != 0) {
          f.setPos(pos)
        }
      }

      val n = f.readBlock(buf, pos, len)
      readRes = f.readRes
      readLen = f.readLen
      n
    }
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
