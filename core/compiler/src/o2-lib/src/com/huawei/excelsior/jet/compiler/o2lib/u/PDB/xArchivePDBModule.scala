/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u.PDB

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.Stage
import com.huawei.excelsior.jet.compiler.driver.CompilationWorker
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcNamesModule as pcNames}
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xPDBModule.{PDBKind, allArchiveContents}
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.{xArchiveModule as xArchive, xFSPlaceholdersModule as xFSPlaceholders, xPDBModule as xPDB, xRandomAccessArchiveModule as xRandomAccessArchive, xZipArchiveModule as xZipArchive}
import com.huawei.excelsior.jet.compiler.o2lib.u.{Hashtable, DirsModule as Dirs, JStringsModule as js, xcModesModule as xcModes, xiEnvModule as env, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.{FSModule as FS, TimeModule as Time}
import com.huawei.excelsior.jet.compiler.options.BoolOption.{BuildXKRN, GenProfileLibrary}
import com.huawei.excelsior.jet.compiler.options.StrOption.{LibraryName, PDBLocation}
import com.huawei.excelsior.jet.compiler.xminizip.Minizip
import com.huawei.excelsior.jet.compiler.xpackii.ArchiveUtils
import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object xArchivePDBModule {
  import xPDB.ContentType as CT

  private abstract class Placeholder(val pdb: PDB, val ctype: CT, val name: XString)
    extends xPDB.Placeholder {

    override lazy val fullName: XString = {
      // we manually construct name here instead of js.format for performance reasons
      val buf = new js.StringBuffer()
      buf.appendString(pdb.placeNames(ctype.ordinal))
      buf.appendChar(':')
      buf.appendString(name)
      buf.toJString
    }
  }

  private class ArchiveEntryPlaceholder(_pdb: PDB, _ctype: CT, _name: XString)
    extends Placeholder(_pdb, _ctype, _name) {

    override def delete(): Boolean = ???
    override def iterateDir(prefix: XString, i: xfs.DirIterator): Boolean = ???

    var pending: Boolean = false
    var bexists: Boolean = _
    var newEntry: Boolean = _
    var timestamp: Int = _

    override def exists: Boolean = bexists && (newEntry || !pdb.isMain || !(xPDB.checkReusableCompilerArtifacts contains ctype) || xPDB.isResourceReusable(name))

    /** Tries to open associated (or stubbed) entry for write as RawFile.
      * See also comment for PDB::openEntryForWrite for details about returned RawFile.
      */
    override def openAsRawForWrite(): xfs.RawFile = {
      if (xcModes.workerMode || !bexists && !pdb.reused) {
        // for smart mode always write new artifacts in wplaces
        // to not corrupt the previous PDB in case of termination during compilation
        pdb.places(ctype.ordinal).openFileForWrite(name)
      } else {
        assert(!pending) // double rewrite is not supported
        if (pdb.wplaces(ctype.ordinal) == null) {
          if (pdb.isRandomAccessArchive(ctype)) {
            pdb.wplaces(ctype.ordinal) = xRandomAccessArchive.newArchive(pdb.makePlaceName(ctype, "wpdb"), pdb)
          } else {
            pdb.wplaces(ctype.ordinal) = xZipArchive.newArchive(pdb.makePlaceName(ctype, "wzip"), pdb)
          }
        }
        pending = true
        newEntry = true
        pdb.wplaces(ctype.ordinal).openFileForWrite(name)
      }
    }

    /** Tries to open associated entry (if any) for read as RawFile.
      * See also comment for PDB::openEntryForRead for details about returned RawFile
      */
    override def openAsRawForRead(): xfs.RawFile = {
      if (exists) {
        if (xcModes.workerMode) {

          // In worker mode try shared pdb first.
          val sharedPlace = pdb.rplaces(ctype.ordinal)
          if (sharedPlace != null) {
            val sharedFile = sharedPlace.openFileForRead(name)
            if (sharedFile != null) {
              return sharedFile
            }
          }
        }
        if (!pending) {
          return pdb.places(ctype.ordinal).openFileForRead(name)
        } else {
          return pdb.wplaces(ctype.ordinal).openFileForRead(name)
        }
      }
      null
    }

    override def getModifyTime: Int = this.timestamp
  }


  private class DirectoryPlaceholder(_pdb: PDB, _ctype: CT, _name: XString)
    extends Placeholder(_pdb, _ctype, _name) {

    val children = new ArrayBuffer[Placeholder]

    override def exists: Boolean = true
    override def isDirectory: Boolean = true

    override def openAsTextForWrite(): xfs.TextFile = ???
    override def openAsTextForRead(): xfs.TextFile = ???

    override def openAsSymForWrite(): xfs.SymFile = ???
    override def openAsSymForRead(): xfs.SymFile = ???

    override def openAsRawForWrite(): xfs.RawFile = ???
    override def openAsRawForRead(): xfs.RawFile = ???

    override def delete(): Boolean = ???
    override def getModifyTime: Int = ???


    override def iterateDir(prefix: XString, di: xfs.DirIterator): Boolean = {
      for (e <- children) e match {
        case e: DirectoryPlaceholder =>
          var name = FS.getBaseName(e.name)
          if (prefix != null) {
            name = FS.addPath(prefix, name)
          }
          if (e.iterateDir(name, di)) {
            return true
          }
        case e: ArchiveEntryPlaceholder =>
          val name = FS.replacePath(e.name, prefix)
          if (di.entry(name, dir = false)) {
            return true
          }
      }

      false
    }
  }


  /**
    * Implementation of xPDB.PDB interface.
    * Entry name ending (i.e. entry extension) must be one of the four predefined ones
    * Entries are stored in 4 different archives ({pdbname}_ext.{pdb|zip})
    * Existing entries can't be overwritten
    */
  private class PDB(_kind: PDBKind, _rootDir: XString, val reused: Boolean)
    extends xPDB.PDB(_kind, _rootDir) {

    private val placeCount = xPDB.ContentType.values.length

    val placeNames = new Array[XString](placeCount)
    val places= new Array[xArchive.Archive](placeCount)
    val wplaces = new Array[xArchive.Archive](placeCount)

    // Read-only places shared between workers (e.g. sym.pdb instead of worker-specific sym1.pdb)
    val rplaces = new Array[xArchive.Archive](placeCount)

    val exts = new Array[XString](placeCount)
    var lock: xfs.RawFile = _

    private val symDirectoryIndex = mutable.HashMap.empty[XString, DirectoryPlaceholder]

    def buildSymDirectoryIndex(): Unit = { //TODO: remove
      assert(symDirectoryIndex.isEmpty)
      allPlacesIterator foreach { case place: ArchiveEntryPlaceholder =>
        if (place.ctype == CT.SYM) addDirectoryFor(place)
      }
    }

    /* ------------------------------------------------------------------------------- */
    private def addDirectoryFor(child: Placeholder): Unit = {
      val dirname = FS.getPath(child.name)
      if (!dirname.isEmpty) {
        if (!symDirectoryIndex.contains(dirname)) {
          val dir = new DirectoryPlaceholder(this, child.ctype, dirname)
          symDirectoryIndex(dirname) = dir
          addDirectoryFor(dir)
        }
        symDirectoryIndex(dirname).children += child
      }
    }

    /** Updates existance state of given entry descriptor. */
    override def onWriteFile(name: XString): Unit = {
      val e = getPlace(name).asInstanceOf[ArchiveEntryPlaceholder]
      e.bexists = true

      val unixtime = if (xPDB.stableBuild) {
        // disable timestamps for profile build to stabilize JET distros.
        0
      } else {
        Time.getTime
      }

      // use timestamp not less than dostime base point
      e.timestamp = Minizip.DOSTIME_BASE max unixtime
    }

    override def iterateAll(ct: CT, callback: XString => Unit): Unit = {
      assert(xPDB.manager != null)

      val placeNames = allPlacesIterator.collect {
        case place: ArchiveEntryPlaceholder if place.ctype == ct => FS.cutExt(place.name)
      }.to(ArrayBuffer).sorted

      placeNames foreach callback
    }


    override protected def repackByOrderImpl(ct: CT, orderedEntriesNames: Option[Iterator[XString]]): Unit = {
      val archives = if (wplaces(ct.ordinal) != null) wplaces else places
      val archive = archives(ct.ordinal)
      if (archive == null) {
        return
      }

      val repacked: xArchive.Archive = newArchive(ct, FS.addExt2(archive.name, "repacked"), archive.db)
      archive.copyEntriesByOrder(orderedEntriesNames.getOrElse(
        archive.getIterator.map(entry => entry.getName).toSeq.sorted.iterator), repacked)

      repacked.close()
      archive.close()

      xfs.sys.remove(archive.name)
      xfs.sys.rename(repacked.name, archive.name)

      if (isRandomAccessArchive(ct)) {
        // reopen archive in the same place as before
        archives(ct.ordinal) = openArchive(ct)
      }
    }

    override def mergeFromWorkers(): Unit = {
      for (ct <- allArchiveContents) {
        if (xPDB.workerContents contains ct) {
          val archive = if (wplaces(ct.ordinal) != null) wplaces(ct.ordinal) else places(ct.ordinal)
          if (archive != null) { // may be null when GenDEBUG is false
            CompilationWorker.foreach() { worker =>
              val workArchive = openArchive(ct, js.format("%d", worker))
              putEntries(ct, workArchive)
              archive.mergeAndDelete(workArchive)
            }
          }
        }
      }
    }

    override def flush(): Unit = {
      for (ct <- allArchiveContents) {
        if (xPDB.flushableContents contains ct) {
          flushOrCloseContentHolder(ct, completely = true, flush = true)
        }
      }

      // clean pending
      allPlacesIterator foreach {
        case place: ArchiveEntryPlaceholder =>
          if (xPDB.flushableContents contains place.ctype) {
            place.pending = false
          }
        case _ =>
      }
    }

    /**
     * Closes all opened zips
     * Overrides xPDB.PDB::close
     */
    override def close(): Unit = {
      for (ct <- allArchiveContents) {
        flushOrCloseContentHolder(ct, completely = false, flush = false)
      }
      for (ct <- allArchiveContents) {
        completeMergeOrCleanup(ct, flush = false)
      }
      if (!xcModes.workerMode) {
        releaseLock()
      }
    }

    /** Tries to open all zips */
    def open(): Boolean = O2Env.stage(Stage.PDBOpen) {
      // get and create jetpdb directory
      assert(rootDir.length > 0)

      if (isMain) {
        createPDBDir(rootDir)
      } else if (!xfs.sys.exists(rootDir)) {
        return false
      }

      init()

      val workerMode = isMain && xcModes.workerMode

      for (ct <- allArchiveContents) {
        val idx = ct.ordinal
        if (placeNames(idx).length > 0) {
          // open cachedobjs lazily as we can promote global cache replacing local
          // after main pdb is opened
          if (ct != CT.CACHEDOBJ) {
            if (workerMode && (xPDB.workerContents contains ct)) {
              places(idx) = newArchive(ct)
              rplaces(idx) = openArchive(ct, rplace = true)
            } else {
              places(idx) = openArchive(ct)
              rplaces(idx) = null
              if (places(idx) == null && !ignoreOpenFailure(ct)) {
                close()
                return false
              }
            }
          }
        } else {
          places(idx) = null
          rplaces(idx) = null
        }
        wplaces(idx) = null
      }


      // put existing entries into hashtable
      for (ct <- allArchiveContents) {
        val workerPlace = workerMode && (xPDB.workerContents contains ct)
        if (workerPlace) {
          if (rplaces(ct.ordinal) != null) {
            putEntries(ct, rplaces(ct.ordinal))
          }
        } else if (places(ct.ordinal) != null) {
          putEntries(ct)
        }
      }

      true
    }

    override def findPlaceToWriteTo(namePar: XString, ct: CT): xPDB.Placeholder = {
      ensureOpen(ct, forWrite = true)

      val name = xPDB.createPlaceName(namePar, ct)
      var generic = getPlaceOrNull(name)

      if (generic != null) {
        return generic
      }

      ct match {
        case CT.OBJ |
             CT.SYM |
             CT.DBG |
             CT.IRB |
             CT.IREI |
             CT.MOD |
             CT.CACHEDOBJ =>
          if (places(ct.ordinal) == null) {
            assert(placeNames(ct.ordinal) != null)
            places(ct.ordinal) = newArchive(ct)
          }
          val entry = new ArchiveEntryPlaceholder(this, ct, name)
          entry.bexists = false
          entry.newEntry = true
          entry.timestamp = Int.MinValue
          return addPlace(name, entry)

        case CT.TMPRES =>
          if (name.equals2("tmpres")) {
            generic = xFSPlaceholders.newFilenamePlaceholder(this, addDirPrefix(name))
          } else {
            generic = xFSPlaceholders.newFilenamePlaceholder(this, name)
          }
        case _ =>
          if (xPDB.toJETPDBDir contains ct) {
            if (O2Env.env.enabled(BuildXKRN) && xfs.sys.existLookups(makePattern(ct))) {
              generic = xFSPlaceholders.newFilenamePlaceholder(this, xfs.sys.useFirst(name))
            } else {
              generic = xFSPlaceholders.newFilenamePlaceholder(this, addDirPrefix(name))
            }
          } else {
            generic = xFSPlaceholders.getDefaultPlaceToWriteTo(this, name)
          }
      }
      addPlace(name, generic)
    }

    override def findPlaceToReadFrom(namePar: XString, ct: CT): xPDB.Placeholder = {
      ensureOpen(ct, forWrite = false)

      val name = xPDB.createPlaceName(namePar, ct)
      var place = getPlaceOrNull(name)

      if (place == null || !place.exists) {
        if (ct == CT.SET || ct == CT.REPL) {
          place = xFSPlaceholders.newFilenamePlaceholder(this, addDirPrefix(name))
          if (!place.exists) {
            return null
          }
          addPlace(name, place)
        } else if ((xPDB.respectedLookups contains ct) && O2Env.env.enabled(BuildXKRN) && xfs.sys.existLookups(makePattern(ct))) {
          val fd = xfs.sys.lookup(name)
          if (fd.exists) {
            place = xFSPlaceholders.newFDPlaceholder(this, fd)
            addPlace(name, place)
          }
        } else {
          return null
        }
      }

      place
    }

    def ensureOpen(ct: CT, forWrite: Boolean): Unit = {
      if (!allArchiveContents.contains(ct)) {
        // not archive content type
        return
      }
      if (places(ct.ordinal) == null) {
        places(ct.ordinal) = openArchive(ct)
        if (places(ct.ordinal) != null) {
          putEntries(ct)
        } else if (forWrite) {
          places(ct.ordinal) = newArchive(ct)
        }
      }
    }

    override def findDirectory(name: XString, ct: CT): xPDB.Placeholder = {
      symDirectoryIndex.get(name).orNull
    }

    override def getContentHolder(ct: CT): xPDB.Placeholder = {
      assert(placeNames(ct.ordinal) != null)
      // close content holder to let manipulate with it as a whole (openForRead);
      flushOrCloseContentHolder(ct, completely = true, flush = false)
      xFSPlaceholders.newFilenamePlaceholder(this, placeNames(ct.ordinal))
    }

    private def flushOrCloseContentHolder(ct: CT, completely: Boolean, flush: Boolean): Unit = {
      val idx = ct.ordinal
      if (rplaces(idx) != null) {
        rplaces(idx).close()
        rplaces(idx) = null
      }
      if (places(idx) != null) {
        if (wplaces(idx) != null) {
          places(idx).mergeAndCloseBoth(wplaces(idx))
          if (completely) {
            completeMergeOrCleanup(ct, flush)
          }
          wplaces(idx) = null
        } else if (isMain && !xcModes.workerMode && reused && (xPDB.cleanableContents contains ct)) {
          places(idx).cleanupAndClose()
          if (completely) {
            completeMergeOrCleanup(ct, flush)
          }
        } else if (flush) {
          places(idx).flush()
        } else {
          places(idx).close()
          places(idx) = null
        }
      }
    }

    private def completeMergeOrCleanup(ct: CT, flush: Boolean): Unit = {
      val idx = ct.ordinal
      if (places(idx) != null) {
        places(idx).completeMergeOrCleanup()
        if (!flush) {
          places(idx) = null
        } else {
          places(idx) = openArchive(ct)
        }
      }
    }

    private def addDirPrefix(filename: XString): XString = FS.addPath(rootDir, filename)

    // Archives that should be created at PDB creation
    private val createOnNew = CT.Set(CT.SYM, CT.OBJ, CT.IRB, CT.IREI, CT.MOD)

    /** Creates all zips */
    def create(): Unit = {
      assert(rootDir.length > 0)

      createPDBDir(rootDir)

      init()
      for (ct <- allArchiveContents) {
        val idx = ct.ordinal
        if (createOnNew(ct)) {
          places(idx) = newArchive(ct)
        } else {
          places(idx) = null
        }
        wplaces(idx) = null

        if (xcModes.workerMode) {
          rplaces(idx) = openArchive(ct, rplace = true)
        } else {
          rplaces(idx) = null
        }
      }
    }

    private def newArchive(ct: CT): xArchive.Archive = {
      newArchive(ct, placeNames(ct.ordinal), this)
    }

    private def newArchive(ct: CT, name: XString, db: xPDB.PDB): xArchive.Archive = {
      if (isRandomAccessArchive(ct)) {
        xRandomAccessArchive.newArchive(name, db)
      } else {
        xZipArchive.newArchive(name, db)
      }
    }

    private def openArchive(ct: CT, worker: XString = null, rplace: Boolean = false): xArchive.Archive = {
      val name = if (worker != null || rplace) {
        if (isRandomAccessArchive(ct)) {
          makePlaceName(ct, "pdb", worker)
        } else {
          makePlaceName(ct, "zip", worker)
        }
      } else {
        placeNames(ct.ordinal)
      }
      if (isRandomAccessArchive(ct)) {
        xRandomAccessArchive.openArchive(name, this)
      } else {
        xZipArchive.openArchive(name, if (isMain) this else null)
      }
    }

    private def putEntries(ct: CT, aPar: xArchive.Archive = null): Unit = {
      val a = if (aPar != null) aPar else places(ct.ordinal)
      val i = a.getIterator
      while (i.hasNext) {
        val entry = i.next()

        val ph = new ArchiveEntryPlaceholder(this, ct, entry.getName)
        ph.timestamp = entry.getTime
        ph.bexists = true
        ph.newEntry = false
        addPlace(ph.name, ph)
      }
    }

    /** Inits auxiliary PDB structures
      * (strings containing extensions, regular expressions)
      * Also creates directories, registers `fake entries`
      */
    private def init(): Unit = {
      this.lock = null

      // get extensions
      for (ct <- allArchiveContents) {
        exts(ct.ordinal) = xPDB.getExtensionFor(ct)
      }

      val worker = if (isMain) {
        // we may reset placement for obj.zip when compiling profile via "OBJECTS" equations.
        placeNames(CT.OBJ.ordinal) = env.config.equation("OBJECTS")
        env.config.equation("worker")
      } else {
        null
      }

      for (ct <- allArchiveContents) {
        if (placeNames(ct.ordinal) == null) {
          val ext = if (isRandomAccessArchive(ct)) "pdb" else "zip"
          placeNames(ct.ordinal) = makePlaceName(ct, ext, worker)
        }
      }

      if (isMain && targetArch != CBC) {
        // in case "OBJECTS" equation was not set before compilation, set it to real value
        val objzip = FS.HOST.toPlatform(placeNames(CT.OBJ.ordinal))
        env.config.setEquation2("OBJECTS", objzip)
      }

      if (worker == null && !acquireLock()) {
        val projectName = env.config.equation("PRJ")
        if (projectName != null) {
          env.errors.fault(xPDB.MSG_CANNOT_LOCK_PDB_FOR, projectName, rootDir)
        } else {
          env.errors.fault(xPDB.MSG_CANNOT_LOCK_PDB, rootDir)
        }
      }
    }

    def isRandomAccessArchive(ct: CT): Boolean = ct match {
      case CT.SYM | CT.DBG | CT.IRB | CT.IREI | CT.MOD => true
      case _ => false
    }

    def makePlaceName(ct: CT, archiveExt: String, worker: XString = null): XString = {
      val name = ct match {
        case CT.CACHEDOBJ => env.getRTCacheFileName
        case _ if worker != null && (xPDB.workerContents contains ct) =>
          exts(ct.ordinal).concat(worker)
        case _ =>
          exts(ct.ordinal)
      }
      FS.makeFileName(rootDir, name, js.newJString(archiveExt))
    }

    private def releaseLock(): Unit = {
      if (lock == null) {
        return
      }

      lock.closeNew()
      lock = null

      val lockfilename = makeLockFilename()
      assert(xfs.sys.exists(lockfilename))
      // Sometimes Windows blocks to delete a file.
      // Ignore this. We should be able to lock the file anyway next time.
      xfs.sys.remove(lockfilename)
    }

    private def acquireLock(): Boolean = {
      if (!isMain) {
        // only main PDB is subject for locking
        return true
      }

      assert(lock == null) // double acquire?

      val lockfilename = makeLockFilename()

      if (xfs.sys.exists(lockfilename)) {
        if (!xfs.sys.remove(lockfilename) || xfs.sys.exists(lockfilename)) {
          return false
        }
      }

      val file = xfs.raw.openToWrite(lockfilename)
      if (file == null) {
        // failed to open, ignore locking:
        // pdb may be write protected for imported pdb
        return true
      }
      lock = file

      xfs.sys.exists(lockfilename)
    }

    private def makeLockFilename(): XString = FS.addPath(rootDir, js.newJString("lock"))
  }


  class Constructor {
    def makePDBName(outputname: XString, projectfile: XString): XString = {
      val pdbLocation = XString(O2Env.env.valueOfOrNull(PDBLocation))
      if (isEmpty(pdbLocation)) {
        if (O2Env.env.enabled(GenProfileLibrary)) {
          env.getProfileLibraryPath(O2Env.env.valueOf(LibraryName))
        } else if (!isEmpty(projectfile)) {
          js.format("%S_jetpdb", FS.cutExt(projectfile))
        } else if (!isEmpty(outputname)) {
          js.format("%S_jetpdb", outputname)
        } else {
          js.newJString("./jetpdb")
        }
      } else {
        pdbLocation
      }
    }

    def open(kind: PDBKind, pdbName: XString): xPDB.PDB = {
      val pdbDir = if (ArchiveUtils.isZipArchive(pdbName.toString)) {
        val unpackedPdb = ArchiveUtils.unzipOnce(pdbName.toString)
        XString.ascii(unpackedPdb)
      } else {
        pdbName
      }

      val pdb = new PDB(kind, pdbDir, true)
      if (pdb.open()) pdb else null
    }

    def createMain(pdbName: XString): xPDB.PDB = {
      val pdb = new PDB(PDBKind.Main, pdbName, false)
      pdb.create()
      pdb
    }
  }

  var ctor: Constructor = _
  private val pats = new Array[XString](CT.values.length)

  private def createPDBDir(name: XString): Unit = {
    if (!Dirs.mkdirs(name)) {
      env.errors.fault(xPDB.MSG_CANNOT_CREATE_SMTH, "directory", name)
    }
  }

  private def makePattern(ct: CT): XString = {
    if (pats(ct.ordinal) == null) {
      pats(ct.ordinal) = js.format("*.%S", xPDB.getExtensionFor(ct))
    }
    pats(ct.ordinal)
  }

  private def ignoreOpenFailure(ct: CT): Boolean = ct match {
    case CT.SYM => false
    case CT.OBJ => true
    case _      => true
  }

  private def isEmpty(s: XString): Boolean = s == null || s.isEmpty

  def getProfilePlaceName(ct: CT): XString = {
    def archiveNameByType(ct: CT): String = (ct: @unchecked) match {
      case CT.OBJ => "obj.zip"
      case CT.DBG => "dbg.pdb"
      case CT.SYM => "sym.pdb"
      case CT.IRB => "irb.pdb"
      case CT.IREI => "irei.pdb"
      case CT.REPL => "repl.pdb"
    }

    js.format("%S/%s", env.getDevelopDir, archiveNameByType(ct))
  }

  def openProfilePDB(): xPDB.PDB = {
    val pdb = new PDB(PDBKind.Profile, env.getDevelopDir, true)

    for (ct <- Seq(CT.OBJ, CT.DBG, CT.SYM, CT.IRB, CT.IREI)) {
      pdb.placeNames(ct.ordinal) = getProfilePlaceName(ct)
    }

    if (pdb.open()) {
      pdb.buildSymDirectoryIndex()
      pdb
    } else {
      null
    }
  }

  def initCtor(): Unit = {
    ctor = new Constructor()
  }
}
