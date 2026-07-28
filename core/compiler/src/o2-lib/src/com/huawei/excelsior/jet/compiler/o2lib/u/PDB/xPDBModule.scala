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
import com.huawei.excelsior.jet.compiler.PDB2.{EntryKind, Location}
import com.huawei.excelsior.jet.compiler.driver.ProjectLogic
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{DirsModule as Dirs, JStringsModule as js, TextFileModule as tf, xiEnvModule as env, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule as FS
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.o2s.runtime.*
import xscala.io.{Files, Path}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object xPDBModule {
  import ContentType as CT

  /** Presents abstract Project Database (PDB) interface. */
  abstract class PDB(val kind: PDBKind, val rootDir: XString) {
    private val entries = mutable.HashMap.empty[XString, Placeholder]

    def isMain = kind == PDBKind.Main
    def isProfile = kind == PDBKind.Profile

    /** `onWriteFile`` is an event callback */
    def onWriteFile(name: XString): Unit = {}

    /** Iterates over all files in PDB and checks that they are not contained in other PDBs. */
    def checkConsistency(): Boolean = true

    /** Iterates over all files in PDB of given type and calls callback on them. */
    def iterateAll(type0: ContentType, callback: XString => Unit): Unit

    // By default entries will be sorted in lexicographic order
    final def repackByOrder(type0: ContentType): Unit = {
      repackByOrderImpl(type0, None)
    }

    final def repackByOrder(type0: ContentType, orderedEntriesNames: Iterator[XString]): Unit = {
      assert(orderedEntriesNames != null)
      repackByOrderImpl(type0, Some(orderedEntriesNames))
    }

    protected def repackByOrderImpl(type0: ContentType, orderedEntriesNames: Option[Iterator[XString]]): Unit

    def mergeFromWorkers(): Unit

    def flush(): Unit

    /** Closes PDB. If any PDB entry is opened as file, behavior is undefined. */
    def close(): Unit

    def findPlaceToWriteTo(name: XString, type0: ContentType): Placeholder

    def findPlaceToReadFrom(name: XString, type0: ContentType): Placeholder

    def findDirectory(name: XString, type0: ContentType): Placeholder

    def getContentHolder(type0: ContentType): Placeholder

    def getPlaceOrNull(name: XString) = entries.get(name).orNull

    def hasPlace(name: XString) = entries contains name

    def getPlace(name: XString) = entries(name)

    def addPlace(name: XString, place: Placeholder): Placeholder = { entries(name) = place; place }

    protected def allPlacesIterator = entries.valuesIterator
  }

  enum PDBKind {
    case Main, Profile, Library, LibResources, Other
  }

  /** Manager manages PDB lifetime.
    * Manager must support at least one PDB (called main).
    * Manager can also support several auxiliary PDBs.
    */
  abstract class Manager {
    def mainPDB: PDB
    def profilePDB: PDB
    def libraryPDB: PDB

    def findPlaceToWriteTo(name: XString, type0: ContentType): Placeholder

    def findPlaceToReadFrom(name: XString, type0: ContentType, skipProfile: Boolean): Placeholder

    def findDirectory(name: XString, type0: ContentType): Placeholder

    /** Closes all opened PDBs. */
    def closeAll(): Unit

    def registerAuxPDB(pdb: PDB): Unit

    /** Returns main PDB state. */
    def isMainPDBOpened: Boolean

    def mainPDBDir: XString

    /** Tries to open existing PDB, if succeeds sets it as main pdb.
      * If main pdb is already set behavior is undefined.
      */
    def openMainPDB(name: XString): Boolean

    /** Creates new PDB with given name and sets it as main pdb.
      * If `reset` is true, close already opened main pdb at first.
      */
    def createMainPDB(name: XString, reset: Boolean): Unit

    /** Creates PDB Name based on the values of different equations: OUTPUTNAME, PRJ, etc. */
    def makePDBName(outputname: XString, projectfile: XString): XString
  }

  enum ContentType(val ext: String, val extEquation: String = null) {
    case UNSUPPORTED extends ContentType("UNSUPPORTED", "UNSUPPORTED")
    case SYM         extends ContentType("sym",     "SYM")
    case OBJ         extends ContentType("obj",     "OBJEXT")
    case DBG         extends ContentType("dbg",     "DBGEXT")
    case IRB         extends ContentType("irb",     "IRB")
    case IREI        extends ContentType("irei",    "IREI")
    case MOD         extends ContentType("mod")
    case _UNUSED_1   extends ContentType("cho")
    case CACHEDOBJ   extends ContentType("objc")
    case REPL        extends ContentType("pdb")
    case RSP         extends ContentType("rsp",     "MKFEXT")
    case EFS         extends ContentType("tmp",     "EFSEXT")
    case TMPRES      extends ContentType("")
    case _UNUSED_2   extends ContentType("meta.sym")
    case VCFZIP      extends ContentType("vcfzip")
    case FUS         extends ContentType("fus",     "FUSEXT")
    case LIB         extends ContentType("lib",     "LIB")
    case RES         extends ContentType("res",     "RESEXT")
    case EFSDATA     extends ContentType("efsdata", "EFSDATA")
    case LI          extends ContentType("li",      "LIEXT")
    case RC          extends ContentType("rc",      "RCEXT")
    case ENV         extends ContentType("env")
    case CLIDTABLE   extends ContentType("table")
    case OBJ_LIB     extends ContentType("objlib")
    case SET         extends ContentType("set")
  }

  object ContentType {
    type Set = collection.Set[ContentType]

    def Set(xs: ContentType*): Set = mutable.LinkedHashSet(xs: _*)
  }

  /** Where to write/from where to read */
  abstract class Placeholder {
    def pdb: PDB

    def getFileDescriptor: xfs.FileDescriptor = new FileDescriptor(this)

    def openAsTextForWrite(): xfs.TextFile = tf.newTextFile(openAsRawForWrite(), read = false)

    def openAsTextForRead(): xfs.TextFile = tf.newTextFile(openAsRawForRead(), read = true)

    def openAsSymForWrite(): xfs.SymFile = {
      val raw = openAsRawForWrite()
      if (raw == null) {
        return null
      }
      xfs.sym.wrapForWrite(raw)
    }

    def openAsSymForRead(): xfs.SymFile = {
      val raw = openAsRawForRead()
      if (raw == null) {
        return null
      }
      xfs.sym.wrapForRead(raw)
    }

    def openAsRawForWrite(): xfs.RawFile

    def openAsRawForRead(): xfs.RawFile

    def getModifyTime: Int

    def delete(): Boolean

    def exists: Boolean

    def fullName: XString

    def iterateDir(prefix: XString, i: xfs.DirIterator): Boolean

    def isDirectory: Boolean = false
  }


  class FileDescriptor(val place: Placeholder) extends xfs.FileDescriptor {
    override def openRawFile(): xfs.RawFile = place.openAsRawForRead()

    override def openSymFile(): xfs.SymFile = place.openAsSymForRead()

    override def openTextFile(): xfs.TextFile = place.openAsTextForRead()

    override def iterateDir(i: xfs.DirIterator): Boolean = place.iterateDir(null, i)

    override def getDir(name: XString): xfs.FileDescriptor = ???

    override def isDirectory: Boolean = place.isDirectory

    override def exists: Boolean = true

    override def modifyTime(): Int = place.getModifyTime

    /* ------------------------------- FileDescriptor ------------------------------- */
    override def getName: XString = place.fullName

    override def getIterator = throw new AssertionError()

    override def getEntry(name: XString, ext: XString): xfs.FileDescriptor = null
  }

  /** During smart re-compilation some resources that were resided in PDB for the previous compilation
    * can become obsolete (don't exist in the current shape of project).
    * As PDB knows nothing about this, the project system should implement
    * the ResourceCleanupAdviser interface to let PDB to sweep dead entries.
    */
  trait ResourceCleanupAdviser {
    /** Returns `true`, if the given PDB resource can be reused in the current compilation session
      * (that means that the respective class of the resource is decided to be not-recompiled).
      *
      * The methods is used to hide obsolete resources from the PDB during their requesting on existing.
      * If the resource is reusable then it is alive but not all alive resources are reusable.
      */
    def isResourceReusable(placename: XString): Boolean

    /** Returns `true`, if the given PDB resource persists for the current project.
      * The methods is used to remove obsolete resources from the PDB on PDB closing.
      */
    def isResourceAlive(placename: XString): Boolean
  }

  lazy val allArchiveContents: Seq[ContentType] = {
    if (pcOModule.isCangjie) {
      Seq(CT.SYM)
        ++ Option.when(ProjectLogic.openIRAndExtraInfoPDB)(Seq(CT.IRB, CT.IREI)).toSeq.flatten
        ++ Option.when(targetArch != CBC)(CT.OBJ)
    } else {
      Seq(CT.SYM, CT.OBJ, CT.IRB, CT.IREI, CT.MOD, CT.CACHEDOBJ, CT.DBG)
    }
  }

  val ignoredLookups = ContentType.Set(CT.SYM, CT.OBJ, CT.DBG, CT.IRB, CT.IREI, CT.MOD, CT.RSP, CT.EFS, CT.EFSDATA, CT.CACHEDOBJ, CT.VCFZIP)
  val cleanableContents = ContentType.Set(CT.SYM, CT.OBJ, CT.DBG, CT.IRB, CT.IREI, CT.MOD)
  val flushableContents = ContentType.Set(CT.SYM, CT.DBG, CT.IRB, CT.IREI, CT.MOD)
  val workerContents = ContentType.Set(CT.OBJ, CT.DBG, CT.IRB, CT.IREI)
  val parsingStageCompilerArtifacts = ContentType.Set(CT.SYM, CT.MOD)

  /** Compiler artifacts that we consult for re-usability with project system. */
  val checkReusableCompilerArtifacts = ContentType.Set(CT.IRB, CT.IREI)

  // Note: C._OBJ lookups will be respected only for read operations
  // See option "respectlookups"
  val respectedLookups = ContentType.Set(CT.RSP, CT.EFS, CT.EFSDATA, CT.VCFZIP, CT.OBJ)

  /** Files of these types will be placed in jetpdb directory. */
  val toJETPDBDir = ContentType.Set(CT.RSP, CT.EFS, CT.EFSDATA, CT.VCFZIP, CT.RC, CT.RES, CT.LI, CT.CLIDTABLE, CT.OBJ_LIB, CT.SET, CT.REPL)

  val MSG_CANNOT_OPEN_ZIP = ErrMsg489 /* %S %S */
  val MSG_CANNOT_FIND_ZIPENTRY = ErrMsg491 /* %S %S */
  val MSG_CANNOT_READ_ZIPENTRY = ErrMsg492 /* %S %S */
  val MSG_CANNOT_WRITE_ZIPENTRY = ErrMsg493 /* %S %S */
  val MSG_CANNOT_LOCK_PDB_FOR = ErrMsg496 /* %S %S */
  val MSG_CANNOT_LOCK_PDB = ErrMsg498 /* %S */
  val MSG_CANNOT_CREATE_SMTH = ErrMsg503 /* %s %S */

  var manager: Manager = _
  var isProfileBuild: Boolean = _
  def stableBuild: Boolean = isProfileBuild || O2Env.env.enabled(PackPDB)
  private var resourceCleanupAdviser: ResourceCleanupAdviser = _

  /* ------------------------------------------------------------------------------ */
  def getExtensionFor(ct: ContentType): XString = {
    if (ct == CT.TMPRES) {
      return js.jstrEmpty
    }

    var ext: XString = null
    if (ct.extEquation != null) {
      ext = env.config.equation(ct.extEquation)
      //assert(ext == null) // TODO: burn extEquation with fire
    }
    if (ext == null || ext.length == 0) {
      assert(ct.ext != "")
      ext = js.newJString(ct.ext)
    }
    ext
  }

  private lazy val typeByExtMap = {
    CT.values.filter(_ != CT.UNSUPPORTED).map(t => (getExtensionFor(t), t)).toMap
  }

  def getTypeByExt(file: XString): ContentType = {
    val pos = file.lastIndexOf('.')
    if (pos < 0) {
      CT.UNSUPPORTED // files with no extension do not have any PDB types
    } else {
      val ext = file.substring(pos + 1)
      typeByExtMap.getOrElse(ext, CT.UNSUPPORTED)
    }
  }

  def getLocationType(loc: Location): ContentType = (loc.kind: @unchecked) match {
    case EntryKind.IR => CT.IRB
    case EntryKind.ExtraInfo => CT.IREI
    case EntryKind.ModuleInfo => CT.MOD
    case EntryKind.DelayedUsage => CT.SET
    case EntryKind.Repl => CT.REPL
  }

  def getNameByPlaceName(file: XString): XString = FS.cutExt(file)

  def createPlaceName(name: XString, type0: ContentType): XString = {
    if (type0 != CT.TMPRES) {
      FS.addExt(name, getExtensionFor(type0))
    } else {
      name
    }
  }

  def findPlaceToWriteTo(name: XString, type0: ContentType): Placeholder = manager.findPlaceToWriteTo(name, type0)

  def findPlaceToReadFrom(name: XString, type0: ContentType, skipProfile: Boolean = false): Placeholder = manager.findPlaceToReadFrom(name, type0, skipProfile)

  def isResourceAlive(placename: XString): Boolean = resourceCleanupAdviser == null || resourceCleanupAdviser.isResourceAlive(placename)

  def isResourceReusable(placename: XString): Boolean = resourceCleanupAdviser == null || resourceCleanupAdviser.isResourceReusable(placename)

  private def getPDBName(): XString = {
    var pdbname = env.config.equation("PDBNAME")
    if (pdbname != null) {
      return pdbname
    }
    assert(manager != null)
    var projectfile = env.config.equation("PDBNAMEPREFIX")
    if (projectfile == null) {
      projectfile = env.config.equation("PRJ")
    }
    pdbname = manager.makePDBName(env.config.equation("outputname"), projectfile)
    env.config.setEquation2("PDBNAME", pdbname)
    pdbname
  }

  /** Opens main PDB. Returns `true`, if old PDB was reused. */
  def openMainPDB(reuse: Boolean, cleanupAdviser: ResourceCleanupAdviser): Boolean = {
    isProfileBuild = O2Env.env.enabled(BuildXKRN) || env.config.option("genprofilelibrary") || env.config.option("GenCbcStdLib")
    xfs.sys.checkRedirections()

    val pdbname = getPDBName()
    if (reuse && manager.openMainPDB(pdbname)) {
      assert(cleanupAdviser != null)
      resourceCleanupAdviser = cleanupAdviser
      true
    } else {
      manager.createMainPDB(pdbname, reset = false)
      false
    }
  }

  def resetMainPDB(): Unit = {
    if (manager.isMainPDBOpened) {
      manager.createMainPDB(getPDBName(), reset = true)
    }
  }

  def closeAll(): Unit = {
    manager.closeAll()
  }

  def closeAndCleanup(): Unit = {
    if (manager != null) {
      val mainDir = manager.mainPDBDir
      val wasOpened = mainDir != null
      val wasClosed = wasOpened && !manager.isMainPDBOpened
      manager.closeAll()

      val removeMainPDB = wasClosed && O2Env.env.enabled(CleanCompilation) && !O2Env.env.enabled(GenProfileLibrary)
      if (removeMainPDB) {
        val worker = env.config.equation("worker")
        assert(worker == null)
        /* Remove all contents of `mainDir` and `mainDir` itself. */
        Files.deleteRecursively(Path(mainDir.toString))
      }
    }
  }

  def getTempResourcesDir: xfs.FileDescriptor = {
    val tmpdirname = env.config.equation("tmpresourcedir")
    val tmpdirPlace = findPlaceToWriteTo(tmpdirname, CT.TMPRES)
    val fd = tmpdirPlace.getFileDescriptor

    if (Dirs.mkdirs(fd.getName)) fd else null
  }

  /* Utility method that copies the content of one placeholder to another.
     TODO: move to some utility class.
  */
  def copy(from: Placeholder, to0: Placeholder): Unit = {
    val fromF = from.openAsRawForRead()
    val toF = to0.openAsRawForWrite()
    xfs.copy(fromF, toF)
  }
}
