/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.o2lib.o2.CharClassModule as CharClass
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xPDBModule as xPDB
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xiEnvModule as env, xiFilesModule as xfs, xmZipModule as zip}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.{FSModule as FS, PortableRegCompModule as RegComp}
import com.huawei.excelsior.jet.compiler.options.BoolOption.BuildXKRN
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** FileSys managers */
object xmFSModule { /* Ned 24-Feb-94. */

  private class Dir(val path: XString, var next: Dir = null)

  class Node {
    private[xmFSModule] var expr: RegComp.Expr = _
    private[xmFSModule] var patt: XString = _
    private[xmFSModule] var dirs: Dir = _
    private[xmFSModule] var level: Int = _
    private[xmFSModule] var next: Node = _
  }

  abstract class FileSys extends xfs.FileSys {
    private[xmFSModule] var nodes: Node = _
    private[xmFSModule] var level: Int = _

    def init(): Unit = {
      this.nodes = null
      this.level = 0
    }

    override def compareExtSys(ext1: XString, ext2: XString): Boolean = {
      val e1 = FS.HOST.caseToPlatform(ext1)
      val e2 = FS.HOST.caseToPlatform(ext2)
      e1.equals(e2)
    }

    override def createFileDescriptor(fname: XString): xfs.FileDescriptor = new FileDescriptor(fname)

    override def restoreRed(): Unit = {
      var l = this.nodes
      while (l != null && l.level == this.level) {
        l = l.next
      }
      this.nodes = l
      this.level -= 1
    }

    override def restoreRedAtLevel(level: Int): Unit = {
      assert(level <= this.level)
      var l = this.nodes
      var prev: Node = null
      while (l != null) {
        if (l.level == level) {
          if (prev == null) {
            this.nodes = l.next
          } else {
            prev.next = l.next
            prev = l
          }
        } else {
          prev = l
        }
        l = l.next
      }
    }

    override def saveRed(): Unit = {
      this.level += 1
    }

    override def parseRed(s: XString): Int = this.parseRedAtLevel(s, this.level)

    override def parseRedAtLevel(s: XString, level: Int): Int = {
      assert(level <= this.level)
      var i = 0
      val len = s.length
      while (i < len && CharClass.isWhiteSpace(s.charAt(i))) {
        i += 1
      }
      val p = i
      while (i < len && s.charAt(i) != '=' && !CharClass.isWhiteSpace(s.charAt(i))) {
        i += 1
      }
      val pattern = s.substring(p, i)
      while (i < len && s.charAt(i) != '=') {
        i += 1
      }
      if (i == len) {
        i
      } else {
        i += 1
        while (i < len && CharClass.isWhiteSpace(s.charAt(i))) {
          i += 1
        }
        set(this, pattern, s, i, level)
      }
    }

    /* Checks that no *.sym,*.irb etc lookups are specified */
    override def checkRedirections(): Unit = {
      var n = this.nodes
      while (n != null) {
        val ct = xPDB.getTypeByExt(n.patt)
        if (ct != xPDB.ContentType.UNSUPPORTED && (xPDB.ignoredLookups contains ct) && !((xPDB.respectedLookups contains ct) && O2Env.env.enabled(BuildXKRN))) {
          // & (n.level # xfs.RED_LEVEL_REDFILE) 
          env.errors.fault(ErrMsg487, n.patt)
        }
        n = n.next
      }
    }

    override def useFirstDir(nm: XString): XString = {
      var l = this.nodes
      val sNm = FS.HOST.caseToPlatform(nm)
      while (l != null) {
        if (RegComp.Match(l.expr, sNm, 0)) {
          if (l.dirs == null) {
            return js.jstrDot
          } else if (!FS.isZip(l.dirs.path)) {
            return FS.HOST.toPlatform(l.dirs.path)
          }
        }
        l = l.next
      }

      js.jstrDot
    }

    override def useFirst(nm: XString): XString = {
      var l = this.nodes
      val exeout = checkForExe(nm)
      if (exeout != null) {
        return exeout
      }
      val sNm = FS.HOST.caseToPlatform(nm)
      while (l != null) {
        if (RegComp.Match(l.expr, sNm, 0)) {
          if (l.dirs == null) {
            return nm
          } else if (!FS.isZip(l.dirs.path)) {
            return FS.addPath(l.dirs.path, nm)
          }
        }
        l = l.next
      }

      nm
    }

    override def sysLookup(ext: String): XString = {
      val u = env.args.programName
      val dir = FS.getPath(u)
      val jc = FS.addExt2(FS.getBaseName(u), ext)
      var fname = jc
      var fd = this.look(fname)
      if (fd != null) {
        return fd.getName
      }
      if (xfs.sys.exists(fname)) {
        return fname
      }
      if (dir.nonEmpty) {
        fname = FS.addPath(dir, jc)
        if (xfs.sys.exists(fname)) {
          return fname
        }
      }
      val xm = FS.addExt2(js.newJString("xm"), ext)
      fname = xm
      fd = this.look(fname)
      if (fd != null) {
        return fd.getName
      }
      if (xfs.sys.exists(fname)) {
        return fname
      }
      if (dir.nonEmpty) {
        fname = FS.addPath(dir, xm)
        if (xfs.sys.exists(fname)) {
          return fname
        }
      }
      jc
    }

    override def lookupDir(name: XString, pattern: XString): xfs.FileDescriptor = {
      var fd: xfs.FileDescriptor = null

      var l = this.nodes
      val sNm = FS.HOST.caseToPlatform(pattern)
      while (l != null) {
        if (RegComp.Match(l.expr, sNm, 0)) {
          val t = lookupFile(l.dirs, name)
          if (t != null) {
            if (fd == null) {
              fd = t
            } else {
              var s = fd.next
              var p = fd
              var b = false
              while (s != null) {
                if (s eq t) {
                  s = null
                  b = true
                } else {
                  p = s
                  s = s.next
                }
              }
              if (!b) {
                p.next = t
              }
            }
          }
        }
        l = l.next
      }
      if (fd != null) {
        return fd
      }
      fd = xfs.sys.createFileDescriptor(name)
      fd
    }

    override def lookup(name: XString, lookInCurrentDir: Boolean = true): xfs.FileDescriptor = {
      val fd = this.look(name)
      if (fd != null) {
        return fd
      }
      if (lookInCurrentDir) {
        xfs.sys.createFileDescriptor(name)
      } else {
        null
      }
    }

    override def existLookups(pat: XString): Boolean = {
      var l = this.nodes
      val sNm = FS.HOST.caseToPlatform(pat)
      while (l != null) {
        if (RegComp.Match(l.expr, sNm, 0)) {
          return true
        }
        l = l.next
      }
      false
    }

    def look(nm: XString): xfs.FileDescriptor = {
      var l = this.nodes
      val sNm = FS.HOST.caseToPlatform(nm)
      while (l != null) {
        if (RegComp.Match(l.expr, sNm, 0)) {
          val fd = lookupFile(l.dirs, nm)
          if (fd != null) {
            return fd
          }
        }
        l = l.next
      }
      null
    }
  }

  private class FileDescriptor(val name: XString) extends xfs.FileDescriptor {
    override def getIterator: xfs.Iterator = new Iterator(name)

    override def openRawFile(): xfs.RawFile = {
      val file = xfs.raw.openToRead(name)
      if (file == null) {
        env.errors.fault(xfs.MSG_FILE_OPEN_ERROR, xfs.raw.errmsg)
      }
      file
    }

    override def openSymFile(): xfs.SymFile = {
      val file = xfs.sym.openToRead(name)
      if (file == null) {
        env.errors.fault(xfs.MSG_FILE_OPEN_ERROR, xfs.sym.errmsg)
      }
      file
    }

    override def openTextFile(): xfs.TextFile = {
      val file = xfs.text.openToRead(name)
      if (file == null) {
        env.errors.fault(xfs.MSG_FILE_OPEN_ERROR, xfs.text.errmsg)
      }
      file
    }

    override def iterateDir(i: xfs.DirIterator): Boolean = xfs.sys.iterateDir(name, i)

    override def getDir(dname: XString): xfs.FileDescriptor = getEntry(dname, js.jstrEmpty)

    override def getEntry(ename: XString, ext: XString): xfs.FileDescriptor =
      new FileDescriptor(FS.makeFileName(name, ename, ext))

    override def isDirectory: Boolean = {
      if (!exists) {
        return false
      }

      if (name == js.jstrDot || name == js.jstrTwoDots) {
        return true
      }

      class DirIter(val name: XString) extends xfs.DirIterator {
        var exists: Boolean = _
        var isDir: Boolean = _
        override def entry(name: XString, dir: Boolean): Boolean = {
          exists = this.name == name
          isDir = dir
          exists
        }
      }

      val d = new DirIter(FS.cutPath(name))
      if (d.name.isEmpty) { // ends with slash or dot.
        return true
      }

      var parentDir = FS.getPath(name)
      if (parentDir.isEmpty) {
        parentDir = js.jstrDot
      }


      if (xfs.sys.iterateDir(parentDir, d)) {
        d.exists && d.isDir
      } else {
        false
      }
    }

    override def exists: Boolean = xfs.sys.exists(name)

    override def modifyTime(): Int = xfs.sys.modifyTime(name)

    override def getName: XString = name
  }

  private class FileDescriptorKnownType(_name: XString, isDir: Boolean) extends FileDescriptor(_name) {
    override def isDirectory: Boolean = isDir
  }

  private class FlatIterator(path: XString) {
    private val list = xfs.sys.listFiles(path)
    private var index = -1
    private var needGoInDepth = false

    def getFileDescriptor: FileDescriptor = new FileDescriptorKnownType(getFullname, isDirectory)

    def getFullname: XString = FS.makeFileName(path, list(index).name)

    def checkNeedGoInDepthAndSetFalseIfNeed(): Boolean = {
      if (needGoInDepth) {
        needGoInDepth = false
        true
      } else {
        false
      }
    }

    def next(): Boolean = {
      if (index >= list.size) {
        return false
      }
      index += 1
      val isNotEnd = index < list.size
      if (isNotEnd) {
        needGoInDepth = isDirectory
      }
      isNotEnd
    }

    def isDirectory: Boolean = list(index).isDir
  }


  private class Iterator(path: XString) extends xfs.Iterator {
    private val flatIterators = new mutable.Stack[FlatIterator]
    flatIterators.push(new FlatIterator(path))

    override def getRelativeName: XString = {
      val fullName = flatIterators.top.getFullname
      fullName.substring(path.length + 1)
    }

    override def getFileDescriptor: xfs.FileDescriptor = {
      flatIterators.top.getFileDescriptor
    }

    override def next(): Boolean = {
      if (flatIterators.isEmpty) {
        return false
      }

      val flatIterator = flatIterators.top

      if (flatIterator.checkNeedGoInDepthAndSetFalseIfNeed()) {
        // if last iterated element was directory, continue iteration into depth 
        flatIterators.push(new FlatIterator(flatIterator.getFullname))
        return next()
      }

      if (flatIterator.next()) {
        return true
      }

      flatIterators.pop()
      next()
    }
  }

  private def createFD(dir: XString, name: XString): xfs.FileDescriptor = {
    if (FS.isZip(dir)) {
      zip.createFileDescriptor(dir, name)
    } else {
      new FileDescriptor(FS.addPath(dir, name))
    }
  }

  private def lookupFile(dirPar: Dir, name: XString): xfs.FileDescriptor = {
    var dir = dirPar

    var tail: xfs.FileDescriptor = null
    var fd: xfs.FileDescriptor = null
    while (dir != null) {
      val f = createFD(dir.path, name)
      if (f.exists) {
        if (tail != null) {
          tail.next = f
          tail = f
        } else {
          fd = f
          tail = f
        }
      }
      dir = dir.next
    }
    fd
  }

  private def parsePath(path: XString, iPar: Int, mess461allowed: Boolean): Dir = {
    /* start position in path */
    /* rewritten in asumption that path_sep may not include space */
    var i = iPar
    var str: XString = null

    val len = path.length
    var last: Dir = null
    var dirs: Dir = null
    loop {
      while (i < len && (FS.HOST.isPathSep(path.charAtAsChar(i)) || path.charAt(i) == ' ')) {
        i += 1
      }

      if (i == len) {
        break()
      }

      val q = path.charAt(i)
      if (q == '\"' || q == '\'') {
        var e = path.indexOf(q, i + 1)
        if (e == -1) {
          return null
        }
        str = path.substring(i + 1, e)
        i = e + 1
      } else {
        val b = i
        while (i < len && !FS.HOST.isPathSep(path.charAtAsChar(i))) {
          i += 1
        }
        var e = i
        if (!FS.HOST.isPathSep(' ')) {
          e -= 1
          while (e >= b && path.charAt(e) == ' ') {
            e -= 1
          }
          e += 1
        }
        str = path.substring(b, e)
      }
      if (str.isEmpty) {
        break()
      }

      val d = new Dir(FS.HOST.fromPlatform(str))
      if (mess461allowed && !env.config.option("noinvlookups") && !xfs.sys.exists(d.path)) {
        env.errors.silentMessage(ErrMsg461, d.path, path)
      }
      if (last == null) {
        dirs = d
      } else {
        last.next = d
      }
      last = d
    }
    dirs
  }

  private def checkForExe(s: XString): XString = {
    // There is no way to set lookup for executable for unixes, 
    // since they have no extension. So the only way to insert this patch: 
    // if name is outputname then it is executable name that should be put 
    // according outputdir equation 
    if (targetOS.isLinux) {
      val outputname = env.config.equation("outputname")
      if (outputname != null && outputname.equals(s)) {
        val outputdir = env.config.equation("outputdir")
        if (outputdir != null && outputdir.nonEmpty) {
          return FS.addPath(outputdir, s)
        }
      }
    }
    null
  }

  /** Returns non-negative index of position in `path`, at which error might have occurred. */
  private def set(fs: FileSys, pattern: XString, path: XString, i: Int, level: Int): Int = {
    val sPatt = FS.HOST.caseToPlatform(pattern)
    val RegComp.CompileRes(e, res) = RegComp.compile(sPatt)
    if (res <= 0) {
      return math.abs(res) // error
    }

    val n = new Node()
    n.expr = e
    n.level = level
    n.patt = sPatt
    n.dirs = parsePath(path, i, mess461allowed = true)
    if (n.dirs == null) {
      return path.length // error
    }

    var l = fs.nodes
    var p: Node = null
    while (l != null && l.level >= level) {
      p = l
      l = l.next
    }
    if (p == null) {
      fs.nodes = n
    } else {
      p.next = n
    }
    n.next = l
    -1 // ok
  }

}
