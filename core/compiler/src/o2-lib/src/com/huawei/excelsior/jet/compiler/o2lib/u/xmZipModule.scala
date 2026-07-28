/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, TextFileModule as TextFile, xRamFileModule as xRamFile, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule as FS
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.JZip.{ZipEntry, ZipFileModule as ZipFile}
import com.huawei.excelsior.jet.compiler.xminizip.Minizip
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*

import scala.collection.mutable.ArrayBuffer
import scala.util.Using

object xmZipModule {

  class FileDescriptor(val zname: XString, val ename: XString) extends xfs.FileDescriptor {
    private[xmZipModule] var zf: ZipFile.ZipFile = _
    private[xmZipModule] var ze: ZipEntry = _
    private[xmZipModule] var dir: Boolean = _

    override def openRawFile(): xfs.RawFile = {
      val len = ze.getSize
      val buf = new Array[Byte](len)
      if (len != 0) {
        val zfis = zf.getInputStream(ze)
        zfis.read(buf)
      }
      xRamFile.newRamFile(getName, buf, len)
    }

    override def openSymFile(): xfs.SymFile = {
      xfs.sym.wrapForRead(openRawFile())
    }

    override def openTextFile(): xfs.TextFile = {
      TextFile.newTextFile(openRawFile(), read = true)
    }

    override def getIterator: xfs.Iterator = {
      assert(zf != null)
      assert(ename == js.jstrDot)
      new Iterator(this)
    }

    override def iterateDir(it: xfs.DirIterator): Boolean = {
      assert(zf != null)
      scanZipForDirs(this)
      val dirsforzip = zipsdirs.get(zname).asInstanceOf[Hashtable]
      assert(dirsforzip != null)
      val elements = dirsforzip.get(ename).asInstanceOf[ArrayBuffer[Element]]
      assert(elements != null)
      elements.indexWhere(e => it.entry(e.name, e.dir))
      true
    }

    override def getDir(name: XString): xfs.FileDescriptor = {
      assert(dir)
      val res = new FileDescriptor(zname, FS.makeFileName(ename, name))
      res.zf = ZipFile.newZipFile(zname)
      res.dir = true
      val enm = if (res.ename.charAt(res.ename.length - 1) == '/') res.ename else js.format("%S/", res.ename)
      res.ze = res.zf.getEntry(enm)
      res
    }

    override def getEntry(name: XString, ext: XString): xfs.FileDescriptor = {
      assert(dir)
      val res = new FileDescriptor(zname, FS.makeFileName(ename, name, ext))
      res.zf = ZipFile.newZipFile(zname)
      res.dir = false
      res.ze = res.zf.getEntry(res.ename)
      res
    }

    override def isDirectory: Boolean = dir

    override def exists: Boolean = ze != null || dir

    override def modifyTime(): Int = {
      val time = if (ze != null) ze.getTime else Int.MinValue
      Minizip.DOSTIME_BASE max time
    }

    override def getName: XString = js.format("%S:/%S", FS.HOST.toPlatform(zname), ename)
  }

  private class Element(val name: XString, val dir: Boolean)

  private class Iterator(fd: FileDescriptor) extends xfs.Iterator {
    private val entries = fd.zf.entries
    private var ze: ZipEntry = _

    override def getRelativeName: XString = ze.getName

    private def isDirectory: Boolean = {
      val s = ze.getName
      s.charAt(s.length - 1) == '/'
    }

    override def getFileDescriptor: xfs.FileDescriptor = {
      if (isDirectory) {
        fd.getDir(ze.getName)
      } else {
        fd.getEntry(ze.getName, js.jstrEmpty)
      }
    }

    override def next(): Boolean = {
      if (!entries.hasNext) {
        return false
      }
      ze = entries.next()
      true
    }
  }

  private var zipsdirs: Hashtable = _

  private def checkEntryAsDir(entry: XString): Boolean = {
    val ext = FS.getExt(entry)
    ext.isEmpty
  }

  private def enameIsFdPlusSlash(ename: XString, fd: FileDescriptor, ignoreCase: Boolean = false): Boolean = {
    val startsWith = if (ignoreCase) ename.startsWithIgnoreCase(fd.ename, 0) else ename.startsWith(fd.ename, 0)
    startsWith && ename.length > fd.ename.length && ename.charAt(fd.ename.length) == '/'
  }

  def createFileDescriptor(zipnm: XString, entry: XString): xfs.FileDescriptor = {
    val fd = new FileDescriptor(zipnm, entry)
    fd.dir = false
    fd.zf = ZipFile.newZipFile(zipnm)
    if (fd.zf != null) {
      fd.ze = fd.zf.getEntry(entry)
      if (entry.equals(js.jstrDot)) {
        fd.dir = true
      } else if (fd.ze == null) {
        if (checkEntryAsDir(entry)) {
          val eWithTailSlash = js.format("%S/", entry)
          fd.ze = fd.zf.getEntry(eWithTailSlash)
          if (fd.ze != null) {
            fd.dir = true
          } else {
            if (fd.zf.entryNames.exists(enm => enameIsFdPlusSlash(enm, fd))) {
              fd.dir = true
            }
          }
        }
      } else {
        fd.dir = fd.ze.isDirectory
      }
      if (fd.ze == null) {
        /* fill msg */
      }
    }
    fd
  }

  def createFileDescriptorNoCase(zipnm: XString, entry: XString): xfs.FileDescriptor = {
    val fd = new FileDescriptor(zipnm, entry)
    fd.dir = false
    fd.zf = ZipFile.newZipFile(zipnm)
    if (fd.zf != null) {
      fd.ze = fd.zf.getEntry(entry)
      if (fd.ze == null) {
        if (fd.zf.entryNames.exists(enm => enameIsFdPlusSlash(enm, fd, ignoreCase = true))) {
          fd.dir = true
        }
      }
      if (fd.ze == null) {
        /* fill msg */
      }
    }
    fd
  }

  private def addEntry(dirsforzip: Hashtable, root: ArrayBuffer[Element], enm: XString, isDir: Boolean): Unit = {
    var name: XString = null
    var v: ArrayBuffer[Element] = null

    val pos = enm.lastIndexOf('/')
    if (pos != -1 && pos != 0) {
      name = enm.substring(pos + 1)
      val dir = enm.substring(0, pos)
      v = dirsforzip.get(dir).asInstanceOf[ArrayBuffer[Element]]
      if (v == null) {
        addEntry(dirsforzip, root, dir, isDir = true)
        v = new ArrayBuffer[Element]
        assert(dirsforzip.put(dir, v) == null)
      }
    } else {
      name = enm
      v = root
    }
    if (name.nonEmpty) {
      v += new Element(name, isDir)
    }
  }

  private def scanZipForDirs(fd: FileDescriptor): Unit = {
    if (zipsdirs == null) {
      zipsdirs = new Hashtable()
    } else if (zipsdirs.get(fd.zname) != null) {
      return
    }
    val dirsforzip = new Hashtable()
    assert(zipsdirs.put(fd.zname, dirsforzip) == null)

    val root = new ArrayBuffer[Element]
    assert(dirsforzip.put(js.jstrDot, root) == null)

    val entries = fd.zf.entries
    while (entries.hasNext) {
      val ze = entries.next()
      val enm = ze.getName
      addEntry(dirsforzip, root, enm, isDir = false)
    }
  }

  def cleanDirs(): Unit = {
    zipsdirs = null
  }
}
