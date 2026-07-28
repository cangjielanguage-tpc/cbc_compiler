/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.xmlib

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.o2lib.o2.CharClassModule as CharClass
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xiFilesModule as xfs}
import xscala.matching.Regex
import xscala.properties.OS
import xscala.util.StringOps.r

object FSModule {

  // TODO: Refactor this crap into a proper path library.
  sealed trait Filesystem {
    val separator: Char
    val pathEnvVarSeparator: Char
    val isCaseSensitive: Boolean
    val dllPrefix: String
    val exeExtension: String
    val dllExtension: String
    val batchExtension: String
    val exeLikeExtension: String

    def endsWith(path: XString, suffix: XString) = {
      if (isCaseSensitive) {
        path.endsWith(suffix)
      } else {
        path.toUpperCase.endsWith(suffix.toUpperCase)
      }
    }

    def caseToPlatform(path: XString) = {
      if (isCaseSensitive) {
        path
      } else {
        path.toUpperCase
      }
    }

    def toPlatform(path: XString): XString

    def fromPlatform(path: XString): XString

    /** Checks whether `sep` is the `PATH` environment variable separator.
      *
      * NOTE: ';' is used in xm configuration files.
      */
    def isPathSep(sep: Char) = sep == ';' || sep == pathEnvVarSeparator

    def getPrefixLength(path: XString): Int

    protected def stripSurroundingQuotes(str: XString): XString = {
      if (str.startsWith(js.jstrQuote) && str.endsWith(js.jstrQuote)) {
        str.substring(1, str.length - 1)
      } else {
        str
      }
    }
  }

  sealed trait HostFilesystem extends Filesystem {
    def fullPath(path: XString) = fromPlatform(xfs.sys.getCanonicalPath(toPlatform(path)))

    def isSameFile(a: XString, b: XString) = fullPath(a) == fullPath(b)
  }

  object Windows extends HostFilesystem {
    private val DriveLetterPrefix: Regex = "^[a-zA-Z]:$".r
    private val Substitution: Regex = """^\*\{[a-zA-Z0-9._]+}$""".r

    override val separator: Char = '\\'
    override val pathEnvVarSeparator: Char = ';'
    override val isCaseSensitive: Boolean = false
    override val dllPrefix: String = ""
    override val exeExtension: String = "exe"
    override val dllExtension: String = "dll"
    override val batchExtension: String = "bat"
    override val exeLikeExtension: String = "bat"

    private val separators = Array(separator, '/')

    override def toPlatform(path: XString) = {
      // Source: https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file
      if (!path.toString.split(separators).forall {
        case DriveLetterPrefix() => true

        case Substitution() => true

        case s =>
          s forall {
            case '<' | '>' | ':' | '"' | '/' | '\\' | '|' | '?' | '*' => false

            // ASCII non-printable characters, hex.
            case c if ('\u0000' to '\u001F') contains c => false

            case _ => true
          }
      }) {
        throw new IllegalArgumentException(s"Invalid Windows path: $path")
      }

      stripSurroundingQuotes(path.replace('/', separator))
    }

    override def fromPlatform(path: XString) = path.replace(separator, '/')

    override def getPrefixLength(path: XString): Int = {
      val len = path.length
      if (len >= 2 && CharClass.isLetter(path.charAt(0)) && path.charAt(1) == ':') {
        if (len >= 3 && isSeparator(path.charAt(2))) {
          return 3 // drive root: "X:/"
        } else {
          return 2 // drive current: "X:"
        }
      }
      if (path.startsWith(js.jstrTwoSlashes, 0)) { // UNC prefix
        return js.jstrTwoSlashes.length
      }
      if (len >= 1 && isSeparator(path.charAt(0))) {
        return 1 // Root dir: "/"
      }
      0
    }

    private def isSeparator(char: Byte) = separators contains char.toChar
  }

  object Unix extends HostFilesystem {
    override val separator: Char = '/'
    override val pathEnvVarSeparator: Char = ':'
    override val isCaseSensitive: Boolean = true
    override val dllPrefix: String = "lib"
    override val exeExtension: String = ""
    override val dllExtension: String = "so"
    override val batchExtension: String = "sh"
    override val exeLikeExtension: String = ""

    override def toPlatform(path: XString) = {
      if (path.indexOf(0.toByte) >= 0) {
        throw new IllegalArgumentException(s"Invalid Unix path: $path")
      }

      stripSurroundingQuotes(path)
    }

    override def fromPlatform(path: XString) = path

    override def getPrefixLength(path: XString): Int = {
      if (path.length >= 1 && path.charAt(0) == separator) {
        1 // Root dir: "/"
      } else {
        0
      }
    }
  }

  private class FileNameFormat {

    private[FSModule] var dirPos: Int = _ // directory position and length 
    private[FSModule] var dirLen: Int = _
    private[FSModule] var namePos: Int = _ // name position and length 
    private[FSModule] var nameLen: Int = _
    private[FSModule] var extPos: Int = _ // extension position and length 
    private[FSModule] var extLen: Int = _

  }

  // returns NIL on error 
  private def splitFileName(fname: XString): FileNameFormat = {
    if (fname.isEmpty) {
      return null
    }

    val f = new FileNameFormat()
    f.dirPos = 0

    // FIXME: Use the corresponding filesystem.
    val prefixLength = HOST.getPrefixLength(fname)
    f.namePos = prefixLength + fname.substring(prefixLength).lastIndexOf('/') + 1

    if (f.namePos == prefixLength) {
      f.dirLen = f.namePos
    } else {
      f.dirLen = f.namePos - 1 // dir w/o terminating '/' 
    }

    f.nameLen = fname.substring(f.namePos).lastIndexOf('.')
    if (f.nameLen < 0) { // no extension 
      f.nameLen = fname.length - f.namePos
      f.extPos = 0
      f.extLen = 0
    } else {
      f.extPos = f.namePos + f.nameLen + 1
      f.extLen = fname.length - f.extPos
    }

    if (f.nameLen + f.extLen > 0) {
      f
    } else {
      null
    }
  }

  def validName(fname: XString): Boolean = splitFileName(fname) != null

  def getBaseName(fname: XString): XString = {
    val f = splitFileName(fname)
    if (f == null) {
      js.jstrEmpty
    } else {
      fname.substring(f.namePos, f.namePos + f.nameLen)
    }
  }

  def getPath(fname: XString): XString = {
    val f = splitFileName(fname)
    if (f == null) {
      js.jstrEmpty
    } else {
      fname.substring(f.dirPos, f.dirPos + f.dirLen)
    }
  }

  def getExt(fname: XString): XString = {
    val f = splitFileName(fname)
    if (f == null) {
      js.jstrEmpty
    } else {
      fname.substring(f.extPos, f.extPos + f.extLen)
    }
  }

  private def tryEraseLastSegment(/*VAR*/ buf: js.StringBuffer): Boolean = {
    if (buf.length == 0) { // empty path => do nothing
      return false
    }

    val slash = buf.lastIndexOf('/')
    val segStart = slash + 1
    if (segStart + 2 == buf.length && buf.endsWith("..")) {
      false // last segment is ".." => do nothing 
    } else {
      buf.trunc(Math.max(0, slash)) // erase last path segment
      true
    }
  }

  private def normalizePathNoPrefix(path: XString): XString = {
    var segment: XString = null
    val buf = new js.StringBuffer()
    var pos = 0

    while (pos < path.length) {
      val fileSepPos = path.indexOf('/', pos)
      if (fileSepPos < 0) {
        segment = path.substring(pos)
      } else {
        segment = path.substring(pos, fileSepPos)
      }
      pos += segment.length
      if (pos < path.length) {
        pos += 1 // skip '/' 
      }

      if (segment.equals(js.jstrEmpty)) {
        // ignore "/" duplicates 
      } else if (segment.equals(js.jstrDot)) {
        // skip single "." 
      } else if (segment.equals(js.jstrTwoDots) && tryEraseLastSegment(buf)) {
        // just erased last segment before ".." 
      } else {
        if (buf.length != 0) {
          buf.append("/")
        }
        buf.appendString(segment)
      }
    }
    buf.toJString
  }

  def makeFileName(dirPar: XString, namePar: XString, extPar: XString = null): XString = {
    var dir = dirPar
    var name = namePar
    var ext = extPar

    if (dir == null) {
      dir = js.jstrEmpty
    }
    if (name == null) {
      name = js.jstrEmpty
    }
    if (ext == null) {
      ext = js.jstrEmpty
    }

    val buf = new js.StringBuffer()
    val plen = HOST.getPrefixLength(dir) // FIXME: Use the corresponding filesystem.
    buf.appendString(dir.substring(0, plen))
    val normPath = normalizePathNoPrefix(dir.substring(plen))
    buf.appendString(normPath)
    if (normPath.nonEmpty) {
      buf.append("/")
    }
    buf.appendString(name)

    if (ext.nonEmpty && ext.charAt(0) == '.') {
      ext = ext.substring(1)
    }
    if (ext.nonEmpty) {
      buf.append(".")
      buf.appendString(ext)
    }

    buf.toJString
  }

  def normalizeFileName(fnmPar: XString): XString = {
    var fnm = fnmPar

    // JET-4775 workaround: MakeFileName performs filename normalization 
    // of its first argument (eats '.' and '..') 
    fnm = makeFileName(fnm, js.jstrEmpty)
    if (fnm.length > 1) {
      assert(fnm.charAt(fnm.length - 1) == '/')
      fnm = fnm.substring(0, fnm.length - 1)
    }
    fnm
  }

  def addPath(dir: XString, name: XString): XString = makeFileName(dir, name, null)

  def addExt(name: XString, ext: XString): XString = makeFileName(null, name, ext)

  def addExt2(name: XString, ext: String): XString = addExt(name, js.newJString(ext))

  def cutPath(fname: XString): XString = {
    if (validName(fname)) {
      makeFileName(null, getBaseName(fname), getExt(fname))
    } else {
      js.jstrEmpty
    }
  }

  def cutExt(fname: XString): XString = {
    if (validName(fname)) {
      makeFileName(getPath(fname), getBaseName(fname), null)
    } else {
      js.jstrEmpty
    }
  }

  def replacePath(fname: XString, newpath: XString): XString = addPath(newpath, cutPath(fname))

  def isJar(path: XString): Boolean = {
    var ext = getExt(path)
    ext = ext.toUpperCase
    ext.equals2("JAR") || ext.equals2("WAR")
  }

  def isZip(path: XString): Boolean = {
    var ext = getExt(path)
    ext = ext.toUpperCase
    ext.equals2("ZIP") || ext.equals2("JAR") || ext.equals2("WAR")
  }

  def getFileSepForString(str: XString): Char = {
    if (str.lastIndexOf(HOST.separator) != -1) {
      return HOST.separator
    }

    if (str.lastIndexOf(Windows.separator) != -1) {
      return Windows.separator
    }

    assert(str.lastIndexOf(Unix.separator) != -1)
    Unix.separator
  }

  val HOST: HostFilesystem = OS.host match {
    case OS.WINDOWS => Windows
    case OS.LINUX => Unix
  }

  val TARGET: Filesystem = targetOS match {
    case OS.WINDOWS => Windows
    case OS.LINUX => Unix
  }
}
