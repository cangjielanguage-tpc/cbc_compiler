/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.fe

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.o2.CharClassModule as CharClass
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xiEnvModule as xiEnv, xiFilesModule as xiFiles}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule as FS
import com.huawei.excelsior.jet.compiler.symlevel.Signature
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*

object pcNamesModule {

  abstract class NAME(val name: XString) {
    override def equals(that: Any): Boolean = that match {
      case that: AnyRef if this eq that => true
      case that: NAME => (this.getClass == that.getClass) && (this.name == that.name)
      case _ => false
    }

    override def hashCode = name.##

    def getClassloaderID: XString = null
    def getReadableName: XString = name

    def getMangledName: XString = throw new AssertionError
    def toStringID: XString = throw new AssertionError
  }

  /** Name for standard classes (loaded by system classloader). */
  class ClassName(_name: XString) extends NAME(_name) {
    override def toStringID: XString = js.format("%S%S", name, jstrClassNameMarker)
    override def getMangledName: XString = mangleJavaName(name)
  }

  /** Raw name for intermediate symbols and data. */
  class RawName(_name: XString) extends NAME(_name) {
    override def getMangledName: XString = null
  }

  /** Name for class members: fields & methods. */
  class NameAndSig(_name: XString, val sig: Signature) extends NAME(_name) {
    override def equals(that: Any): Boolean = that match {
      case that: AnyRef if this eq that => true
      case that: NameAndSig => (this.name == that.name) && (this.sig == that.sig)
      case _ => false
    }

    override def getMangledName: XString = null
  }

  /** Name for absent classes. */
  class AbsentClassName(_name: XString) extends NAME(_name) {
    override def getMangledName: XString = null
  }

  /** Name for file (project unit). */
  class FileName(_name: XString, val ext: XString) extends NAME(_name) {
    override def equals(that: Any): Boolean = that match {
      case that: AnyRef if this eq that => true
      case that: FileName => (this.name == that.name) && (this.ext == that.ext)
      case _ => false
    }

    override def toStringID: XString = js.format("%S.%S%S", name, ext, jstrFileNameMarker)

    override def getReadableName: XString = js.format("%S.%S", name, ext)
  }

  /** Name for bundle (project unit). */
  class BundleName(_name: XString) extends NAME(_name) {
    override def toStringID: XString = {
      if (xiEnv.config.tags contains xiEnv.springboot) {
        // strip Spring Boot archive container from bundle name as
        // all Spring Boot bundle names share the same container.
        // It makes string id stable on container renaming (version change f.i.)
        js.format("%S%S", name.substring(name.lastIndexOf(':') + 2), jstrBundleNameMarker)
      } else {
        js.format("%S%S", name, jstrBundleNameMarker)
      }
    }
  }

  /** Name for classes from a bundle. */
  class BundleClassName(_name: XString, val classloaderID: XString) extends NAME(_name) {
    override def equals(that: Any): Boolean = that match {
      case that: AnyRef if this eq that => true
      case that: BundleClassName => (this.name == that.name) && (this.classloaderID == that.classloaderID)
      case _ => false
    }

    override def toStringID: XString = js.format("%S%%%S%S", classloaderID, name, jstrBundleClassNameMarker)

    override def getClassloaderID: XString = classloaderID

    override def getMangledName: XString = {
      // by default mangle names with classloader ID
      if (classloaderID != null) {
        js.format(".%S/%S", classloaderID, name)
      } else {
        name
      }
    }
  }

  class LambdaClassName(_name: XString, val classloaderID: XString) extends NAME(_name) {
    override def equals(that: Any): Boolean = that match {
      case that: AnyRef if this eq that => true
      case that: LambdaClassName => (this.name == that.name) && (this.classloaderID == that.classloaderID)
      case _ => false
    }

    override def toStringID: XString = {
      if (classloaderID == null) {
        js.format("0%S%S", name, jstrLambdaClassNameMarker)
      } else {
        js.format("1%S%%%S%S", classloaderID, name, jstrLambdaClassNameMarker)
      }
    }

    override def getClassloaderID: XString = classloaderID

    // dot inside is needed to not conflict with other class names
    override def getMangledName: XString = {
      if (classloaderID != null) {
        js.format("%s/.%S/%S", LAMBDA_CLASSES_DIR, classloaderID, name)
      } else {
        js.format("%s/%S", LAMBDA_CLASSES_DIR, name)
      }
    }
  }

  class ParsedBundleID {
    var container: XString = _
    var entry: XString = _
  }

  private val null0: Int = 0
  private val classname: Int = 1
  private val absentclassname: Int = 3
  private val filename: Int = 6
  private val bundleclassname: Int = 8
  private val bundlename: Int = 9
  private val lambdaclassname: Int = 10
  private val jstrFileNameMarker = js.internJString("_0")
  private val jstrBundleNameMarker = js.internJString("_1")
  private val jstrClassNameMarker = js.internJString("_2")
  private val jstrBundleClassNameMarker = js.internJString("_3")
  private val jstrLambdaClassNameMarker = js.internJString("_4")
  private val LAMBDA_CLASSES_DIR: String = "lambda.classes"

  def writeName(name: NAME)(writeByte: Int => Unit, writeXString: XString => Unit): Unit = {
    writeByte(name match {
      case null                 => null0
      case _: ClassName         => classname
      case _: AbsentClassName   => absentclassname
      case _: FileName          => filename
      case _: BundleName        => bundlename
      case _: BundleClassName   => bundleclassname
      case _: LambdaClassName   => lambdaclassname
    })
    if (name == null) return

    writeXString(name.name)

    name match {
      case name: FileName =>
        writeXString(name.ext)

      case name: BundleClassName =>
        writeXString(name.classloaderID)

      case name: LambdaClassName =>
        if (name.classloaderID == null) {
          writeByte(0)
        } else {
          writeByte(1)
          writeXString(name.classloaderID)
        }

      case _ =>
    }
  }

  def readName(readByte: () => Int, readXString: () => XString): NAME = readByte() match {
    case `null0` => null
    case `classname`        => newClassName(readXString())
    case `absentclassname`  => newAbsentClassName(readXString())
    case `bundleclassname`  => newBundleClassName(readXString(), readXString())
    case `filename`         => newFileName(readXString(), readXString())
    case `bundlename`       => newBundleName(readXString())
    case `lambdaclassname`  => newLambdaClassName(readXString(), { if (readByte() == 0) null else readXString() })
  }

  private def mangleJavaName(name: XString): XString = {
    val buf = new js.StringBuffer()
    for (ch <- name) {
      if (CharClass.isUpper(ch)) {
        buf.appendChar('~')
      }
      buf.appendChar(ch)
    }
    buf.toJString
  }

  def demangleJavaName(name: XString): XString = {
    val sb = new js.StringBuffer()
    for (i <- 0 until name.length) {
      val ch = name.charAt(i)
      if (ch != '~' || i + 1 == name.length || !CharClass.isUpper(name.charAt(i + 1))) {
        sb.appendChar(ch)
      }
    }
    sb.toJString
  }

  def newClassName(name: XString): ClassName = new ClassName(js.intern(name))

  private def isClassNameStringID(id: XString): Boolean = id.endsWith(jstrClassNameMarker)

  private def newClassNameByStringID(id: XString): NAME = {
    assert(isClassNameStringID(id))
    val name = id.substring(0, id.length - jstrClassNameMarker.length)
    newClassName(name)
  }

  def isClassName(name: NAME): Boolean = name.isInstanceOf[ClassName]

  def newAbsentClassName(name: XString): AbsentClassName = new AbsentClassName(name)

  def isAbsent(name: NAME): Boolean = name.isInstanceOf[AbsentClassName]

  private def isFileNameStringID(id: XString): Boolean = id.endsWith(jstrFileNameMarker)

  def newFileName(name: XString, ext: XString): FileName = new FileName(FS.HOST.fromPlatform(name), js.intern(ext))

  def newFileNameByFD(f: xiFiles.FileDescriptor): NAME = {
    var name = FS.cutExt(f.getName)
    if (name.isEmpty) {
      assert(f.getName.equals(js.jstrDot))
      name = js.jstrDot // JET-5245: empty filenames leads to bugs
    }
    newFileName(name, FS.getExt(f.getName))
  }

  private def newFileNameByStringID(id: XString): NAME = {
    assert(isFileNameStringID(id))
    val name = id.substring(0, id.length - jstrFileNameMarker.length)
    val pos = name.lastIndexOf('.')
    newFileName(name.substring(0, pos), name.substring(pos + 1))
  }

  private def isBundleNameStringID(id: XString): Boolean = id.endsWith(jstrBundleNameMarker)

  def newBundleName(id: XString): BundleName = new BundleName(id)

  private def newBundleNameByStringID(id: XString): NAME = {
    var name: XString = null

    assert(isBundleNameStringID(id))
    if (xiEnv.config.tags contains xiEnv.springboot) {
      val springbootarchive = xiEnv.config.equation("springbootarchive")
      assert(springbootarchive != null)
      name = js.format("%S:/%S", springbootarchive, id.substring(0, id.length - jstrBundleNameMarker.length))
    } else {
      name = id.substring(0, id.length - jstrBundleNameMarker.length)
    }
    newBundleName(name)
  }

  def newParsedBundleID(container: XString, entry: XString): ParsedBundleID = {
    val res = new ParsedBundleID()
    res.container = container
    res.entry = entry
    res
  }

  def parseBundleID(bid: XString): ParsedBundleID = {
    assert(bid != null)
    val idx = bid.lastIndexOf(':')

    if (idx == -1) {
      newParsedBundleID(bid, null)
    } else {
      newParsedBundleID(bid.substring(0, idx), bid.substring(idx + 2))
    }
  }

  def newBundleClassName(name: XString, classloaderid: XString): BundleClassName = new BundleClassName(name, classloaderid)

  private def isBundleClassNameStringID(id: XString): Boolean = id.endsWith(jstrBundleClassNameMarker)

  private def newBundleClassNameByStringID(id: XString): NAME = {
    assert(isBundleClassNameStringID(id))
    val name = id.substring(0, id.length - jstrBundleClassNameMarker.length)
    val pos = name.indexOf('%')
    newBundleClassName(name.substring(pos + 1), name.substring(0, pos))
  }

  def isBundleClassName(name: NAME): Boolean = name.isInstanceOf[BundleClassName]

  val CLASS_LOADER_ID_SEPARATOR = '%'

  def parseMangledName(str: XString): NAME = {
    val i = str.indexOf(CLASS_LOADER_ID_SEPARATOR)
    if (i >= 0) {
      val classloaderID = js.internSubstring(str, 0, i)
      val classname = js.internSubstring(str, i + 1, -1)
      return newBundleClassName(classname, classloaderID)
    }
    newClassName(str)
  }

  def newLambdaClassName(name: XString, classloaderid: XString): LambdaClassName = new LambdaClassName(name, classloaderid)

  private def isLambdaClassNameStringID(id: XString): Boolean = id.endsWith(jstrLambdaClassNameMarker)

  private def newLambdaClassNameByStringID(id: XString): NAME = {
    assert(isLambdaClassNameStringID(id))
    val name = id.substring(0, id.length - jstrClassNameMarker.length)
    if (name.charAt(0) == '0') {
      newLambdaClassName(name.substring(1), null)
    } else {
      val pos = name.indexOf('%')
      newLambdaClassName(name.substring(pos + 1), name.substring(1, pos))
    }
  }

  def isLambdaClassName(name: NAME): Boolean = name.isInstanceOf[LambdaClassName]

  /**
    Takes mangled class name string that was produced by GetMangledName of one of
    ClassName successors (ClassName, BundleClassName or LambdaClassName)
    and produces de-mangled instance of the respective ClassName.
    Used for linking PDB resources (that usually use the mangled class name as
    resource name) with project entities.
  */
  def demangleClassName(namePar: XString): NAME = {
    var name = namePar

    var pos = name.indexOf('/')
    if (pos > 0) {
      val prefix = name.substring(0, pos)
      if (prefix.equals2(LAMBDA_CLASSES_DIR)) {
        name = name.substring(pos + 1)
        var classloaderId: XString = null
        if (name.charAt(0) == '.') {
          pos = name.indexOf('/')
          assert(pos > 0)
          classloaderId = name.substring(1, pos)
          name = name.substring(pos + 1)
        }
        return newLambdaClassName(name, classloaderId)
      } else if (prefix.charAt(0) == '.') {
        val classloaderId = prefix.substring(1)
        name = name.substring(pos + 1)
        return newBundleClassName(name, classloaderId)
      }
    }
    newClassName(demangleJavaName(name))
  }

  def fromStringID(id: XString): NAME = {
    if (isFileNameStringID(id)) {
      newFileNameByStringID(id)
    } else if (isBundleNameStringID(id)) {
      newBundleNameByStringID(id)
    } else if (isClassNameStringID(id)) {
      newClassNameByStringID(id)
    } else if (isBundleClassNameStringID(id)) {
      newBundleClassNameByStringID(id)
    } else if (isLambdaClassNameStringID(id)) {
      newLambdaClassNameByStringID(id)
    } else {
      throw new AssertionError
    }
  }
}
