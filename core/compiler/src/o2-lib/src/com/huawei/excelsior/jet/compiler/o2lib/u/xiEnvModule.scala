/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.common.JetDirs
import com.huawei.excelsior.common.Language.CANGJIE
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.languagePack
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pcNamesModule as pcNames, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, WritablePathsModule as WritablePaths}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.TimeModule as Time
import com.huawei.excelsior.jet.compiler.options.Option.SmartKind
import com.huawei.excelsior.jet.compiler.options.Option.SmartKind.*
import com.huawei.excelsior.jet.compiler.symlevel.{JBCSignature, MethodSignature}
import com.huawei.excelsior.jet.compiler.xpackii.ArchiveUtils
import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.io.{Path, stdout}
import xscala.util.{Set32, UByte, UInt}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Abstract interface level */
object xiEnvModule { /* Ned 19-Feb-94. */
  type STAGE = UByte
  val CHECKING: STAGE = UByte(0)
  val FRONT: STAGE = UByte(1)
  val BACK: STAGE = UByte(2)

  type CompilerOption = UByte
  val superimportonly: CompilerOption = UByte(1)//* =  1;
  val springboot: CompilerOption = UByte(3)//* =  3;
  val nil_check: CompilerOption = UByte(6)//* =  6;
  val index_check: CompilerOption = UByte(10)//* = 10;
  val arrstore_check: CompilerOption = UByte(18)
     /* bits 16-19 are reserved for interface with back-end */
  val regularbuild: CompilerOption = UByte(29)

  /** compiler options */
  val CompilerOptionSet = Set32
  type CompilerOptionSet = Set32


  abstract class OptionsHolder extends Object {

    def options: Iterator[AbstractOption]

    def setOption(name: String, value: Boolean): Unit = {
      this.setOptionJS(js.newJString(name), value)
    }

    def setOptionJS(name: XString, value: Boolean): Unit = {
    }

    def setEquation2(name: String, value: XString): Unit = {
      this.setEquationJS(js.newJString(name), value)
    }

    def setEquation(name: String, value: String): Unit = {
      this.setEquationJS(js.newJString(name), js.newJString(value))
    }

    /*------------------------  OptionsHolder  ---------------------------*/
    def setEquationJS(name: XString, value: XString): Unit = {
    }

  }


  type Config = xmConfigModule.Config
  type Context = xmConfigModule.Context

  class Info {
    var filename: XString = _  /** file name */
    var module: pcNames.NAME = _  /** module name */
    var lines: Int = _ /** number of lines */
    var newSF: Boolean = _ /** new symbol file */
    var worker: Int = _

    def reset(): Unit = {
      this.filename = null
      this.module = null
      this.lines = 0
      this.newSF = false
    }

    final def forcePrint(format: String, x: Any*): Unit =
      stdout.print(JStringsModule.format(format, x: _*).toString)

    final def print(format: String, x: Any*): Unit =
      mute { forcePrint(format, x: _*) }

    def report(): Unit = {}

    /** Prints compiler header */
    def header(): Unit = {}
  }

  def exit(no: Int = 3): Nothing = {
    val e = new Error()
    stdout.printStackTrace(e)
    sys.exit(no)
  }

  abstract class Errors {
    var errDetected: Boolean = _
    var envErrDetected: Boolean = _
    var lastError: XString = _

    def reset(): Unit = {
      errDetected = false
      envErrDetected = false
    }

    def showErrors(): Unit

    def execute(action: => Unit): Unit

    def fault(err: ErrMsg, x: Any*): Nothing = {
      printMsg('f', getMsg(err), err.no, x: _*)
      shouldNotReachHere()
    }

    def envError(err: ErrMsg, x: Any*): Unit = {
      printMsg('v', getMsg(err), err.no, x: _*)
      envErrDetected = true
      errDetected = true
    }

    def message(err: ErrMsg, x: Any*): Unit = {
      printMsg('m', getMsg(err), err.no, x: _*)
    }

    def silentMessage(err: ErrMsg, x: Any*): Unit = {
      mute { message(err, x: _*) }
    }

    def getMsg(err: ErrMsg): XString

    /** type:
          'f' - fatal error
          'v' - environment error
          'm' - message
     */
    def printMsg(type0: Char, fmt: XString, msgno: Int, x: Any*): Unit
  }


  class Args {
    def parse(): Unit = {}

    /** returns internal representation of program file name */
    def programName: XString = null

    def deleteArg(i: Int): Unit = {}

    def getArg(i: Int): XString = null

    def number(): Int = 0
  }

  /*------------------- Options ---------------------------------*/

  class Value


  sealed abstract class AbstractOption(_name: XString, val checked: SmartKind) extends Object {

    val name = _name.toUpperCase

    private[xiEnvModule] var value: Value = _

    def setValue(value: Value): Unit = {
      this.value = value
      if (value != null) {
        // initial NIL values must be permitted
        this.verify()
      }
    }

    def getValue: Value = this.value

    /** This method is intended to be overriden,
     *  if it is needed to check its value defined by user.
     *  Now it is called imediatly after setting a value (see setValue),
     *  bit in future, it may be called someway after (just before compilation).
     *  The method is natural replacementy of old style
     *  Active options & equations.
    */
    protected def verify(): Unit = {
    }

  }


  private class StringValue extends Value {

    private[xiEnvModule] var value: XString = _

  }


  class Equation(name: XString, checked: SmartKind = Checked)
    extends AbstractOption(name, checked) {

    def getStringValue: XString = {
      val value = this.getValue
      if (value == null) {
        null
      } else {
        value.asInstanceOf[StringValue].value
      }
    }

    /*
      Always creates new Value object
    */
    def setNewStringValue(value: XString): Unit = {
      val v = new StringValue()
      if (value != null) {
        v.value = this.preprocessValue(value)
        this.setValue(v)
      } else {
        // TODO: deny NILs later
        v.value = null
        this.value = v
      }
    }

    /** Sometimes values of an equation should be converted to some form before set.
     *  Override this method in this cases.
     */
    def preprocessValue(value: XString): XString = value

  }


  private class BooleanValue extends Value {

    private[xiEnvModule] var value: Boolean = _

  }


  class Option(name: XString, checked: SmartKind = Checked)
    extends AbstractOption(name, checked) {

    def getBooleanValue: Boolean = {
      val value = this.getValue
      if (value == null) {
        false
      } else {
        value.asInstanceOf[BooleanValue].value
      }
    }

    /*
      Updates Value object's value or creates new if it was not created
      TODO: do we really need to update values? Equations do not do this.
    */
    def setBooleanValue(value: Boolean): Unit = {
      val v = this.getValue.asInstanceOf[BooleanValue]
      if (v == null) {
        this.setNewBooleanValue(value)
      } else {
        v.value = value
        this.setValue(v) // to call performAction
      }
    }

    /*
      Always creates new Value object
    */
    def setNewBooleanValue(value: Boolean): Unit = {
      val v = new BooleanValue()
      v.value = value
      this.setValue(v)
    }

  }

  /*----------------------------------------------------------------*/
  type OptionSpecifiedType = UByte
  val NO: OptionSpecifiedType = UByte(0) // Synchronized with compiler.CompileOptions.OptionResult
  val YES: OptionSpecifiedType = UByte(1)
  val UNSPECIFIED: OptionSpecifiedType = UByte(2)

  //----------------------------------------------------------------------------
  type PackMode = UByte
  val PM_NONCOMPILED: PackMode = UByte(0)            // pack all resources and non-compiled classes
  val PM_ALL: PackMode = UByte(1)                    // pack entry as whole, without any changes
  val PM_NONE: PackMode = UByte(2)                   // do not pack entry, leave it standalone
  val PM_NONE_AND_OMIT_CLASSES: PackMode = UByte(3)  // same as PM_NONE, but generate stumps/VCF for compiled classses and
                                          // make directives for JetPackII to remove corresponding class files.
                                          // Used for directories only.
  val PM_AS_DIR_NONCOMPILED: PackMode = UByte(4)     // same as PM_NONCOMPILED, but pack jar files as directory
  val PM_RESOURCES: PackMode = UByte(5)             // pack resources only, and do not include classes into compilation set

  /* modifications:
    18-Oct-95 Ned: shell interface is added.
  */
  /* config results */
  val isEquation: Int = -2 /** for Parse only */
  val isOption: Int = -1 /** for Parse only */
  val ok: Int = 0 /** after Parse means: empty line or comment */
  val unknownOption: Int = 1
  val unknownEquation: Int = 2
  val definedOption: Int = 3
  val definedEquation: Int = 4
  val wrongSyntax: Int = 5
  val defineOptionWhenEquationDefined: Int = 8
  val defineEquationWhenOptionDefined: Int = 9
  var stage: STAGE = CHECKING
  var config: Config = new Config
  var context: Context = null
  var info: Info = _
  var errors: Errors = _
  var loadType: XString => pcO.Class = null
  var args: Args = _
  var errDetected: Boolean = false
  /** flags in Info.decor */
  val dc_header: Int = 0  /** utility header */
  val dc_tailer: Int = 1  /** total results */
  val dc_compiler: Int = 2  /** compiler header */
  val dc_report: Int = 3  /** compiler report */
  val dc_progress: Int = 4  /** compiler progress report */
  val dc_warnings: Int = 5  /** compiler warnings */
  val dc_silent: Int = 6 /** silent compilation */
  var decor: Set32 = Set32.empty
  private var lastStrLen: Int = 20

  def setArgs(a: Args): Unit = {
    args = a
  }

  def newEquation(name: XString, checked: SmartKind = Checked): Equation = {
    new Equation(name, checked)
  }

  def newOption(name: XString, checked: SmartKind = Checked): Option = {
    new Option(name, checked)
  }

  def silentCompilation: Boolean = decor contains dc_silent
  def mute(f: => Unit): Unit = if (!silentCompilation) { f }

  def isProgressShowable: Boolean = !silentCompilation && (decor contains dc_progress)

  private def checkedOptionRes(retValue: Boolean): OptionSpecifiedType = {
    if (config.res == unknownOption) {
      UNSPECIFIED
    } else if (retValue) {
      YES
    } else {
      NO
    }
  }

  def optionSpecified(s: String): OptionSpecifiedType = {
    val b = config.option(s, initWithDefault = false)
    checkedOptionRes(b)
  }

  def equationList(s: String): Unit = {
    val list = js.newJString(s)
    var i = 0
    while (i < list.length) {
      var pos = i
      while (i < list.length && list.charAt(i) != ';' && list.charAt(i) != '=') {
        i += 1
      }
      val name = list.substring(pos, i)
      config.newEquationJS(name)

      if (i < list.length && list.charAt(i) == '=') {
        i += 1
        pos = i
        while (i < list.length && list.charAt(i) != ';') {
          i += 1
        }
        val value = list.substring(pos, i)
        config.setEquationJS(name, value)
      }
      i += 1
    }
  }

  /** Splits the given string by the provided delimiter. */
  def splitString(values: XString, delimiter: Char): ArrayBuffer[XString] = {
    val result = ArrayBuffer.empty[XString]
    val len = values.length
    if (len == 0) {
      return result
    }

    var start = 0
    var pos = 0
    while (pos < len) {
      if (values.charAt(pos) == delimiter) {
        result += values.substring(start, pos)
        start = pos + 1
      }
      pos += 1
    }
    result += values.substring(start, len) // append the last value
  }

  /** Converts string containing list of values separated by ';' to a set. */
  def convValueToSet(values: XString): mutable.HashSet[XString] = {
    mutable.HashSet.empty[XString] ++= splitString(values, ';')
  }

  def clear(): Unit = {
    if (errors != null) {
      errors.reset()
    }
    if (info != null) {
      info.reset()
    }
  }

  /**
    Time in 100th of a second modulo by amount of 100th seconds in a day.
    Should be used only for metering the number of 100th seconds between
    two points: start and end.
    We assume that such computation will not be longer than a day.

    See also: "diffTimes".

    TODO: Do we need such accuracy (100th of a second)?
  */
  def time(): UInt = Time.getTimeMillisFromMidnight / UInt(10)

  def diffTimes(start: UInt, end: UInt): UInt = {
    if (end >= start) {
      end - start
    } else { // DAY overflow
      // Time.curTimeMillis returns time in ms from the beginning of the day (0:00:00,000)
      // so if we started computation before midnight and ended it after midnight
      // end will be less than start
      ((24 * 60 * 60 * 100).toUInt - start) + end
    }
  }

  def getRTCacheFileName: XString = js.newJString("cachedobj")

  def getProfileDir: XString = js.format("%S/profile", config.equation("jet_home"))

  def getJetLibDir: XString = js.format("%S/lib", config.equation("jet_home"))

  def getProfileJREDir: XString = js.format("%S/jre", getProfileDir)

  // gets JET/profileX/develop path
  def getDevelopDir: XString = js.format("%S/develop", getProfileDir)

  /**
     Returns a path where we can store profile-specific global information
     such as global compilation cache.
  */
  def getProfileWritablePath: XString = {
    val path = config.equation("profileWritablePath")
    if (path != null) {
      // suggest to use equation above in support
      // in case of problems with auto configuration
      return path
    }
    WritablePaths.getProfileWritablePath(config.equation("profile_name"))
  }

  def findLibraryPath(libraryName: String): Path = {
    val libPath = Path(libraryName)
    if (libPath.exists) {
      if (ArchiveUtils.isZipArchive(libraryName)) {
        Path(ArchiveUtils.unzipOnce(libraryName))
      } else {
        libPath
      }
    } else if (libPath.isRelative && libPath.parent == Path.dot) {
      val profileLibPath = JetDirs.jetHome / "profile/develop" / libraryName
      if (!profileLibPath.exists) {
        null
      } else {
        profileLibPath
      }
    } else {
      null
    }
  }

  def getProfileLibraryPath(libraryName: String): XString = {
    js.format("%S/%s", getDevelopDir, libraryName)
  }

  def printWithErasingPrevious(str: XString): Unit = {
    val str_len = str.length

    var k = lastStrLen - str_len
    if (k < 0) {
      k = 0
    }
    info.forcePrint("\\r%S%.*c", str, k, UInt(32).toInt) // fixme

    lastStrLen = str_len
  }

  /*
     Returns the current pack mode.
  */
  def getPackMode: PackMode = {
    var mode = config.equation("pack")
    if (mode == null) {
      return PM_NONCOMPILED
    }

    mode = mode.toUpperCase

    if (mode.equals2("NONCOMPILED")) { // most common case
      PM_NONCOMPILED
    } else if (mode.equals2("ALL")) {
      PM_ALL
    } else if (mode.equals2("NONE")) {
      PM_NONE
    } else if (mode.equals2("NONEANDOMITCLASSES")) {
      PM_NONE_AND_OMIT_CLASSES
    } else if (mode.equals2("ASDIRNONCOMPILED")) {
      PM_AS_DIR_NONCOMPILED
    } else if (mode.equals2("RESOURCES")) {
      PM_RESOURCES
    } else {
      throw new AssertionError
    }
  }

  /*
    Returns TRUE if the current classpath entry is packed into executable.
  */
  def isPackedIntoExe: Boolean = {
    val packMode = getPackMode
    packMode != PM_NONE && packMode != PM_NONE_AND_OMIT_CLASSES
  }

  /*
    Returns TRUE if VCF should be generated for the compiled classes in the
    current classpath entry.
  */
  def shouldGenerateVCF(): Boolean = {
    if (config.option("novcf")) {
      return false
    }

    val packMode = getPackMode
    packMode == PM_NONCOMPILED || packMode == PM_AS_DIR_NONCOMPILED || packMode == PM_NONE_AND_OMIT_CLASSES
  }

  def isMainMethodSig(sig: MethodSignature, declaringClass: pcO.Class) = {
    // TODO: replace with pattern matching
    if (declaringClass.isCangjieType) {
      declaringClass.isCangjiePackage
    } else {
      val mainSig = if (declaringClass.isAJManagedType) {
        // Cangjie launcher uses argc/argv main, not the normal LWRT one.
        // TODO: switch cangjie launcher to LWRT sig
        if (languagePack.supports(CANGJIE) ) js.jstrCangjieMainSig else js.jstrLWRTMainSig
      } else if (declaringClass.isXScalaType) {
        js.jstrXScalaMainSig
      } else {
        js.jstrMainSig
      }
      JBCSignature(sig) == mainSig.toString
    }
  }
}
