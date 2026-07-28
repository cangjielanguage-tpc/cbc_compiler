/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

import com.huawei.excelsior.jet.assembler.Segment
import com.huawei.excelsior.jet.common.{BuiltInField, XString}
import com.huawei.excelsior.jet.compiler.abi.{Frame, XTableGenerator}
import com.huawei.excelsior.jet.compiler.bytecode.{NoPosition, Position}
import com.huawei.excelsior.jet.compiler.ir.{MarkedRegion, XInfo}
import com.huawei.excelsior.jet.compiler.options.BoolOption.PrintDeltaMaps
import com.huawei.excelsior.jet.compiler.options.{BoolOption, NumOption, StrOption, Option as CompilerOption}
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.verifier.VerifiableMethod
import xscala.io.{ByteBuffer, Path, TextOutput}
import xscala.properties.OS
import xscala.sync.Sync.{Lock, newLock}

import scala.collection.mutable

/** Abstract legacy environment
  *
  * @author cypok
  * @author conwor
  * @author paul
  */
abstract class Environment {

  def initEnv(): Unit = {}
  protected def rtConstResolver: RTConst.Resolver

  initEnv()
  RTConst.init(rtConstResolver)

  //////////////////////////////////////////////////////////////////////////////

  def getPass: Pass

  def stage[A](stage: Stage)(action: => A): A

  /** Send result method code to compiler for adding it into obj file.
    *
    * @param codeUnit         generated code unit
    * @param seg              result code & fixups
    * @param xinfo            exception sites in the method
    * @param xTable           exception table generated in java side
    * @param trivialXHandler  Exception handler is trivial if it is the only handler in method
    *                         and just rethrows any kind of caught exception
    * @param hasMarkedRegions if there are marked regions in method, profiler should record hits into them
    * @param siberiaOffset    offset of siberia part in PGO host's code
    * @param frame            description of the method's frame
    */
  protected def sendMethodCode0(codeUnit: CodeUnit, seg: Segment, xinfo: XInfo, xTable: ByteBuffer,
                                trivialXHandler: Boolean, hasMarkedRegions: Boolean, siberiaOffset: Int,
                                frame: Frame[?, ?, ?]): Unit

  def sendMethodCode(codeUnit: CodeUnit, seg: Segment, xinfo: XInfo, markedRegions: Seq[MarkedRegion],
                     siberiaOffset: Int, frame: Frame[?, ?, ?], slotOffset: Frame.Slot => Int): Unit = {

    assert(seg.frozen)

    if (enabled(PrintDeltaMaps)) {
      reportDeltaMaps(codeUnit, xinfo)
    }

    val packed = new XTableGenerator(codeUnit.method, slotOffset)(this)
      .packXInfo(xinfo, markedRegions)

    sendMethodCode0(codeUnit, seg, xinfo, packed.xTable, trivialXHandler = packed.trivialXHandler,
      hasMarkedRegions = markedRegions != null && markedRegions.nonEmpty, siberiaOffset, frame)
  }

  /** Returns iterator over types, required for preparation from current compiled type. */
  def getMarkedForPreparationTypes: Iterator[Type]

  /** Marks given `type` as type, required for preparation from current compiled type. */
  def markForPreparation(`type`: Type): Unit

  /** Marks given `type` as type, required for bootstrap preparation from current compiled type. */
  def markForBootstrapPreparation(`type`: Type): Unit

  /** Returns whether class hierarchy analysis is enabled.
    * It is not allowed for classes which object files (.obj) are subject for subsequent compilation reuse.
    */
  def isCHAEnabledForTraceableReferences: Boolean

  def getBuiltInFieldOffset(f: BuiltInField): Int

  def getRTSProc(proc: RTSProc): Method

  def getSpecStrConcatMethod(format: String): Method

  def getTypeProvider: TypeProvider

  def getSymbolLinker(rootClass: ClassType): SymbolLinker

  def getSymbolLinker(rootMethod: Method): SymbolLinker = getSymbolLinker(rootMethod.getDeclaringClass)

  def pdb: PDB2

  def getSymlevelWriter(writer: SymlevelWriter.StreamWriter, contextClass: Type): SymlevelWriter

  def getSymlevelReader(reader: SymlevelReader.StreamReader, contextClass: Type): SymlevelReader

  def dropSymCache(): Unit


  /** Returns true if this method performs initialization of all turbo clinited classes.
    *
    * @see Type#isTurboClinited()
    */
  def isTurboClinitHost(method: Method): Boolean = {
    method.isConstructor && method.getDeclaringClass.isJavaLangClassLoader
  }

  /** `xiEnvModule.info.forcePrint` wrapper. */
  def forcePrint(s: String): Unit

  /** `xiEnvModule.info.print` wrapper. */
  def print(s: String): Unit

  /** Same as [[forcePrint]], but with a newline added. */
  def forcePrintln(s: String): Unit = {
    forcePrint(s)
    forcePrintln()
  }

  /** Same as [[print]], but with a newline added. */
  def println(s: String): Unit = {
    print(s)
    println()
  }

  /** Forced prints a line separator (newline). */
  def forcePrintln(): Unit = forcePrint(OS.host.lineSeparator)

  /** Prints a line separator (newline). */
  def println(): Unit = print(OS.host.lineSeparator)

  /** Prints stack trace of given `error` using [[forcePrintln]]. */
  def forcePrintln(error: Throwable): Unit = {
    forcePrintln(TextOutput.asString(_.printStackTrace(error)))
  }

  /** Prints given string to stdout and reports it to LaunchPad
    * iff `decor` option contains `header` decoration.
    */
  def reportStatus(stage: String, methodName: String): Unit

  /** Reports that given method name from jprof file is inconsistent with compilation environment.
    * If `fatal` is set, terminates the compilation.
    * TODO: generalize or move to more PGO-specific location.
    */
  def reportPGOFailure(methodName: String, fatal: Boolean): Unit

  /** `xiEnvModule.info.print` wrapper,
    * but prints only if `decor` option contains `warnings` decoration.
    */
  def reportWarning(s: String): Unit

  def reportDeltaMaps(codeUnit: CodeUnit, xinfo: XInfo): Unit = {
    forcePrintln()
    forcePrintln(s"Printing delta maps for code unit $codeUnit")
    for (x <- xinfo.getCollectedXSites) {
      forcePrintln(s"    xsite offset = ${x.siteOffset}; delta map = ${x.gcDeltaMap}; kind = ${x.kind}")
    }
    forcePrintln()
  }

  /** Returns index of given class in import table of a `host`.
    *
    * Depending on the calling context `host` can be one of the following:
    *  - `Method` instance: for imports associated with a method being generated.
    *  Given method must be the root method of a compilation.
    *  Mostly used for JIT as it cannot generate class-wide import tables.
    *  - `Type` instance: for imports associated with a whole class
    *  (e.g. those referenced from Serial type info).
    *  Should only be used in pure AOT.
    *  - `null` is deprecated but still allowed for uses where it is known
    *  that host can be determined from context (e.g. unit-tests & AOT code gen).
    *
    * If given class is not in import table yet, it is appended to import table after normal imports.
    */
  def getImportedClassIdx(importedType: Type, host: Object): Int


  ///////////////////////////////////////////////////////////////////////////
  // Options support

  // TODO: these mutable fields are really bad for JIT, see JET-12499
  // optionsCache access must be thread-safe to avoid data-races in JIT (see JET-12684),
  // but can't be represented as ConcurrentHashMap as those cannot store nulls.
  private val optionsCache = mutable.HashMap.empty[CompilerOption[?], Any]
  private val optionsCacheLock: Lock = newLock()

  // For unit-tests only
  protected def dropOptionsCache(): Unit = optionsCache.clear()

  /** Returns `option` value from defined configuration (o2-project system, jit options or fake for unit-tests). */
  protected def optionValueOrNullFromConfig(option: CompilerOption[?]): Any

  @SuppressWarnings(Array("unchecked"))
  private def optionValueOrNull[T >: Null <: AnyRef](option: CompilerOption[T]): T = {
    def optionValue(option: CompilerOption[T]): T = {
      val value: T = if (!option.isAlias) optionValueOrNullFromConfig(option).asInstanceOf[T] else null
      if (value == null) option.defaultValueOrNull(this) else value
    }

    // thread-safe optionsCache access
    optionsCacheLock.sync {
      optionsCache.getOrElseUpdate(option, optionValue(option)).asInstanceOf[T]
    }
  }

  final def defined(option: CompilerOption[? >: Null <: AnyRef]): Boolean = optionValueOrNull(option) != null

  /** Returns the value of `option`. */
  final def enabled(option: BoolOption): Boolean = {
    val valueOrNull = optionValueOrNull(option)
    valueOrNull != null && valueOrNull
  }

  final def valueOfOrNull(option: StrOption): String = optionValueOrNull(option)

  final def valueOf(option: StrOption): String = valueOfOrNull(option).nn

  final def valueOfOrElse(option: StrOption, defaultValue: String): String = {
    val valueOrNull = valueOfOrNull(option)
    if (valueOrNull != null) valueOrNull.nn else defaultValue
  }

  /** Returns list of elements specified by `option`, separated by `,` character. */
  final def listOf(option: StrOption): Array[String] = valueOfOrElse(option, "").split(',')

  final def valueOf(option: NumOption): Int = option.rangeCheck(optionValueOrNull(option).nn)

  /** Returns an iterator over hot switch cases specified in JCA for given `method`
    * based on provided `cases` number and the range specified in the JCA entry.
    */
  def getHotSwitchCases(method: Method, cases: Int): Iterator[Int]

  def getDebugIrLogsDir: Path

  def asVerifiableMethod(method: Method): VerifiableMethod

  private var curPos: Position = NoPosition
  def currentDebugPosition: Position = curPos
  def withDebugPosition[T](pos: Position)(action: => T): T = {
    val oldPos = curPos
    curPos = pos
    try {
      action
    } finally {
      curPos = oldPos
    }
  }
}
