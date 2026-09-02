/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.driver

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.Language.CANGJIE
import com.huawei.excelsior.common.LanguagePack
import com.huawei.excelsior.jet.compiler.Env.{isStandalone, languagePack, targetArch}
import com.huawei.excelsior.jet.compiler.driver.CompilationMode.*
import com.huawei.excelsior.jet.compiler.options.BoolOption.{ONoCode, *}
import com.huawei.excelsior.jet.compiler.options.NumOption.{MinClassAmountForParallelCompilation, Parallelism}
import com.huawei.excelsior.jet.compiler.options.StrOption.{AppType, OPTRTFILES}
import com.huawei.excelsior.jet.compiler.{Env, Environment}

/** This object encapsulates logic about global project decisions, like parallelism and middle stage usage.
  *
  * @author conwor
  */
object ProjectLogic {

  /////////////////////////////////////////////////////////////////////////////
  // Properties of compilation environment and compilation set on which the
  // project logic decisions are based

  private var _env: Environment = _
  def env: Environment = _env ensuring (_ != null)
  def env_=(x: Environment): Unit = { assert(_env == null); _env = x }
  def setEnvForUnitTests(x: Environment): Unit = _env = x

  private var _classesAmount: Int = -1
  def classesAmount: Int = _classesAmount ensuring (_ != -1)
  def classesAmount_=(x: Int): Unit = { assert(_classesAmount == -1); _classesAmount = x }


  /////////////////////////////////////////////////////////////////////////////
  // Parallelism predicates and amount of workers

  /** Returns true, iff parallelism may be used in this compilation (based only on compilation options). */
  lazy val parallelismMayBeEnabled: Boolean =
    (env.valueOf(Parallelism) > 1) &&
      (targetArch != CBC) &&
      !env.enabled(GenCbcStdLib)

  /** Returns true, iff parallelism will be used in this compilation. */
  lazy val parallelismEnabled: Boolean =
    parallelismMayBeEnabled &&
      (classesAmount >= env.valueOf(MinClassAmountForParallelCompilation))

  /** Returns amount of workers used in parallelism. */
  lazy val workersAmount: Int = { assert(parallelismEnabled); env.valueOf(Parallelism) }

  /** Returns true iff `classesAmount` was already set. In this case we can ask `parallelismEnabled` predicate. */
  lazy val classesAmountWasSet: Boolean = _classesAmount != -1


  /////////////////////////////////////////////////////////////////////////////
  // Compiler phases predicates

  /** Returns true iff this compilation is XKRN build which may be parallel. Uses [[parallelismMayBeEnabled]] instead
    * of [[parallelismEnabled]] because classes amount property is not ready yet. Anyway, we are sure that for XKRN
    * it is not important.
    */
  private lazy val parallelXKRNBuild: Boolean = env.enabled(BuildXKRN) && parallelismMayBeEnabled

  /** Returns true iff middle stage should not be used in this compilation. */
  private lazy val noMiddleStage: Boolean = {
    if (env.enabled(IgnoreMiddleStageLogic)) {
      !env.enabled(MiddleStage)
    } else {
      // Middle phase always enabled in parallel XKRN build as a workaround for JET-17391
      // Also middle stage always enabled in Scala LP as a workaround for JET-17444
      (compilationMode == O1) && !parallelXKRNBuild && (languagePack != LanguagePack.SCALA)
    }
  }

  /** Returns true iff middle stage should be used in this compilation. */
  lazy val useMiddleStage: Boolean = !noMiddleStage


  /////////////////////////////////////////////////////////////////////////////
  // PDB

  /** Returns true iff IRB & IRIE PDB should be opened.
    *
    * We try not to open them to optimize compilation time, but in some cases we should open them anyway:
    *   1. When middle stage used in our compilation set (IRB & IREI used to serialize methods after middle stage)
    *   1. When AOT targets use, because they have lowering with inlined calls
    *   1. When profile library compiled, because they re-compile delayed intrinsics
    *
    * TODO: make PDB lazy opened and remove all this complications
    */
  lazy val openIRAndExtraInfoPDB: Boolean = useMiddleStage || (targetArch != CBC) || env.enabled(GenProfileLibrary)


  /////////////////////////////////////////////////////////////////////////////
  // Compilation mode

  /** Returns default compilation mode for the project. */
  lazy val compilationMode: CompilationMode = {
    if (env.enabled(ONoCode)) {
      assert(targetArch == CBC)
      CompilationMode.ONoCode

    } else if (env.enabled(O1ForCJStdLib) && (env.enabled(GenLibrary) || env.enabled(GenCbcStdLib))) {
      O1

    } else if (env.enabled(BuildXKRN) && languagePack.supports(CANGJIE)) {
      // O1 disabled for CJ XKRN as a workaround for macro evaluation problems (JET-17430)
      O2

    } else if (env.enabled(FastBackEnd)) {
      O1

    } else if (isStandalone) {
      // Standalone JC mode is O1-only
      O1

    } else {
      O2
    }
  }


  /////////////////////////////////////////////////////////////////////////////
  // CHA predicates

  /** CHA analysis should be done only for PGO or if `DoCHA` option is enabled */
  def isCHAEnabled = env.enabled(BuildXKRN) || env.enabled(PGO) || env.enabled(DoCHA)


  /////////////////////////////////////////////////////////////////////////////
  // Preparation kind used for compiled classes

  lazy val useLazyPreparation = false

  /////////////////////////////////////////////////////////////////////////////
  // Other oberon options
  
  lazy val multiapp: Boolean = env.enabled(Multiapp) || (env.valueOfOrNull(AppType) != null && env.valueOfOrNull(AppType).equalsIgnoreCase("TOMCAT"))
  
  // TODO: use this instead of StrOption.OPTRTFILES
  lazy val optRTFiles = {
    val optionValue = env.valueOfOrNull(OPTRTFILES)
    if (optionValue == null) {
      "JCE"
    } else {
      assert(optionValue.nonEmpty)
      optionValue
    }
  }
}
