/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt

import com.huawei.excelsior.common.CodeHelpers.NotImplementedException
import com.huawei.excelsior.jet.compiler.*
import com.huawei.excelsior.jet.compiler.Pass.{Backend, Middle}
import com.huawei.excelsior.jet.compiler.abi.GCMapStatisticCollector
import com.huawei.excelsior.jet.compiler.driver.ProjectLogic
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile
import com.huawei.excelsior.jet.compiler.opt.platforms.PlatformConfig
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.options.NumOption.*
import com.huawei.excelsior.jet.compiler.serialization.ExtraInfo
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.compiler.util.Log
import xscala.io.stderr
import xscala.util.simpleClassName

/** Optimizing compiler.
  *
  * @author paul
  * @author conwor
  */
class Opt(_env: Environment, platformConfig: PlatformConfig) extends CompilerWithStats(_env) {

  override val stats: Stats = new Stats(env)

  private val verboseProgress = env.enabled(VerboseProgress)

  Log.setEnv(env)

  Profile.blame.planInline(platformConfig, stats)
  Profile.fields.checkJProfDataSanity()

  override def genCode(codeUnit: CodeUnit): Unit = {
    val method = codeUnit.method
    val stage = env.getPass

    if (stage == Backend) {
      registerInputMethod(method)
    }

    if (verboseProgress) println()

    try {
      new IRSandbox(codeUnit, env, stats) run { sandbox =>
        val worker = env.valueOf(Worker) != 0

        if (codeUnit.isVersionedMethod) {
          assert(!method.isInlineAllAndRemove)
          Opt.passFront(method, env, platformConfig, stats, null) ensuring (_ == true)
          if (stage != Middle) env.stage(Stage.OptStartBack) {
            platformConfig.back(sandbox, parent = null).run()
          }

        } else { // normal method
          stage match {
            case Middle =>
              if (!Opt.passedFront(method, env)) env.stage(Stage.OptStartFront) {
                platformConfig.front(sandbox, parent = null).run()
              }

            case Backend =>
              if (Opt.passedFront(method, env)) env.stage(Stage.OptStartBack) {
                if (!method.isInlineAllAndRemove) {
                  platformConfig.back(sandbox, parent = null).run()
                }
              } else if (method.isInlineAllAndRemove) env.stage(Stage.OptStartFront) {
                platformConfig.front(sandbox, parent = null).run()
              } else env.stage(Stage.OptStartComboFrontBack) {
                platformConfig.comboFrontBack(sandbox).run()
              }
          }
        }

        if (verboseProgress) println()

        if (stage == Backend) {
          registerCompiledMethod(method)
        }
      }

    } catch {
      case e: Throwable => throw new CompilerException(s"Could not compile $method", e)
    }
  }

  override def printFinalStatistics(): Unit = {
    Log.closeAll()
    stats.finishVerbosePrinting()

    if (!env.enabled(SilentCompilation)) {
      printStats("Optimizing compiler")
      GCMapStatisticCollector.print()(env)
    }

    if (env.enabled(VerboseInlinePlanning) && env.enabled(PGOChains)) {
      Profile.blame.inlinePlan.printAsDOT("Inlined", null)
    }
  }
}

object Opt {

  private def extraInfoAvailableFor(method: Method, env: Environment): Boolean =
    ProjectLogic.openIRAndExtraInfoPDB && method.shouldBeSerialized

  private def passedFront(method: Method, env: Environment) =
    extraInfoAvailableFor(method, env) && ExtraInfo.methodHasGlobalExtraInfo(method, env)

  /** Returns true, iff given `method` has passed front stage. May provoke it's compilation. */
  def passFront(method: Method, env: Environment, platformConfig: PlatformConfig, stats: Stats, parent: Universe): Boolean = {
    if (passedFront(method, env)) {
      // Check the method already pass front
      true
    } else if (!extraInfoAvailableFor(method, env)) {
      // Check if it is useless to try to pass front for method, because its extra info will never be written
      false
    } else {
      // Try to pass front
      runFront(method, env, platformConfig, stats, parent)
    }
  }

  def canParseMethod(method: Method): Boolean = {
    if (Env.isStandalone) {
      method.getCHIRDef.nonEmpty
    } else {
      // FIXME: fix me when cangjie will have profile classes
      (((method.getDeclaringClass.isBytecodeAvailable || method.getDeclaringClass.isCangjieType || method.getDeclaringClass.isLambdaClass) && !method.isNative) ||
        (method.isAJReplaced && method.getAJReplacement != null)) && (method.getDeclaringClass.isInCurrentCompilationSet || method.shouldBeSerialized)
    }
  }

  private def runFront(method: Method, env: Environment, platformConfig: PlatformConfig, stats: Stats, parent: Universe): Boolean = {
    assert(!method.isAbstract)

    if (parent != null) {
      if (parent.methodAlreadyInCompilation(method) ||
        method.getDeclaringClass.isJavaAnnotatedCangjieClass ||
        (env.valueOf(Worker) != 0)) {

        return false
      }
    }

    if (!canParseMethod(method)) {
      return false
    }

    if (method.isMethodInfoFrameDescriptorGetter) {
      // Workaround for JET-17071
      return false
    }

    val currentGlobalInfoVersion = ExtraInfo.globalInfoVersion

    new IRSandbox(CodeUnit.of(method), env, stats).run { sandbox =>
      platformConfig.front(sandbox, parent).run()
    }

    if ((parent != null) && (ExtraInfo.globalInfoVersion > currentGlobalInfoVersion)) {
      parent.invalidateGlobalDependentNodeTypes()
    }

    true
  }

  def reportCrash(env: Environment, compilerName: String, codeUnit: CodeUnit, e: Throwable): Unit = {
    if (!env.enabled(VerboseCrashes)) {
      return
    }

    val reason = if (isCompilerException(e)) simpleClassName(e) else "unknown reason"
    println(s"$compilerName failed by $reason for $codeUnit")

    print("  ")
    stderr.printStackTrace(e)

    if (env.enabled(TerminateOnNotSuppressedVerboseCrash) && env.valueOf(Worker) == 0) {  // no sys.exit() for worker mode to let the worker close the socket normally
      // Standard mechanism of crash reporting is too verbose during active development.
      sys.exit(1)
    }
  }

  def isCompilerException(e: Throwable) =
    e.isInstanceOf[CompilerException] || e.isInstanceOf[NotImplementedException]
}
