/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.be_386

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.Language.JAVA
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.o2lib.opt.{O2Env, VZCModule as VZC}
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.TypeMetaInfoGenerator
import com.huawei.excelsior.jet.compiler.o2lib.be_386.{CodeDefModule as cd, opAttrsModule as at, opDefModule as opDef}
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, TimeRecModule as TimeRec, xiEnvModule as env}
import com.huawei.excelsior.jet.compiler.symlevel.CallConv.*
import com.huawei.excelsior.jet.compiler.symlevel.ClassType
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.{classByO2Object, typeToO2Class, typesForBootstrapPreparation}
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.{ExteriorMethodsVersioning, VersionedMethod}
import com.huawei.excelsior.jet.compiler.{Pass, Stage}
import xscala.io.TextOutput

object opCodeModule {
  def generateModule(clazz: pcO.Class, stage: Pass): Unit = {
    try {
      at.currProc = null
      at.currClass = clazz

      assert(!clazz.isUnavailable)

      val genObj = genModule(stage)

      if (env.errors.errDetected) {
        return
      }

      if (genObj && (targetArch != CBC)) {
        O2Env.stage(Stage.ObjFile) {
          formOMFModule.generateFormOMF()
        }
      }
      finalizeMemory()
    } catch {
      case e: Throwable =>
        finalizeMemory()

        //  TimeRec.Done();
        if (!e.isInstanceOf[OutOfMemoryError]) {
          val nm = getSafeReadableName(at.currClass)
          val msg = TextOutput.asString(_.printStackTrace(e))
          env.info.forcePrint("%s", s"\n# Compilation of class $nm failed:\n# $msg\n")
        }
        exi()
        throw e
    }
  }

  def exi(): Unit = {
    if (optEntered) {
      VZC.compiler.exitClass(at.currClass)
      optEntered = false
    }
    opDef.exitModule()
    TypeMetaInfoGenerator.Imports.exit()
  }

  def ini(): Unit = {
    opDef.initModule()
  }

  def execute(action: => Unit): Unit = {
    try {
      ini()
      env.errors.execute { action }
    } finally {
      exi()
      if (env.errors.errDetected) { // TODO: make compiler immediately fail at any error and remove all this bullshit about `errDetected`
        env.errDetected = true
      }
    }
  }

  private var optEntered: Boolean = false

  private def finalizeMemory(): Unit = {
    if (env.isProgressShowable) {
      env.info.forcePrint("\\r")
    }
  }

  //---------------------------------------------------
  private def decorEndModule(): Unit = {
    if (env.isProgressShowable) {
      env.printWithErasingPrevious(js.newJString("            "))
    }
  }

  private def getMethodName(p: pc.Symbol): XString = p.getReadableName(need_class_name = false, need_full_sign = false)

  private def decorWriteOptimizing(p: pc.Symbol): Unit = {
    if (env.isProgressShowable) {
      val nm = getMethodName(p)

      env.printWithErasingPrevious(js.format("Optimizing  %S", nm))
    }
  }

  private def genProc(m: pcO.Method, versioned: VersionedMethod, stage: Pass): Unit = {
    var klass: ClassType = null

    if (versioned != null) {
      klass = versioned.getHostingClass
    } else {
      klass = classByO2Object(m.getDeclaringClass)
    }

    assert(klass.isVerifiable)

    if (typeToO2Class(klass) ne at.currClass) {
      return
    }

    // we should pass @Inline methods to opt to let it generate IR for them.
    if (!m.shouldBeGenerated && !m.isInlineAllAndRemove || m.isExternal) {
      // versioned methods should be added only if they are going to be generated
      assert(versioned == null)
      return
    }

    if (!targetOS.isWindows) {
      if (m.getCallConv == STDCALL && m.isVarArgs) { // TODO: replace with `m.getCallConv == STDCALL && m.isCVarArgs`
        env.errors.fault(ErrMsg994, m.getReadableName(need_class_name = true))
      }
    }

    val only_proc = env.config.equation("gen_only_proc")
    if (only_proc != null) {
      val procname = m.getReadableName(need_class_name = false, need_full_sign = false)
      if (!only_proc.equals(procname)) {
        return
      }
    }

    decorWriteOptimizing(m)
    val procname = m.getReadableName(need_class_name = true)
    stage match {
      case Pass.Middle =>
        TimeRec.startStage(TimeRec.METHOD_MIDDLE, procname)
      case Pass.Backend =>
        TimeRec.startStage(TimeRec.METHOD_BACKEND, procname)
    }
    VZC.compiler.compileMethod(m, versioned)
    stage match {
      case Pass.Middle =>
        TimeRec.stopStage(TimeRec.METHOD_MIDDLE, procname)
      case Pass.Backend =>
        TimeRec.stopStage(TimeRec.METHOD_BACKEND, procname)
    }
    if (env.isProgressShowable) {
      env.info.forcePrint("\\r")
    }
  }

  /*------------ G e n e r a t e   m o d u l e   b o d y -----------------------*/
  private def genStaticField(f: pcO.StaticField): Unit = {
    if (f.value != null) { // initialized field
      at.setSegment(f, cd.makeSeg { opDef.putStaticFieldValue(f) })
    } else if (f.hasDataAnnot) {
      at.setSegment(f, cd.makeSeg { TypeMetaInfoGenerator.genDataStaticFields(f) })
    }
  }

  private def genModule(stage: Pass): Boolean = {
    val t = at.currClass

    if (!languagePack.supports(JAVA) && t.isAnnotation) {
      // do not generate annotations in no java mode
      return false
    }

    val clsname = t.name

    stage match {
      case Pass.Middle =>
        TimeRec.startStage(TimeRec.CLASS_MIDDLE, clsname)
      case Pass.Backend =>
        TimeRec.startStage(TimeRec.CLASS_BACKEND, clsname)
    }

    optEntered = false

    VZC.compiler.enterClass(t, stage)
    optEntered = true
    var generateObj = false

    ExteriorMethodsVersioning.collectVersionedMethods(t) // before gen_tdesc

    // before code generation
    TypeMetaInfoGenerator.Imports.outImportTable()

    // methods
    for (m <- t.declaredMethods) {
      generateObj = generateObj || m.shouldBeGenerated && !m.isExternal
      genProc(m, null, stage)
    }

    for (versioned <- ExteriorMethodsVersioning.getIteratorOverVersionedMethods(t)) {
      generateObj = true
      genProc(versioned.original, versioned, stage)
    }

    if (stage == Pass.Backend) {
      // type descriptor
      if (t.hasMetaInformation && (targetArch != CBC)) {
        generateObj = true
        TypeMetaInfoGenerator.genRunTimeTypeInfo(t)
      }

      if (t.hasManagedMetaInformation) {
        // string table
        val stringTable = t.getStringTable
        if (stringTable != null && stringTable.getLength != 0) {
          opDef.createCstrPool(stringTable)
        }
      } else {
        // static fields
        for (f <- t.declaredStaticFields) {
          if (!f.isCompileTimeConstant) {
            generateObj = true
            genStaticField(f)
          }
        }
      }

      TypeMetaInfoGenerator.createBootstrapObject(t)
    }

    if (optEntered) {
      VZC.compiler.exitClass(t)
      optEntered = false
    }

    at.currProc = null

    // absent TDs
    if (stage == Pass.Backend && t.hasManagedMetaInformation) {
      TypeMetaInfoGenerator.stubAbsentClasses()
    }

    if (stage == Pass.Backend && (t.isBootstrap || typesForBootstrapPreparation.nonEmpty)) {
      generateObj = true
    }

    decorEndModule()

    stage match {
      case Pass.Middle =>
        TimeRec.stopStage(TimeRec.CLASS_MIDDLE, clsname)
      case Pass.Backend =>
        TimeRec.stopStage(TimeRec.CLASS_BACKEND, clsname)
    }

    stage == Pass.Backend && generateObj
  }

  /** ------------------------------ CODE --------------------------------- */
  private def getSafeReadableName(clazz: pcO.Class): XString = {
    try {
      clazz.getReadableName
    } catch {
      case _: Throwable => js.newJString("<unknown>")
    }
  }

  private def initOptsEqus(): Unit = {
    env.config.newOption("GENDLL", value = false)  // TODO: replace with ProjectLogic
    env.equationList("CODENAME;")
    env.config.newEquation("CPU")
    env.config.setEquation("CPU", "GENERIC")
  }

  def init(): Unit = {
    env.config.save()
    initOptsEqus()
  }
}
