/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.common.Arch.*
import com.huawei.excelsior.common.Language.JAVA
import com.huawei.excelsior.common.LanguagePack
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.delayed.DelayedIntrinsicsUsageTracker
import com.huawei.excelsior.jet.compiler.driver.ProjectLogic
import com.huawei.excelsior.jet.compiler.o2lib.opt.{O2Env, VZCModule as VZC}
import com.huawei.excelsior.jet.compiler.o2lib.be_386.opCodeModule
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, ExtraPassModule as ep, pcJCAModule as pcJCA, pcNamesModule as pcNames, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.{JBCPreprocessor, jbcFrontModule, JavaClassParserModule as jcp}
import com.huawei.excelsior.jet.compiler.o2lib.jprof.{JProf, JProfManagerModule as JProfManager}
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.DecorParser.parseDecor
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.MemorySizeParser.parseMemorySize
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.scanners.ScanUse.usgExtractClassName
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.scanners.{ScanFus, ScanPro, ScanRed, ScanUse}
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.{xArchivePDBModule, RTCacheModule as RTCache, xPDBModule as xPDB}
import com.huawei.excelsior.jet.compiler.o2lib.u.{BundleImportResolverModule as BundleImportResolver, CacheAPIModule as CacheAPI, CompilationDriverModule as CompilationDriver, DirsModule as Dirs, JStringsModule as js, PropertiesModule as Properties, StringTokenizerModule as strtok, TimeRecModule as TimeRec, xOptionsModule as opt, xcCompModule as xcComp, xcMain0Module as xcMain0, xcMakeModule as mk, xcModesModule as xcModes, xiEnvModule as env, xiFilesModule as xfs, xmErrorsModule as xmErrors, xmZipModule as xmZip}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule as FS
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.options.BoolOption.{GenLibrary, GenMegaObj, PGO, Prelink, PrelinkExe, ReuseRtDwarf}
import com.huawei.excelsior.jet.compiler.options.Option.SmartKind.*
import com.huawei.excelsior.jet.compiler.options.StrOption.{DefaultCompactProfile, FrontEnd, Locales, OPTRTFILES, UseLibrary}
import com.huawei.excelsior.jet.compiler.smart.ImportResolutionType
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.{JavaVerifier, LightweightEnvironment}
import com.huawei.excelsior.jet.compiler.verifier.VerificationError.ExceptionKind.UnsupportedClassVersionError
import com.huawei.excelsior.jet.compiler.{Pass, Stage}
import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.io.TextOutput
import xscala.management.Management
import xscala.util.StringOps.*
import xscala.util.{UByte, UInt}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/* Project system user interface */
object xcMainModule { /* Ned 04-Feb-93. */

  /////////////////////////////////////////////////////////////////////////////
  // Front-end component

  private object Frontend {
    def parseModule(src: xfs.FileDescriptor): pcO.Class = jbcFrontModule.parseClassFile(src)

    def processModule(clazz: pcO.Class): Unit = {
      pc.currentModule = clazz.mno
      JavaVerifier {
        _.verify(clazz)
      }
      jbcFrontModule.completeSymLevel(clazz)
      clazz.classInfo = null
      pcO.outSymFile(clazz)
    }

    def execute(action: => Unit): Unit = {
      try {
        env.errors.execute { action }
      } finally {
        if (env.errors.errDetected) { // TODO: make compiler immediately fail at any error and remove all this bullshit about `errDetected`
          env.errDetected = true
        }
      }
    }
  }

  private def parsingStage(fd: xfs.FileDescriptor, parseModule: Boolean, clazzPar: pcO.Class): pcO.Class = {
    var clazz = clazzPar

    if (fd != null) {
      env.info.filename = fd.getName
      env.info.lines = fd.getLength
    } else {
      env.info.filename = null
    }

    Frontend.execute {
      try {
        if (parseModule) {
          clazz = Frontend.parseModule(fd)
        }
        Frontend.processModule(clazz)
      } catch {
        case e: OutOfMemoryError => throw e
        case e: Throwable =>
          env.info.forcePrint("%s", s"\n# Compilation of class ${fd.getName} failed:\n# ${TextOutput.asString(_.printStackTrace(e))}\n")
          throw e
      }
    }

    clazz
  }

  private def generateModule(p: Project, clazz: pcO.Class, stage: Pass, boc: XString): Boolean = {
    env.info.module = clazz.nameObj

    if (!env.errors.errDetected) {
      if (boc == null || clazz.name.equals(boc)) {
        opCodeModule.execute {
          opCodeModule.generateModule(clazz, stage)
        }
      }
    }
    val errFlag = env.errors.errDetected // TODO: refactor and remove errors.errDetected

    if (errFlag) {
      p.errs += 1
    }
    env.errors.showErrors()
    env.info.report()
    env.errors.reset()

    pcO.symCache_gc_BackEndFinishedFor(clazz)
    !errFlag
  }


  class Project extends mk.Project {
    private[xcMainModule] var customClassloaders: Boolean = _

    override def readBundleDescriptor(f: xfs.FileDescriptor): Unit = {
      BundleImportResolver.readBundleDescriptor(f, this)
    }

    override def openPDB(): Unit = {
      assert(xPDB.manager != null)
      initOutputName(fatal = false)
      val reused = xPDB.openMainPDB(xcModes.workerMode, newResourceCleanupAdviser(this))
      assert(!xcModes.workerMode || reused)
    }

    override def findClassAndAppend(str: XString, doError: Boolean): Unit = {
      var file: mk.File = null

      val name = pcNames.parseMangledName(str)
      if (!mk.chkFile(this, name, mk.SetOfModes.of(mk.md_jbc, mk.md_sym))) {
        if (pcNames.isBundleClassName(name)) {
          file = BundleImportResolver.appendBundleClass(this, name)
        } else {
          file = this.appendJava(str, symImport = false, doError)
        }

        if (file == null) {
          return
        }

        file.tags += mk.compileall.toUByte

        val nt = new mk.Node()
        nt.mod = file
        if (this.tail != null) {
          this.tail.next = nt
          this.tail = nt
        } else {
          this.tail = nt
        }
        if (file.mode == mk.md_sym) {
          val imp = pcO.prjSys_getClassByName(name)
          assert(imp != null)
          this.addImport(imp, fromsym = true)
        }
      } else {
        val l = mk.search(this.hashtable, name)

        if (l.asInstanceOf[mk.File].mode == mk.md_sym) {
          assert(!pcNames.isBundleClassName(name))
          val imp = pcO.prjSys_getClassByName(name)
          assert(imp != null)
          this.addImport(imp, fromsym = true)
        } else {
          l.asInstanceOf[mk.File].tags += mk.compileall.toUByte
        }
      }
    }

    override def compile(): Unit = {
      if (!this.initCompile()) {
        return
      }

      val workerMode = xcModes.workerMode

      addClassesFromExecutionProfileToCompilationSet(this)
      initCompactProfileTypes(this.findClassAndAppend(_, doError = true))

      env.info.print("\\n------------------------  Parsing Stage  ---------------------------------------\\n\\n")

      analyzeClassFiles()

      // currently we cannot skip parsing stage in relink only mode,
      // because xcF requires modules information to be collected
      if (!this.performParsingStage()) {
        return
      }

      // drop symlevel cache on java side, that may hold internal
      // class data from parsing stage, before CHA initialization in opt
      // because types in CHA may live forever with all their contents from parsing stage
      VZC.dropSymCache()

      // TODO: is it actual after removing baseline backup path?
      if (env.config.tags contains env.regularbuild) {
        // Adding JRE jars to lookup to allow baseline compiler compile
        // some JRE classes that cannot be compiled by the main compiler.
        // Add them after parsing stage to not let compiler to find classes instead
        // of sym files on parsing stage.
        xfs.sys.saveRed()

        addLookupToJetRTJar(s"jet-rt-${languagePack.toString.asciiToLowerCase}.zip")

        if (languagePack.supports(JAVA)) {
          addLookupToJREJar("rt.jar")
          addLookupToJREJar("charsets.jar")
          addLookupToJREJar("jsse.jar")
          addLookupToJREJar("jce.jar")
          addLookupToJREJar("ext/cldrdata.jar")
          addLookupToJREJar("ext/dnsns.jar")
          addLookupToJREJar("ext/jfxrt.jar")
          addLookupToJREJar("ext/localedata.jar")
          addLookupToJREJar("ext/nashorn.jar")
          addLookupToJREJar("ext/sunec.jar")
          addLookupToJREJar("ext/sunjce_provider.jar")
          addLookupToJREJar("ext/sunpkcs11.jar")
          addLookupToJREJar("ext/zipfs.jar")
          if (targetOS.isWindows) {
            if (targetArch == AMD64) {
              addLookupToJREJar("ext/access-bridge-64.jar")
            }
            addLookupToJREJar("ext/jaccess.jar")
            addLookupToJREJar("ext/sunmscapi.jar")
          }
        }
      }

      env.stage = env.BACK

      pcJCA.JCACheck()

      if (!this.checkMains()) {
        return
      }

      VZC.compiler // force evaluate

      if (env.config.tags contains env.regularbuild) {
        loadImport()
      }

      markPGORecompilationSet(JProf.manager)

      pcO.symCache_gc_StartCodegenStage()

      if (env.config.tags contains env.regularbuild) {
        if (workerMode) {
          this.startWorker(regular = true)
          return
        } else if (!this.compileRegular()) {
          sys.exit(1)
        }
      } else if (workerMode) {
        this.startWorker(regular = false)
        return
      } else if (!this.compileSpecial()) {
        sys.exit(1)
      }

      VZC.compiler.printFinalStatistics()

      env.config.setEquation2("CPURequirements", env.config.equation("CompilerCPURequirements"))

      pcO.symCache_gc_EndCodegenStage()
    }

    /**
    Special compilation mode for building JET profiles and startups
      */
    def compileSpecial(): Boolean = {
      if (env.config.option("nogencode")) {
        return true
      }

      markRedundantFileDesc(this, SPECIAL)

      if (!this.compileProject(newSpecialProjectIteratorFactory(this), recompileRuntime = false)) {
        return false
      }

      DelayedIntrinsicsUsageTracker.serialize()

      true
    }

    def startWorker(regular: Boolean): Unit = {
      if (regular) {
        val recompileRuntime = shouldRecompileRuntime()
        val useRTCache = RTCache.findGlobalRTCache()
        this.prepareCompilation(recompileRuntime, useRTCache)
      }
      O2Env.stage(Stage.xcMain_CodegenStage) {
        val actor = newCompilationActor(this, Pass.Backend, shouldRecompileRuntime())
        CompilationDriver.startWorker(actor)
        DelayedIntrinsicsUsageTracker.serialize()
        VZC.compiler.printFinalStatistics()
      }
    }

    def compileRegular(): Boolean = {
      val recompileRuntime = shouldRecompileRuntime()
      val useRTCache = RTCache.findGlobalRTCache()
      this.prepareCompilation(recompileRuntime, useRTCache)

      markRedundantFileDesc(this, REGULAR)

      if (!this.compileProject(newRegularProjectIteratorFactory(this, useRTCache), recompileRuntime)) {
        return false
      }

      val genlib = O2Env.env.enabled(GenLibrary)
      val librariesStr = O2Env.env.valueOfOrNull(UseLibrary)
      val usesCangjieStdLib = (librariesStr != null) && librariesStr.split(',').exists(_.equals("CangJieStdLib"))

      if (!genlib && !usesCangjieStdLib) {
        this.copyReusableObjs(!recompileRuntime, useRTCache)
      }

      // promote to local cache
      var cacheChanged = false
      for (c <- pcO.allClasses) {
        if (c.isRuntimeReusable) {
          promoteRuntimeReusableToCache(c)
          cacheChanged = true
        }
      }
      if (cacheChanged) {
        RTCache.updateCache()
      }

      true
    }

    def compileProject(iteratorFactory: ProjectIteratorFactory, recompileRuntime: Boolean): Boolean = O2Env.stage(Stage.xcMain_CodegenStage) {
      val classAmount = getClassAmount(iteratorFactory.newProjectIterator())
      ProjectLogic.classesAmount = classAmount

      if (ProjectLogic.useMiddleStage) {
        initClassAmount(classAmount)
        middleMessage()
        if (!this.doCompilation(iteratorFactory.newProjectIterator(), Pass.Middle, recompileRuntime)) {
          return false
        }
      }

      ReplacementLibrary.serialize()
      if (ProjectLogic.parallelismEnabled) {
        flushPDBAfterMiddleStage()
      }

      initClassAmount(classAmount)
      codeGenMessage()
      if (!this.doCompilation(iteratorFactory.newProjectIterator(), Pass.Backend, recompileRuntime)) {
        this.errs += 1
        return false
      }

      this.mergePDBsFromWorkers(iteratorFactory.newProjectIterator())
      true
    }

    def prepareCompilation(recompileRuntime: Boolean, useRTCache: Boolean): Unit = {
      markRecompilationSet(this, !recompileRuntime, useRTCache)
    }

    def copyReusableObjs(useXKRNObjFiles: Boolean, useRTCache: Boolean): Unit = {
      val supportsJava = languagePack.supports(JAVA)
      for (c <- pcO.allClasses) {
        if (!c.requiredRecompilation && this.getProjectFile(c.nameObj, mk.SetOfModes.of(mk.md_jbc)) == null && (supportsJava || c.isNoJavaClass && !c.isAnnotation)) {
          // In NoJava mode we only link a limited set of runtime classes into the final binary or none.
          // Most common case is to link AJ library classes only.
          tryReuseObj(c, useXKRNObjFiles, useRTCache)
        }
      }
      if (env.config.option(s"$ReuseRtDwarf")) {
        tryReuseDWARFObj(useXKRNObjFiles, useRTCache)
      }
    }

    def mergePDBsFromWorkers(iterator: CompilationDriver.ProjectIterator): Unit = {
      import xPDB.ContentType as CT

      val mainPDB = xPDB.manager.mainPDB
      if (ProjectLogic.parallelismEnabled) {
        mainPDB.mergeFromWorkers()
        mainPDB.repackByOrder(CT.OBJ, this.collectEntriesByCompilationOrder(iterator).iterator.map(
          entry => xPDB.createPlaceName(entry, CT.OBJ)))
      }
      if (xPDB.stableBuild) {
        for (ct <- List(CT.SYM, CT.IRB, CT.IREI)) {
          mainPDB.repackByOrder(ct)
        }
      }
    }

    private def collectEntriesByCompilationOrder(iterator: CompilationDriver.ProjectIterator): ArrayBuffer[XString] = {
      val entries = new ArrayBuffer[XString]
      while (iterator.hasNext) {
        val cuId = iterator.next()
        val x = this.getProjectFileByStringID(cuId)
        val c = if (x.mode == mk.md_sym) {
          pcO.findClassByNameObject(x.name)
        } else {
          x.clazz
        }
        if (!noObjRTClass(c)) {
          entries += c.getMangledName
        }
      }
      entries
    }

    def doCompilation(iterator: CompilationDriver.ProjectIterator, stage: Pass, recompileRuntime: Boolean): Boolean = {
      val actor = newCompilationActor(this, stage, recompileRuntime)
      if (stage != Pass.Backend) {
        while (iterator.hasNext) {
          val cuId = iterator.next()
          if (!actor.compile(cuId)) {
            return false
          }
        }
        true
      } else {
        CompilationDriver.doCompilation(iterator, actor)
      }
    }

    def analyzeClassFiles(): Unit = {
      // TODO: simplify these nested loops
      var n = this.nodes
      while ({
        while (n != null) {
          val x = n.mod
          var state = xcComp.isCompilable(x)

          if (state == mk.compilable) {
            assert(x.mode == mk.md_jbc)
            if (x.fd != null) {
              jbcFrontModule.analyzeClassFile(x.fd)
            }
          }
          n = n.next
        }
        n = this.tail
        n = n.next

        n != null
      }) ()
    }

    def performParsingStage(): Boolean = {
      val savdecor = env.decor
      env.decor -= env.dc_report.toUByte
      // TODO: simplify these nested loops
      var n = this.nodes
      while ({
        while (n != null) {
          val x = n.mod
          var state = xcComp.isCompilable(x)

          if (state == mk.compilable) {
            xmErrors.classAmount += 1
            env.info.module = x.name
            assert(x.mode == mk.md_jbc)
            if (x.fd != null) {
              this.parseModule(x)
            }
            env.errors.showErrors()
            x.tags += mk.isCompilable.toUByte
          } else if (x.mode == mk.md_jbc) {
            val name = if (pcNames.isClassName(x.name)) {
              pcNames.newClassName(JBCPreprocessor.movedScalaClassName(x.name.name))
            } else {
              x.name
            }
            var imp = pcO.prjSys_getClassByName2(name)
            if (imp != null) {
              this.addImport(imp, fromsym = false)
            } else {
              imp = pcO.findClassByNameObject(name)
            }
            assert(imp != null)
            x.clazz = imp
          }
          state = xcComp.isCompilable(x, reuse = false)
          if (state == mk.compilable) {
            val c = x.clazz
            c.fileDescriptor = x.fd
          }
          n = n.next
        }
        n = this.tail
        n = n.next

        n != null
      }) ()
      ep.exi()
      env.decor = savdecor
      if (this.errs != 0) {
        return false
      }
      reportFuzzies()
      env.info.reset()

      true
    }

    def initCompile(): Boolean = {
      val frontend = O2Env.env.valueOf(FrontEnd)
      if (frontend.equalsIgnoreCase("JBC")) {
        jbcFrontModule.set()
        jbcFrontModule.ini()
        jcp.needVerify = !env.config.option("VerifyNone")
        jcp.relaxVerify = !env.config.option("VerifyAll")
      } else {
        env.errors.fault(ErrMsg550, frontend)
      }

      if (O2Env.env.enabled(PrelinkExe) || O2Env.env.enabled(Prelink)) {
        env.config.setOption(s"$GenMegaObj", value = true)  // TODO: try to replace with ProjectLogic
      }

      if (O2Env.env.enabled(Prelink)) {
        assert(languagePack != LanguagePack.JAVA)
      }

      if (!checkNoSplashExtensionOptions()) {
        this.errs += 1
        return false
      }

      pcO.iniOpt()

      initHeapLimit()

      env.clear()

      if (pcO.isTomcat || pcO.isIdea || pcO.isSpringBoot) {
        this.customClassloaders = true
        val classloaderIDGetter = new BundleResolverClassloaderIDGetter()
        pcO.setClassloaderIDGetter(classloaderIDGetter)
      } else {
        this.customClassloaders = false
      }

      if (!checkDllName()) {
        env.exit(msg_no_dllname.no)
      }

      initOutputName()
      initOutputDir()

      initJREHome()

      checkSplashImage()

      initCompactProfiles()

      // After InitOutputName
      JProf.initJProf()
      ReplacementLibrary.deserialize()

      initParallelism()

      if (xmErrors.jarsClassAmount > 0) {
        env.info.print("\\nTotal compilable classes within classpath entries: %d\\n", xmErrors.jarsClassAmount)
      }
      if (xmErrors.cacheClassAmount > 0) {
        env.info.print("\\nTotal compilable classes within !module *.cache: %d\\n", xmErrors.cacheClassAmount)
      }

      env.stage = env.FRONT
      xmErrors.classAmount = 0
      xmErrors.parsedClassAmount = 0
      xmErrors.makeobjClassAmount = 0

      def loadType(name: XString): pcO.Class = {
        val clazz = env.loadType(name)
        if (clazz == null) {
          val symZipName = xArchivePDBModule.getProfilePlaceName(xPDB.ContentType.SYM)
          env.errors.fault(ErrMsg460, name, symZipName)
        }
        clazz
      }
      CacheAPI.loadClasses(loadType)

      JProfReadUse(this)

      LightweightEnvironment.checkRTConstConsistency()

      if (env.config.tags contains env.regularbuild) {
        readUse(this)
      }

      xmZip.cleanDirs()

      true
    }

    def checkMains(): Boolean = {

      val mainStr = env.config.equation("MAIN")
      var mainClass: XString = null

      var mainmno = -1
      var hasNextJavaVersionClasses = false

      val dontNeedMain = (!isMainOptionSpecified(mainStr) && env.config.option("GENDLL")) ||
        O2Env.env.enabled(GenLibrary) || O2Env.env.enabled(Prelink) // TODO: check this equation if unmanaged aj compilation mode will exist

      var entrypAmount = 0
      for (cls <- pcO.allClasses) {
        hasNextJavaVersionClasses = hasNextJavaVersionClasses || isNextJavaVersionClass(cls)
        if (this.willObjBeGenerated(cls.nameObj)) {
          val checkMain = if (isMainOptionSpecified(mainStr)) cls.name == mainStr else cls.hasMain
          if (checkMain) {
            entrypAmount += 1
            mainClass = cls.name
            mainmno = cls.mno
            val mainMethodIndex = LightweightEnvironment.methodByO2Object(cls.declaredMethods.find(_.isMainMethod).get).getHostedIndex
            env.config.setEquation2("MainMethodIndex", js.format("%d", mainMethodIndex))
          }
        }
      }

      if (!dontNeedMain) {
        if (isMainOptionSpecified(mainStr)) { // main eq specified
          val maincls = pcO.findClass(mainStr)
          if (maincls != null && !maincls.isAbsent) {
            if (maincls.hasAbsentSuper) {
              env.errors.fault(ErrMsg462, mainStr)
            } else if (!maincls.isVerifiable) {
              if (isNextJavaVersionClass(maincls)) {
                env.errors.fault(ErrMsg386)
              } else {
                env.errors.fault(ErrMsg385)
              }
            } else if (!maincls.hasMain) {
              this.entrypAmountError(entrypAmount, hasNextJavaVersionClasses, main_was_specified = true)
              this.errs += 1
              return false
            }
            mainmno = maincls.mno
          } else {
            // should not happen: not existing main should be handled before
            throw new AssertionError
          }
        } else if (entrypAmount != 1) {
          this.entrypAmountError(entrypAmount, hasNextJavaVersionClasses, main_was_specified = false)
          this.errs += 1
          return false
        } else {
          env.config.setEquation2("main", mainClass)
        }
        if (mainmno == -1) {
          return true
        }
      }
      true
    }

    def entrypAmountError(entrypAmount: Int, hasNextJavaVersionClasses: Boolean, main_was_specified: Boolean): Unit = {

      if (entrypAmount == 0) {
        if (hasNextJavaVersionClasses) {
          env.errors.fault(ErrMsg369)
        } else {
          env.errors.fault(ErrMsg370)
        }
      } else {
        val buf = new js.StringBuffer()

        for (c <- pcO.allClasses) {
          if (c.hasMain && this.willObjBeGenerated(c.nameObj)) {
            buf.append("\n-main=")
            buf.appendString(c.getReadableName)
          }
        }
        if (main_was_specified) {
          env.errors.fault(ErrMsg374, buf.toJString)
        } else {
          env.errors.fault(ErrMsg375, buf.toJString)
        }
      }
    }

    def willObjBeGenerated(name: pcNames.NAME): Boolean = {
      val objFile = this.getProjectFile(name, mk.SetOfModes.of(mk.md_obj))
      objFile != null
    }

    def parseModule(x: mk.File): Unit = {
      TimeRec.startStage(TimeRec.CLASS_PARSING, x.fd.getName)
      enterModule(x)
      var parseModule = false
      if (x.fd.exists) {
        if (!(x.tags contains mk.objs)) {
          parseModule = true
          x.tags += mk.objs.toUByte
        }
        val fd = x.fd

        env.info.filename = fd.getName
        env.info.header()
        x.clazz = parsingStage(fd, parseModule, x.clazz)
        val err = env.errors.errDetected
        env.errors.showErrors()
        xmErrors.parsedClassAmount += 1
        env.info.report()
        env.errors.reset()

        this.addImport(x.clazz, fromsym = false)

        if (err) {
          this.errs += 1
        }
      } else {
        this.errs += 1
      }
      exitModule(x)
      TimeRec.stopStage(TimeRec.CLASS_PARSING, x.fd.getName)
    }

    override def addImport(cls: pcO.Class, fromsym: Boolean): Unit = {
      var x: mk.File = null

      for (imp <- cls.getImport) {
        val name = imp.nameObj
        if (!pcNames.isAbsent(name)) {
          var l = mk.search(this.hashtable, name)
          if (l != null) {
            var y = l.asInstanceOf[mk.File]
            while (y.s != null && !(Javas contains y.mode)) {
              y = y.s.asInstanceOf[mk.File]
            }
            if (!(Javas contains y.mode) || y.mode == mk.md_sym && (y.tags contains mk.importnotscanned)) {
              l = null
            }
          }
          if (l == null) {
            if (pcNames.isBundleClassName(name)) {
              x = BundleImportResolver.appendBundleClass(this, name)
            } else if (pcNames.isLambdaClassName(name)) {
              x = this.appendLambdaClass(imp.nameObj, imp.fileDescriptor, fromsym)
            } else {
              x = this.appendJava(imp.name, fromsym)
            }
            if (x != null) {
              x.mode match {
                case mk.md_sym =>
                  x.tags -= mk.importnotscanned.toUByte
                  val c2 = pcO.prjSys_getClassByName2(x.name)
                  if (c2 == null) {
                    return
                  }
                  this.addImport(c2, fromsym = true)
                case mk.md_jbc =>
                  if (fromsym) {
                    env.errors.fault(ErrMsg959, cls.name, x.name.name)
                  }
                  var n = x.out
                  while (n != null) {
                    if (n.mod.fd == null) {
                      this.createFD(n)
                    }
                    n = n.next
                  }
                  assert(!(x.tags contains mk.objs))
                  if (imp.isAnonymous) {
                    // we perform make objects on lambda creation.
                    x.tags += mk.objs.toUByte
                    x.clazz = imp
                  }
                  val nt = new mk.Node()
                  nt.mod = x
                  if (this.tail != null) {
                    this.tail.next = nt
                    this.tail = nt
                  } else {
                    this.tail = nt
                  }
              }
            }
          } else if (fromsym && l.asInstanceOf[mk.File].mode == mk.md_jbc) {
            env.errors.fault(ErrMsg959, cls.name, l.name.name)
          }
        }
      }
    }

    override def getModuleClassFD(module: mk.File, clazz: XString): xfs.FileDescriptor = {
      assert(mk.MODs contains module.mode)
      if (module.mode != mk.md_bundle) {
        super.getModuleClassFD(module, clazz)
      } else {
        BundleImportResolver.getSourceClass(this, pcNames.newBundleClassName(clazz, BundleImportResolver.getClassloaderStringIDbyBID(module.name.name)))
      }
    }

    override def resolveImport(module: mk.File, clazz: XString): mk.ImportResult = {
      assert(mk.MODs contains module.mode)
      if (module.mode != mk.md_bundle) {
        super.resolveImport(module, clazz)
      } else {
        val res = BundleImportResolver.resolveImportForBundle(this, module.name.name, clazz)
        if (res.importType == ImportResolutionType.EXTERNAL) {
          // try to find import in classpath modules
          super.resolveImport(module, clazz)
        } else res
      }
    }

    override def createFD(n: mk.Node): Unit = {
      val place = xPDB.findPlaceToWriteTo(n.mod.name.getMangledName, mk.modeToPDBType(n.mod.mode))
      n.mod.fd = place.getFileDescriptor
      assert(n.mod.fd != null)
    }
  }

  def initCompactProfileTypes(classAppender: ClassAppender): Unit = {
    val profileName = env.config.equation("profile_name")
    if (!xPDB.isProfileBuild && ((profileName == null) || !profileName.equals2("-min"))) {
      val profilePDB = xPDB.manager.profilePDB
      // regular jet (not minimal)
      // TODO: for some reason we cannot skip CompactProfilesHandler in non-java builds because
      //       jc would not copy some runtime obj files otherwise.
      //       We need to refactor this place to avoid usage of compactprofiles in non-java builds.
      val compactProfile = env.config.equation("compactprofile")
      assert(compactProfile != null && !compactProfile.equals2("AUTO"))
      profilePDB.iterateAll(xPDB.ContentType.SYM, new CompactProfilesHandler(classAppender))

      if ((languagePack == LanguagePack.JAVA)) {
        val optRTFiles = XString(ProjectLogic.optRTFiles)
        if (optRTFiles != null && !optRTFiles.isEmpty && !optRTFiles.toUpperCase.equals2("NONE")) {
          val optRTFilesSet = env.splitString(optRTFiles, ',').map(_.toUpperCase).toSet
          profilePDB.iterateAll(xPDB.ContentType.SYM, new ExtApisHandler(classAppender, optRTFilesSet))
        }

        val locales = XString(O2Env.env.valueOfOrNull(Locales))
        if (locales != null && !locales.isEmpty && !locales.toUpperCase.equals2("NONE")) {
          val allLocalesSet = env.splitString(Properties.getJCProperty("localeList"), ';').toSet
          val all = locales.toUpperCase.equals2("ALL")
          val localesSet = if (all) allLocalesSet else {
            env.splitString(locales, ',').map(_.toUpperCase).toSet
          }
          if (!localesSet.subsetOf(allLocalesSet)) {
            val badLocale = localesSet.find(!allLocalesSet(_)).get
            env.errors.fault(ErrMsg517, badLocale)
          }
          val includes = localesSet flatMap { locale => env.splitString(Properties.getJCPropertyS(locale), ';') }
          profilePDB.iterateAll(xPDB.ContentType.SYM, new LocalesHandler(classAppender, all, includes))
        }
      }
    }
  }

  def initCompactProfiles(): Unit = {
    var compactProfile = env.config.equation("compactProfile")
    if (compactProfile == null || compactProfile.equals2("AUTO")) {
      compactProfile = XString(O2Env.env.valueOfOrNull(DefaultCompactProfile))
      env.config.setEquation2("compactProfile", compactProfile)
    }

    val JETVMPROP: String = "jetvmprop"
    if (compactProfile != null && !compactProfile.toUpperCase.equals2("FULL")) {
      // set -Djet.compact.profile for runtime
      val jetvmprop = env.config.equation(JETVMPROP)
      if (jetvmprop == null || jetvmprop.isEmpty) {
        env.config.setEquation2(JETVMPROP, js.format("-Djet.compact.profile=%S", compactProfile))
      } else {
        env.config.setEquation2(JETVMPROP, js.format("%S -Djet.compact.profile=%S", jetvmprop, compactProfile))
      }
    }
  }

  //--------------- ResourceCleanupAdviser -----------------------------

  private class ResourceCleanupAdviser extends xPDB.ResourceCleanupAdviser {

    private[xcMainModule] var p: Project = _

    override def isResourceReusable(placename: XString): Boolean = {
      val m = this.findProjectFileByResource(placename, xPDB.getTypeByExt(placename))
      m != null && xcComp.isCompilable(m) == mk.none
    }

    override def isResourceAlive(placename: XString): Boolean = {
      val contentType = xPDB.getTypeByExt(placename)
      var m = this.findProjectFileByResource(placename, contentType)
      if (m != null) {
        if (xPDB.parsingStageCompilerArtifacts contains contentType) {
          true
        } else {
          // Handle super absent classes.
          // We should cleanup files generated on codegen stage (obj, irb, irei, cho, mbi)
          // for super absents but not files generated by parsing stage (sym, mod).
          // mk.redundant is set to md_sym and md_obj for super absents but not for md_jbc,
          // md_obj is not created for platform classes in global mode,
          // so we need to find md_sym here to check.
          m = this.p.getProjectFile(m.name, mk.SetOfModes.of(mk.md_sym))
          assert(m != null)
          !(m.tags contains mk.redundant)
        }
      } else {
        contentType == xPDB.ContentType.CACHEDOBJ // cached objs are always accumulating and thus alive
      }
    }

    def findProjectFileByResource(placename: XString, contentType: xPDB.ContentType): mk.File = {
      if (!(xPDB.cleanableContents contains contentType)) {
        return null
      }
      val nameInPrj = placeToNameInProject(xPDB.getNameByPlaceName(placename), contentType)
      this.p.getProjectFile(nameInPrj, contentTypeToModes(contentType))
    }

  }

  // Compilation modes for IsRedundant procedure
  type CompilationModeForIsRedundant = UByte
  private val SPECIAL: CompilationModeForIsRedundant = UByte(0)
  private val REGULAR: CompilationModeForIsRedundant = UByte(1)


  private class BundleResolverClassloaderIDGetter extends pcO.ClassloaderIDGetter {
    override def getID(clazz: pcO.Class): Int = {
      val name = clazz.nameObj
      if (pcNames.isBundleClassName(name)) {
        BundleImportResolver.getClassloaderIDByName(name)
      } else {
        super.getID(clazz)
      }
    }
  }


  private class ProjectIteratorByAdvance extends CompilationDriver.ProjectIterator {

    private[xcMainModule] var nextClass: mk.File = _

    override def next(): XString = {
      this.advance()
      val nextClass = this.nextClass
      this.nextClass = null
      nextClass.getStringID
    }

    override def hasNext: Boolean = {
      this.advance()
      this.nextClass != null
    }

    // abstract
    def advance(): Unit = {
      throw new AssertionError
    }

  }


  private class SpecialProjectIterator extends ProjectIteratorByAdvance {
    private[xcMainModule] var it: Iterator[pcO.Class] = _
    private[xcMainModule] var project: Project = _
    private[xcMainModule] var goc: XString = _
    private[xcMainModule] var supportsJava: Boolean = _

    override def advance(): Unit = {
      if (this.nextClass != null) {
        return
      }

      while (this.it.hasNext) {
        val c = this.it.next()
        val x = this.project.getProjectFile(c.nameObj, mk.SetOfModes.of(mk.md_jbc))
        if (x != null && ((x.tags contains mk.isCompilable) || c.requiredRecompilation && c.isInCompilationSet) && (this.goc == null || c.name.equals(this.goc)) && !isRedundant(c, SPECIAL) && (this.supportsJava || c.isNoJavaClass)) {
          x.clazz = c
          this.nextClass = x
          return
        }
      }
    }
  }


  private class RegularProjectIterator extends ProjectIteratorByAdvance {

    private[xcMainModule] var project: Project = _
    private[xcMainModule] var curClassId: Int = _
    private[xcMainModule] var goc: XString = _
    private[xcMainModule] var useRTCache: Boolean = _
    private[xcMainModule] var supportsJava: Boolean = _

    override def advance(): Unit = {
      if (this.nextClass != null) {
        return
      }

      var i = this.curClassId

      while (this.nextClass == null && i >= 0) {
        val c = pcO.getClassRecord(i)
        if ((this.goc == null || c.name.equals(this.goc)) && !isRedundant(c, REGULAR)) {
          var x = this.project.getProjectFile(c.nameObj, mk.SetOfModes.of(mk.md_jbc))
          if (x == null) {
            if ((this.supportsJava || (languagePack == LanguagePack.SCALA && c.isNoJavaClass)) && c.requiredRecompilation) {
              x = this.project.getProjectFile(c.nameObj, mk.SetOfModes.of(mk.md_sym))
              assert(x != null)
              x.clazz = c
              this.nextClass = x
            }
          } else if ((x.tags contains mk.isCompilable) || c.requiredRecompilation) {
            this.nextClass = x
          }
        }
        i -= 1
      }
      this.curClassId = i
    }

  }


  private class CompilationActor extends CompilationDriver.CompilationActor {
    private[xcMainModule] var project: Project = _
    private[xcMainModule] var boc: XString = _
    private[xcMainModule] var stage: Pass = _
    private[xcMainModule] var recompileRuntime: Boolean = _

    override def getErrorMessage: XString = env.errors.lastError

    override def compile(cuId: XString): Boolean = {
      var c: pcO.Class = null

      val x = this.project.getProjectFileByStringID(cuId)
      assert(x != null)

      var context: env.Context = null
      if (x.mode == mk.md_sym) {
        c = pcO.findClassByNameObject(x.name)
        if (this.recompileRuntime) {
          // use all set options and equations for runtime recompilation
          context = null
        } else {
          context = runtimeContext
        }
        if (!c.requiredRecompilation) {
          c.markAsRuntimeReusable()
        }
      } else {
        c = x.clazz
        context = x.context
      }

      env.config.setContext(context)
      if (stage != Pass.Backend) {
        xcMainModule.startCompile(x.getFileName, 0)
      }
      if (!generateModule(project, c, stage, boc)) {
        return false
      }
      env.config.removeContext(context)
      true
    }

    override def startCompile(cuId: XString, worker: Int): Unit = {
      val x = this.project.getProjectFileByStringID(cuId)
      xcMainModule.startCompile(x.getFileName, worker)
    }

  }


  class ProjectIteratorFactory {

    // abstract
    def newProjectIterator(): CompilationDriver.ProjectIterator = {
      throw new AssertionError
    }

  }


  private class RegularProjectIteratorFactory extends ProjectIteratorFactory {

    private[xcMainModule] var project: Project = _
    private[xcMainModule] var useRTCache: Boolean = _

    override def newProjectIterator(): CompilationDriver.ProjectIterator = newRegularCompilationIterator(this.project, this.useRTCache)

  }


  private class SpecialProjectIteratorFactory extends ProjectIteratorFactory {

    private[xcMainModule] var project: Project = _

    override def newProjectIterator(): CompilationDriver.ProjectIterator = newSpecialCompilationIterator(this.project)

  }

  trait ClassAppender {
    def findClassAndAppend(clazz: XString): Unit
  }


  private class CompactProfilesHandler(appender: ClassAppender) extends (XString => Unit) {
    override def apply(name: XString): Unit = {
      val clazz = pcNames.demangleJavaName(name)
      if (mk.javaseExcludeTypes.contains(clazz)) {
        return
      }
      val cname = pcNames.newClassName(clazz)
      val ignoredTags = pcO.XOTAG_SET.of(pcO.xot_extension_classloader, pcO.xot_locale)
      pcO.getOptComponentsInfo(cname) match {
        case Some((_, tags)) if (tags & ignoredTags) == pcO.XOTAG_SET.empty && isIncluded(clazz) =>
          appender.findClassAndAppend(clazz)
        case _ =>
      }
    }

    private def isIncluded(clazz: XString): Boolean = {
      var p = clazz
      infiniteLoop {
        val lastslash = p.lastIndexOf('/')
        if (lastslash == -1) {
          return false
        }
        p = p.substring(0, lastslash)
        if (mk.javaseIncludedPackages.contains(p)) {
          return true
        } else if (mk.javaseExcludedPackages != null && mk.javaseExcludedPackages.contains(p)) {
          return false
        }
      }
    }
  }

  private class ExtApisHandler(appender: ClassAppender, extApis: Set[XString]) extends (XString => Unit) {
    override def apply(name: XString): Unit = {
      val class0 = pcNames.demangleJavaName(name)
      val cname = pcNames.newClassName(class0)
      pcO.getOptComponentsInfo(cname) match {
        case Some((extName, tags)) if (tags contains pcO.xot_extension_classloader) && (extApis contains extName) =>
          appender.findClassAndAppend(class0)
        case _ =>
      }
    }
  }

  private class LocalesHandler(appender: ClassAppender, all: Boolean, includes: Set[XString]) extends (XString => Unit) {
    override def apply(name: XString): Unit = {
      val class0 = pcNames.demangleJavaName(name)
      val cname = pcNames.newClassName(class0)
      pcO.getOptComponentsInfo(cname) match {
        case Some((_, tags)) if tags contains pcO.xot_locale =>
          if (all) {
            assert(this contains class0) // sanity check
            appender.findClassAndAppend(class0)
          } else if (this contains class0) {
            appender.findClassAndAppend(class0)
          }
        case _ =>
      }
    }

    private def contains(clazz: XString) = includes(clazz) || {
      val lastslash = clazz.lastIndexOf('/')
      (lastslash != -1) && includes(clazz.substring(0, lastslash))
    }
  }

  /*----------------------------------------------------------------*/

  private val msg_no_dllname = ErrMsg464
  private val msg_no_main = ErrMsg469
  private val msg_deprecated_and_removed = ErrMsg671

  private val Javas: mk.SetOfModes = mk.SetOfModes.of(mk.md_sym, mk.md_jbc)
  private var runtimeContext: env.Context = _
  var starttime: UInt = _ /* time */

  val jstrJavaPackage: XString = js.newJString("java/")
  private val WRONG_USG_FILE: Int = 7
  private val jstrUsgVersionHeader: XString = js.newJString("JET Usage List, v ")

  def declareOptions2(): Unit = {
    env.config.registerEquation(new opt.DeniedEquation("MKFNAME", ErrMsg263, "MKFNAME"))
    env.config.registerEquation(new opt.FileEquation("PRJ", Unchecked))
  }

  //------------- Preprocessor method implementation ---------------------

  private def processJProfUSGEntry(p: Project, usgEntry: JProfManager.USGDataEntry): Unit = {
    if (usgEntry.name.isEmpty) {
      return
    }

    val st = strtok.newStringTokenizer(usgEntry.name, " ")
    if (!st.hasMoreTokens) {
      return
    }

    var s = st.nextToken()

    if (s.equals2(".nativelibrary")) {
      if (!st.hasMoreTokens) {
        return
      }
      s = st.nextToken()
    } else {
      p.findClassAndAppend(usgExtractClassName(s), doError = false)
      // jira JET-510: can not reproduce the user's problem of incorrect writing class references to .usg
      // so ignore not-found entries
    }
  }

  private def JProfReadUse(p: Project): Unit = {
    if (JProf.manager == null) {
      return
    }

    val usgData = JProf.manager.getUSGDataEntries

    for (i <- usgData.indices) {
      processJProfUSGEntry(p, usgData(i))
    }
  }

  private def mangleBlameEntry(e: JProfManager.BlameDataEntry): XString = {
    if (e.classLoaderSID == null || e.classLoaderSID.isEmpty) {
      return e.className
    }

    val sb = new js.StringBuffer()
    sb.appendf("%S%c%S", e.classLoaderSID, pcNames.CLASS_LOADER_ID_SEPARATOR, e.className)
    sb.toJString
  }

  private def addClassesFromExecutionProfileToCompilationSet(p: Project): Unit = {
    if (O2Env.env.enabled(PGO)) {
      val blameData = JProf.manager.getClassesFromExecutionProfile
      for (i <- blameData.indices) {
        p.findClassAndAppend(mangleBlameEntry(blameData(i)), doError = false)
      }
    }
  }

  private def placeToNameInProject(name: XString, contentType: xPDB.ContentType): pcNames.NAME = {
    import xPDB.ContentType as CT

    (contentType: @unchecked) match {
      case CT.SYM |
           CT.OBJ |
           CT.DBG =>
        pcNames.demangleClassName(name)
      case CT.IRB |
           CT.IREI =>
        val fileSep = FS.getFileSepForString(name)
        val cname = name.substring(0, name.lastIndexOf(fileSep)).replace(fileSep, '/')
        pcNames.demangleClassName(cname)
      case CT.MOD =>
        pcNames.fromStringID(name)
    }
  }

  private def contentTypeToModes(contentType: xPDB.ContentType): mk.SetOfModes = {
    import xPDB.ContentType as CT

    (contentType: @unchecked) match {
      case CT.SYM |
           CT.OBJ |
           CT.DBG |
           CT.IRB |
           CT.IREI =>
        mk.SetOfModes.of(mk.md_jbc)
      case CT.MOD =>
        mk.MODs
    }
  }

  private def newResourceCleanupAdviser(p: Project): xPDB.ResourceCleanupAdviser = {
    assert(p != null)
    val cleanupAdviser = new ResourceCleanupAdviser()
    cleanupAdviser.p = p
    cleanupAdviser
  }

  private def enterModule(x: mk.File): Unit =
    env.config.setContext(x.context)

  private def exitModule(x: mk.File): Unit =
    env.config.removeContext(x.context)

  private def isRedundant(cls: pcO.Class, compMode: CompilationModeForIsRedundant): Boolean = {
    compMode match {
      case SPECIAL =>
        cls.isUnavailable
      case REGULAR =>
        !cls.isCompilable
    }
  }

  private def markRedundantFileDesc(p: Project, compMode: CompilationModeForIsRedundant): Unit = {
    var fd = p.list
    while (fd != null) {
      if (fd.mode == mk.md_sym || fd.mode == mk.md_obj) {
        val cls = pcO.findClassByNameObject(fd.name)
        if (cls == null || isRedundant(cls, compMode)) {
          fd.tags += mk.redundant.toUByte
        }
      }
      fd = fd.next
    }
  }


  //--------------------------------------------------------------------
  // Auto parallelism

  def autoParallelism(): Int = {
    // Every JC job is a separate process which spawns VM threads besides the application.
    // So we cannot set parallelism to the number of cores directly as it will cripple the host machine badly.
    //
    // The maximum acceptable parallelism auto-value depends on cores number and RAM amount and it is calculated
    // in a way so it utilizes all cores or whole RAM effectively.
    // The minimal parallelism auto-value is 2 because we are asked for parallelism so let's give something at least.
    //
    // Currently: take all resources without checking of their availability.

    val CORES_PER_JOB = 3
    val RAM_GB_PER_JOB = 3
    val GB_MULTIPLIER = 1024 * 1024 * 1024

    val MIN_PARALLELISM = 2 // if we are asked for parallelism then let's set at least 2

    val totalCores = Management.get.getTotalCores

    val totalRamGB = (Management.get.getTotalPhysicalMemorySize / GB_MULTIPLIER).toInt

    val maxParallelism = Math.max(
      Math.min(totalCores / CORES_PER_JOB, totalRamGB / RAM_GB_PER_JOB) + 1, // +1 to squeeze the host machine
      MIN_PARALLELISM
    )

    env.info.print("\\nAuto-setting parallelism to %d\\n", maxParallelism)
    maxParallelism
  }

  def initParallelism(): Int = {
    val NO_PARALLELISM_VAL = 1
    val parallelismRawVal = env.config.equation("parallelism")

    val parallelism = if (parallelismRawVal == null) {
      NO_PARALLELISM_VAL
    } else {
      parallelismRawVal.toString match {
        case "nice" | "greedy" | "max" => autoParallelism()
        case _ => js.parseIntOrElse(parallelismRawVal, -1)
      }
    }

    if (parallelism <= 0) {
      env.errors.fault(ErrMsg483, parallelismRawVal)
    }

    env.config.setEquation("parallelism", parallelism.toString)
    parallelism
  }

  //--------------------------------------------------------------------
  // Class list writers
  private def reportAbsentClasses(): Unit = {
    var was_header = false
    for (c <- pcO.allClasses) {
      // We should not add warning for absent sym import classes in list of absent classes, because the warning was
      // shown in a time of making that component and does not relate to the compilation of current component.
      if (c.isAbsent && !c.isAbsentSymImport) {
        if (!was_header) {
          was_header = true
          env.info.print("\\nList of absent classes:\\n")
        }
        env.info.print("  %S\\n", c.name)
      }
    }
  }

  private def reportAbsentSupers(): Unit = {
    var was_header = false
    for (c <- pcO.allClasses) {
      if (c.hasAbsentSuper) {
        val super0 = c.getAbsentSuper
        assert(super0.isUnavailable)
        if (!was_header) {
          was_header = true
          env.info.print("\\nList of classes with absent superclass or superinterface:\\n")
        }
        env.info.print("  %S ", c.name)
        if (super0.isInterface) {
          env.info.print("\\n\\tsuperinterface %S\\n", super0.name)
        } else {
          env.info.print("\\n\\tsuperclass %S\\n", super0.name)
        }
      }
    }
  }

  private def reportNotVerifiableClasses(): Unit = {
    var was_header = false
    for (c <- pcO.allClasses) {
      if (!c.isVerifiable && !c.isJetRuntimeClass) {
        if (!was_header) {
          was_header = true
          env.info.print("\\nList of not verifiable classes:\\n")
        }
        val vererr = c.getVerifyError
        env.info.print("  %S:\\n\\tthrows %s: %S\\n", c.name, vererr.errcode.toString, vererr.errmsg)
      }
    }
  }

  def initOutputName(fatal: Boolean = true): Unit = {
    var outputname = env.config.equation("outputname")
    if (outputname == null) {
      val dllname = env.config.equation("dllname")
      val project = env.config.equation("project")
      val main = env.config.equation("main")
      val inputmod = env.config.equation("inputmod")
      val gendll = env.config.option("gendll")
      if (gendll && dllname != null && dllname.nonEmpty) {
        env.config.setEquation2("outputname", dllname)
      } else if (project != null && project.nonEmpty) {
        env.config.setEquation2("outputname", project)
      } else if (main != null && main.nonEmpty) {
        env.config.setEquation2("outputname", FS.getBaseName(main))
      } else if (inputmod != null && inputmod.nonEmpty) {
        env.config.setEquation2("outputname", FS.getBaseName(inputmod))
      } else if (fatal) {
        env.errors.fault(msg_no_main)
      } else {
        return
      }
    }
    outputname = env.config.equation("outputname")
    assert(outputname != null)
    TimeRec.setModuleName(outputname)
  }

  private def setRedirection(outputdir: XString, regexp: String): Unit = {
    val lookup = js.format("%s=%S", regexp, outputdir)
    xfs.sys.parseRed(lookup)
  }

  private def initOutputDir(): Unit = {
    val TOMCAT_OUTDIR: String = "tomcat-bin"
    var outputdir: XString = null

    var appdir = env.config.equation("appdir")

    if (pcO.isTomcat || pcO.isIdea) {
      // JET-4775, JET-7426 fix
      assert(appdir != null)
      appdir = FS.normalizeFileName(appdir)
      env.config.setEquation2("appdir", appdir)
    }

    val pdbdir = xPDB.manager.mainPDB.rootDir

    if (pcO.isTomcat) {
      outputdir = FS.addPath(pdbdir, js.newJString(TOMCAT_OUTDIR))
      env.config.setEquation2("outputdir", outputdir)
    } else if (pcO.isIdea) {
      env.config.setEquation2("outputdir", pdbdir)
    } else {
      outputdir = env.config.equation("outputdir")
      if (outputdir == null || outputdir.isEmpty) {
        outputdir = appdir
        env.config.setEquation2("outputdir", outputdir)
      } else {
        appdir = outputdir
        env.config.setEquation2("appdir", appdir)
      }

      if (outputdir == null || outputdir.isEmpty) {
        return
      }
    }

    outputdir = FS.HOST.fromPlatform(outputdir)
    if (!Dirs.mkdirs(outputdir) || !xfs.sys.exists(outputdir)) {
      env.errors.fault(ErrMsg476, outputdir)
    }

    if (env.config.option("gendll")) {
      setRedirection(outputdir, js.TODO2(js.format("%s%s", "*.", FS.TARGET.dllExtension)))
    } else if (O2Env.env.enabled(GenMegaObj)) {
      val mobjext = env.config.equation("mobjext")
      setRedirection(outputdir, js.TODO2(js.format("%s%s", "*.", js.TODO2(mobjext))))
    } else if (FS.TARGET.exeExtension.equals("")) {
      // there is no way to set lookup for executable for unices
      // since it has no extension. So we decided to patch
      // xmFS.UseFirst for this specific case.
    } else {
      setRedirection(outputdir, js.TODO2(js.format("%s%s", "*.", FS.TARGET.exeExtension)))
    }

    val jexpext = env.config.equation("jexpext")
    setRedirection(outputdir, js.TODO2(js.format("%s%s", "*.", js.TODO2(jexpext))))
  }

  // jet_jre_home option is used to set JREHome value in CPB
  private def initJREHome(): Unit = {
    var value = env.config.equation("jet_jre_home")
    if (value == null || value.isEmpty) {
      if (env.config.tags contains env.regularbuild) {
        if (pcO.isTomcat || env.config.option("bigdata")) {
          value = js.newJString("*{comp.dir}/../rt")
        } else {
          value = js.newJString("*{comp.dir}/rt")
        }
      } else {
        value = env.getProfileJREDir
      }
      env.config.setEquation2("jet_jre_home", FS.TARGET.toPlatform(value))
    }
  }

  private def initHeapLimit(): Unit = {
    val JETVMPROP: String = "jetvmprop"
    val HEAPLIMIT_PROPERTY: String = "-Djet.gc.heaplimit"
    val HEAPLIMIT: String = "HEAPLIMIT"

    val oldstr = env.config.equation(HEAPLIMIT)
    if (oldstr == null || oldstr.isEmpty) {
      return
    }

    val oldval = parseMemorySize(oldstr)
    if (oldval < 0) {
      throw new AssertionError // should be caught earlier
    }

    env.config.setEquation2(HEAPLIMIT, null)

    val jetvmprop = env.config.equation(JETVMPROP)
    if (jetvmprop == null || jetvmprop.equals(js.jstrEmpty)) {
      env.config.setEquation2(JETVMPROP, js.format("-Djet.gc.heaplimit=%S", oldstr))
    } else if (jetvmprop.indexOf(js.newJString(HEAPLIMIT_PROPERTY)) < 0) {
      env.config.setEquation2(JETVMPROP, js.format("%S -Djet.gc.heaplimit=%S", jetvmprop, oldstr))
    }
  }

  private def isNextJavaVersionClass(class0: pcO.Class): Boolean = {
    if (!class0.isVerifiable) {
      val vererr = class0.getVerifyError
      if (vererr.errcode == UnsupportedClassVersionError) {
        val fd = class0.fileDescriptor
        if (fd != null) {
          val file = fd.openSymFile()
          try {
            return jcp.loadHead(file) && jcp.c.versionMajor == (jcp.MaxSupportedVersion + 1).toUShort
          } finally file.close()
        }
      }
    }
    false
  }

  private def isMainOptionSpecified(mainStr: XString): Boolean = mainStr != null && mainStr.nonEmpty

  private def checkDllName(): Boolean = {
    if (env.config.option("gendll")) {
      var name = env.config.equation("outputname")
      if (name == null || name.isEmpty) {
        name = env.config.equation("dllname")
        if (name == null || name.isEmpty) {
          name = env.config.equation("PROJECT")
          if (name == null || name.isEmpty) {
            name = env.config.equation("PRJ")
            if (name == null || name.isEmpty) {
              env.errors.envError(msg_no_dllname)
              return false
            }
          }
        }
      }
      env.config.setEquation2("dllname", name)
    }
    true
  }

  private def reportFuzzies(): Unit = {
    reportAbsentClasses()
    reportAbsentSupers()
    reportNotVerifiableClasses()
  }

  private def initClassAmount(classAmount: Int): Unit = {
    xmErrors.classAmount = classAmount
    xmErrors.backendCounter = xmErrors.classAmount + 1
  }

  private def newSpecialCompilationIterator(p: Project): SpecialProjectIterator = {
    val this0 = new SpecialProjectIterator()
    this0.project = p
    this0.it = pcO.allClasses
    this0.goc = env.config.equation("gen_only_class")
    this0.supportsJava = languagePack.supports(JAVA)
    this0
  }

  private def checkSplashImage(): Unit = {
    val image = env.config.equation("SPLASH")
    if (image == null) {
      return
    }

    val ext = FS.getExt(image).toUpperCase
    if (ext.equals2("BMP")) {
      env.errors.fault(ErrMsg355, image)
    }
  }

  private def checkRemovedEquation(eqname: String, msg: String): Unit = {
    val eq0 = env.config.equation(eqname)
    if (eq0 != null && eq0.nonEmpty) {
      env.errors.envError(msg_deprecated_and_removed, msg)
    }
  }

  private def checkRemovedOption(optname: String, okvalue: Boolean, msg: String): Unit = {
    if (env.config.option(optname) != okvalue) {
      env.errors.envError(msg_deprecated_and_removed, msg)
    }
  }

  private def checkNoSplashExtensionOptions(): Boolean = {
    checkRemovedOption(s"${BoolOption.SplashCloseOnAWTWindow}", okvalue = true, "advanced splash settings -- splash close on non-AWT window")
    checkRemovedEquation("splashcloseontitle", "advanced splash settings -- splash close on a specific title")
    checkRemovedEquation("splashmintime", "advanced splash settings -- splash minimum time")
    checkRemovedOption("splashcloseonclick", okvalue = false, "advanced splash settings -- splash close on click")
    !env.errors.envErrDetected
  }

  private def middleMessage(): Unit = {
    env.info.print("\\n------------------------  Middle Stage  ----------------------------------\\n\\n")
  }

  private def codeGenMessage(): Unit = {
    env.info.print("\\n------------------------  Codegen Stage  ---------------------------------------\\n\\n")
  }

  private def startCompile(filename: XString, worker: Int): Unit = {
    xmErrors.backendCounter -= 1
    env.info.filename = filename
    env.info.worker = worker
    env.info.header()
  }

  private def readPGORecompilationSet(pgoSetPDBName: XString): Unit = {
    val place = xPDB.manager.mainPDB.findPlaceToReadFrom(pgoSetPDBName, xPDB.ContentType.SET)
    if (place != null) {
      val pgoSetFile = place.openAsSymForRead()
      loop {
        val name = pcO.readInternalizableName(pgoSetFile)
        if (name == null) {
          break()
        } else {
          val c = pcO.findClassByNameObject(name)
          if (c != null) {
            c.markAsRequiredRecompilation()
          }
        }
      }
      pgoSetFile.close()
    }
  }

  private def markPGORecompilationSet(jprofMan: JProfManager.JProfManager): Unit = {
    val pgoSetPDBName = js.newJString("pgo")
    readPGORecompilationSet(pgoSetPDBName)

    var pgoSetFile: xfs.SymFile = null
    if (!xcModes.workerMode) {
      val place = xPDB.findPlaceToWriteTo(pgoSetPDBName, xPDB.ContentType.SET)
      pgoSetFile = place.openAsSymForWrite()
    }
    if (O2Env.env.enabled(PGO) && jprofMan != null) {
      val optimizeJetRT = !env.config.option("NoJetRTGlobalOptim")
      val blameData = jprofMan.getOptimizedClasses
      if (blameData != null) {
        for (i <- blameData.indices) {
          val c = pcO.findClass(blameData(i).className, tryAbsent = false, blameData(i).classLoaderSID, tryLambda = true)
          if (c != null && (optimizeJetRT || !c.isJetRuntimeClass)) {
            c.markAsRequiredRecompilation()
            if (!xcModes.workerMode) {
              pcO.writeInternalizableName(c.nameObj, pgoSetFile)
            }
          }
        }
      }
    }
    if (!xcModes.workerMode) {
      pcO.writeInternalizableName(null, pgoSetFile)
      pgoSetFile.closeNew()
    }
  }

  /** For runtime reusable classes we should use stable values of options/equations
    * that affect code generation to get stable result from one compilation to another.
    */
  def saveRuntimeContext(): Unit = {
    runtimeContext = env.config.newRuntimeContext()
    for (o <- env.config.options if o.checked == AffectsCode || o.checked == RuntimeRecompile) {
      o match {
        case o: env.Option => runtimeContext.setOptionJS(o.name, o.getBooleanValue)
        case o: env.Equation => runtimeContext.setEquationJS(o.name, o.getStringValue)
      }
    }
  }

  private def findObjInCacheOrXKRN(name: XString, useXKRNobjs: Boolean, useCache: Boolean): xPDB.Placeholder = {
    var from = xPDB.findPlaceToReadFrom(name, xPDB.ContentType.OBJ, skipProfile = !useXKRNobjs)
    if (from == null && useCache) {
      from = xPDB.manager.mainPDB.findPlaceToReadFrom(name, xPDB.ContentType.CACHEDOBJ)
    }
    from
  }

  private def noObjRTClass(c: pcO.Class): Boolean = {
    if (!c.hasMetaInformation) {
      xPDB.findPlaceToReadFrom(c.getMangledName, xPDB.ContentType.OBJ) == null
    } else {
      false
    }
  }

  private def existsInCacheOrXKRN(c: pcO.Class, useXKRNobjs: Boolean, useCache: Boolean): Boolean = {
    val name = c.getMangledName
    val from = findObjInCacheOrXKRN(name, useXKRNobjs, useCache)
    if (from == null) {
      noObjRTClass(c)
    } else {
      true
    }
  }

  private def tryReuseObj(name: XString, useXKRNobjs: Boolean, useCache: Boolean): Unit = {
    val from = findObjInCacheOrXKRN(name, useXKRNobjs, useCache)
    if (from == null) {
      return
    }

    val to0 = xPDB.findPlaceToWriteTo(name, xPDB.ContentType.OBJ)
    assert(to0 != null)

    xPDB.copy(from, to0)
  }

  // Linker for now does not support merging several DWARF.obj files
  // so if reuseRtDWARF - just rewrite the DWARF.obj if it is collected due to GenDebug
  private def tryReuseDWARFObj(useXKRNobjs: Boolean, useCache: Boolean): Unit =
    tryReuseObj(XString("DWARF"), useXKRNobjs, useCache)

  private def tryReuseObj(c: pcO.Class, useXKRNobjs: Boolean, useCache: Boolean): Unit =
    tryReuseObj(c.getMangledName, useXKRNobjs, useCache)

  private def promoteRuntimeReusableToCache(c: pcO.Class): Unit = {
    val name = c.getMangledName
    val from = xPDB.manager.mainPDB.findPlaceToReadFrom(name, xPDB.ContentType.OBJ)
    if (from == null) {
      // @Inline classes may not produce obj files
      assert(noObjRTClass(c))
      return
    }

    val to0 = xPDB.findPlaceToWriteTo(name, xPDB.ContentType.CACHEDOBJ)
    assert(to0 != null)

    xPDB.copy(from, to0)
  }

  private def markRecompilationSet(p: Project, useXKRNobjs: Boolean, useCache: Boolean): Unit = {
    val jbcSet = mk.SetOfModes.of(mk.md_jbc)
    for (clazz <- pcO.allClasses) {
      if (!isRedundant(clazz, REGULAR)) {
        val x = p.getProjectFile(clazz.nameObj, jbcSet)
        if (x != null && !(x.tags contains mk.isCompilable) && clazz.requiredRecompilation) {
          x.tags += mk.isCompilable.toUByte
        } else if (x == null && !existsInCacheOrXKRN(clazz, useXKRNobjs, useCache)) {
          clazz.markAsRequiredRecompilation()
        }
      }
    }
  }

  private def getClassAmount(iterator: CompilationDriver.ProjectIterator): Int = {
    var amount = 0
    while (iterator.hasNext) {
      amount += 1
      iterator.next()
    }
    amount
  }

  private def shouldRecompileRuntime(): Boolean = {
    if (env.config.option("RecompileRuntime")) {
      return true
    }

    for (o <- runtimeContext.options if o.checked == RuntimeRecompile) {
      o match {
        case o: env.Equation if env.config.equationJS(o.name) != o.getStringValue => return true
        case o: env.Option if env.config.optionJS(o.name) != o.getBooleanValue => return true
        case _ =>
      }
    }
    false
  }

  private def newRegularCompilationIterator(p: Project, useRTCache: Boolean): RegularProjectIterator = {
    val this0 = new RegularProjectIterator()
    this0.project = p
    // Order of compilation is reversed for needs of opt compiler.
    this0.curClassId = pc.modules.size - 1
    this0.goc = env.config.equation("gen_only_class")
    this0.useRTCache = useRTCache
    this0.supportsJava = languagePack.supports(JAVA)
    this0
  }

  private def newCompilationActor(p: Project, stage: Pass, recompileRuntime: Boolean): CompilationActor = {
    val this0 = new CompilationActor()
    this0.project = p
    this0.stage = stage
    this0.recompileRuntime = recompileRuntime
    this0.boc = env.config.equation("back_only_class")
    this0
  }

  private def flushPDBAfterMiddleStage(): Unit = {
    xPDB.manager.mainPDB.flush()

    val midtime = env.diffTimes(starttime, env.time())

    env.info.print("\\nTime spent so far %d:%02d.%02d\\n", ((midtime / UInt(100)) / UInt(60)).toInt, ((midtime / UInt(100)) % UInt(60)).toInt, (midtime % UInt(100)).toInt)
  }

  private def newRegularProjectIteratorFactory(p: Project, useRTCache: Boolean): RegularProjectIteratorFactory = {
    val this0 = new RegularProjectIteratorFactory()
    this0.project = p
    this0.useRTCache = useRTCache
    this0
  }

  private def newSpecialProjectIteratorFactory(p: Project): SpecialProjectIteratorFactory = {
    val this0 = new SpecialProjectIteratorFactory()
    this0.project = p
    this0
  }

  private def addLookupToJREJar(jar: String): Unit = {
    setRedirection(js.format("%S/lib/%s", env.getProfileJREDir, jar), "*.class")
  }

  private def addLookupToJetRTJar(jar: String): Unit = {
    setRedirection(js.format("%S/%s", env.getJetLibDir, jar), "*.class")
  }

  // just load all import for JRE classes (make import closure)
  def loadImport(): Unit = {
    for (c <- pcO.allClasses) {
      c.ensureImportLoaded()
    }
  }

  def readRedirection(f: xfs.TextFile): Boolean = {
    val r = new ScanRed()
    xfs.sys.saveRed()
    env.config.setOption("NOINVLOOKUPS", value = true)
    val res = r.readText(f)
    env.config.setOption("NOINVLOOKUPS", value = false)
    xfs.sys.saveRed()
    res
  }

  def readConfig(f: xfs.TextFile): Boolean = {
    val s = new ScanPro()
    s.config = true
    xfs.sys.saveRed()
    s.readText(f)
  }

  private def findProject(strPar: XString): xfs.FileDescriptor = {
    var str = strPar

    if (FS.validName(str)) {
      val ext = FS.getExt(str)
      var prj = env.config.equation("PRJEXT")
      if (prj == null) {
        prj = js.newJString("prj")
      }
      if (ext.isEmpty) {
        str = FS.addExt(str, prj)
      } else if (!ext.equals(prj)) {
        env.errors.fault(ErrMsg473, str)
      }
    }
    xfs.sys.lookup(str)
  }

  private def openProject(str: XString): xfs.TextFile = {
    val fd = findProject(str)
    val f = fd.openTextFile()
    parseDecor()
    f
  }

  def endProject(p: mk.Project): Unit = {
    /** Close bracket to "UseProject" and "ReadProject" */
    assert(p != null)

    xPDB.closeAll()
    p.destroy()
    env.config.restore()
    xfs.sys.restoreRed()
    parseDecor()
  }

  private def newProject(): Project = {
    val p = new Project()
    p.init()
    p
  }

  /** Reads project if name is non-empty string.
    * If importing then reads only option part
    */
  def useProject(name: XString): Project = {
    var err = false
    env.config.save()
    xfs.sys.saveRed()
    var p = newProject()
    var s: ScanPro = null
    if (name.nonEmpty) {
      val f = openProject(name)
      if (f == null) {
        err = true
      } else {
        s = new ScanPro(p)

        p.fileName = f.getName

        p.setEquations()
        err = s.readText(f)
        f.close()
      }
    }

    if (err) {
      endProject(p)
      p = null
    } else {
      p.setEquations()
      if (s == null || !s.isPDBOpened) {
        p.openPDB()
      }
    }

    parseDecor()

    p
  }

  def readProject(name: XString): Project = O2Env.stage(Stage.ReadProject) {
    /** Reads all project. Use EndProject as close bracket. */
    var p: Project = null

    val f = openProject(name)
    if (f != null) {
      p = newProject()

      env.config.save()
      xfs.sys.saveRed()

      val s = new ScanPro(p)
      p.fileName = f.getName
      p.setEquations()
      val err = s.readText(f)
      f.close()
      if (err) {
        endProject(p)
        p = null
      } else {
        p.setEquations()
        if (s == null || !s.isPDBOpened) {
          p.openPDB()
        }
      }
      parseDecor()
    } else {
      p = null
    }
    p
  }

  /**
  There are some classes in Compact1 profile that import java/beans classes
    that should not be actually resolved for them.
    This method detects the situation.
    */
  private def isGreaterProfileImport(curClazz: pcO.Class, importClazz: XString): Boolean = {
    if (!curClazz.isSystemClass) {
      return false
    }
    val curClassProfile = mk.getCompactProfileForClass(curClazz.name)
    xPDB.isProfileBuild && curClassProfile != 0 && mk.getCompactProfileForClass(importClazz) > curClassProfile
  }

  def loadType(p: mk.Project, namePar: XString): pcO.Class = {
    import BundleImportResolver.*

    var name = namePar
    var pcname: pcNames.NAME = null

    var curmodname: pcNames.NAME = null
    var curClazz: pcO.Class = null
    if (pc.currentModule != pc.INVALID_MNO) {
      curClazz = pcO.getClassRecord(pc.currentModule)
      name = JBCPreprocessor.preprocessClassName(name, curClazz)
      if (isGreaterProfileImport(curClazz, name)) {
        return pcO.findAbsentClass(name)
      }
      curmodname = curClazz.nameObj
    }

    var impres = ImportResult(IMP_SYSTEM)
    if (curmodname != null) {
      if (pcNames.isBundleClassName(curmodname)) {
        impres = BundleImportResolver.resolveImportFor(p, curmodname, name)
      } else if (curClazz.isAnonymous) {
        if (pcNames.isBundleClassName(curClazz.hostClass.nameObj)) {
          impres = BundleImportResolver.resolveImportFor(p, curClazz.hostClass.nameObj, name)
        }
      }
    }

    if (impres.importType == IMP_NONIMPORT) {
      if (name.startsWith(jstrJavaPackage, 0)) {
        // Since classes from java.* package may be loaded by system classloader
        // only, we may safely resolve such import to rt.jar
        impres = ImportResult(IMP_SYSTEM)
      } else {
        // JET-3792: for other imports, it is safe to treat non-existsing import
        // as absent import.
        // We may add ASSERT in the future to not allow to ask for non-existing
        // import of not java/* classes.
        impres = ImportResult(IMP_ABSENT)
      }
    }

    var cls: pcO.Class = null

    impres.importType match {
      case IMP_SYSTEM =>
        pcname = pcNames.newClassName(name)
        // JET-6149: do not try to find absent classes here
        // because we can find non-absent in the project later.
        cls = pcO.findClass(name, tryAbsent = false)
      case IMP_ABSENT =>
        pcname = pcNames.newAbsentClassName(name)
        cls = pcO.findClassByNameObject(pcname)
      case IMP_EXTERNAL =>
        pcname = pcNames.newClassName(name)
        cls = pcO.findClassByNameObject(pcname)
        if (cls != null && cls.isUnavailable) {
          assert(!cls.isAbsent)
        }
      case _ =>
        assert(impres.isBundle)
        if (impres.classFile != null) {
          pcname = impres.classFile.name
          cls = pcO.findClassByNameObject(pcname)
          if (cls != null && cls.isUnavailable) {
            assert(!cls.isAbsent)
          }
        } else {
          // bundle class and bundle(internal) import
          assert(pcNames.isBundleClassName(curmodname))
          pcname = impres.className
          cls = null
        }
    }
    if (cls != null) {
      return cls
    }
    var l = mk.search(p.hashtable, pcname)
    if (l != null) {
      var y = l.asInstanceOf[mk.File]
      while (y.s != null && !(Javas contains y.mode)) {
        y = y.s.asInstanceOf[mk.File]
      }
      if (!(Javas contains y.mode)) {
        l = null
      }
    }

    val x: mk.File = impres.importType match {
      case IMP_SYSTEM | IMP_EXTERNAL => p.appendJava(name)
      case IMP_BUNDLE =>
        if (pcNames.isBundleClassName(curmodname) && impres.classFile == null) {
          // bundle class that was not loaded yet.
          // BundleImportResolver.ResolveImportFor told name of this class, but append it
          BundleImportResolver.appendBundleClass(p, impres.className)
        } else {
          impres.classFile
        }
      case _ => null
    }

    if (x != null) {
      enterModule(x)
      x.mode match {
        case mk.md_sym =>
          val imp = pcO.prjSys_getClassByName(x.name)
          assert(imp != null)
          p.addImport(imp, fromsym = true)
          exitModule(x)
          imp

        case mk.md_jbc =>
          var n = x.out
          while (n != null) {
            if (n.mod.fd == null) {
              p.createFD(n)
            }
            n = n.next
          }
          if (xcComp.isCompilable(x) != mk.compilable) {
            val imp = pcO.prjSys_getClassByName(x.name)
            assert(imp != null)
            p.addImport(imp, fromsym = false)
            exitModule(x)
            return imp
          }
          if (l == null) {
            val nt = new mk.Node()
            nt.mod = x
            if (p.tail != null) {
              p.tail.next = nt
              p.tail = nt
            } else {
              p.tail = nt
            }
          }

          val tmp = env.info.module
          val tmp2 = env.info.filename
          env.info.module = x.name
          x.clazz = Frontend.parseModule(x.fd)
          env.info.module = tmp
          env.info.filename = tmp2
          x.tags += mk.objs.toUByte
          exitModule(x)
          x.clazz
      }

    } else {
      if (impres.importType == IMP_SYSTEM) {
        // JET-6149: We did not find system class but could already add absent class.
        // So try to find absent:
        return pcO.findAbsentClass(name)
      } else if (impres.importType == IMP_EXTERNAL) {
        // we should always find external import except when it was explicitly excluded:
        assert(mk.isFromExcludedPackage(pcNames.newClassName(name)))
        return pcO.findAbsentClass(name)
      }

      null
    }
  }

  private def getExt(equation: String, defext: String): XString = {
    val eqval = env.config.equation(equation)
    if (eqval == null) {
      js.newJString(defext)
    } else {
      eqval
    }
  }

  private def openUSG(str: XString, ext: XString): xfs.TextFile = {
    val fn = js.format("%S.%S", str, ext)
    val fd = xfs.sys.lookup(fn)
    fd.openTextFile()
  }

  private def analyseFUSFile(sf: ScanFus, name: XString): Unit = {
    val ext = getExt("FUSEXT", "fus")
    val f = openUSG(name, ext)
    sf.initParse()
    val err = sf.readText(f)
    f.close()
    if (err) {
      env.exit(WRONG_USG_FILE)
    }
  }

  private def parseUsgFileVersion(usgfile: XString, verPar: XString): Int = {
    var ver = verPar

    if (ver == null || !ver.startsWith(jstrUsgVersionHeader, 0)) {
      env.errors.fault(ErrMsg603, usgfile)
    }
    ver = ver.substring(jstrUsgVersionHeader.length)
    var dot = ver.indexOf('.')
    if (dot == -1) {
      env.errors.fault(ErrMsg603, usgfile)
    }
    val veri = js.format("%S%S", ver.substring(0, dot), ver.substring(dot + 1)) // drop '.'
    dot = js.parseIntOrElse(veri, -1)
    if (dot < 0) {
      env.errors.fault(ErrMsg603, usgfile)
    }
    dot
  }

  private def analyseUSGFile(s: ScanUse, name: XString): Unit = {
    val ext = getExt("USGEXT", "usg")
    val f = openUSG(name, ext)
    if (f == null) {
      env.errors.fault(ErrMsg349, name, ext)
    }
    if (parseUsgFileVersion(name, f.readLine()) < 315) {
      env.errors.fault(ErrMsg613, name)
    }
    val err = s.readText(f)
    f.close()
    if (err) {
      env.exit(WRONG_USG_FILE)
    }
  }

  private def readUse(p: Project): Unit = {
    var tm: mk.TM = new mk.TM()

    val s = new ScanUse()
    val sf = new ScanFus()
    var n = p.nodes
    s.project = p
    sf.project = p
    while (n != null) {
      val x = n.mod
      if (x.mode == mk.md_usg) {
        analyseUSGFile(s, x.name.name)
        tm.getTime(x)
        if (p.usgTm.undef || p.usgTm.time > tm.time) {
          tm._copyTo(p.usgTm)
        }
      } else if (x.mode == mk.md_fus) {
        analyseFUSFile(sf, x.name.name)
        tm.getTime(x)
        if (p.usgTm.undef || p.usgTm.time > tm.time) {
          tm._copyTo(p.usgTm)
        }
      }
      n = n.next
    }
    val name = FS.getBaseName(env.args.programName)
    analyseUSGFile(s, name)
    analyseFUSFile(sf, name)
    val main = env.config.equation("MAIN")
    if (main != null && main.nonEmpty) {
      p.findClassAndAppend(main, doError = true)
    }
  }
}
