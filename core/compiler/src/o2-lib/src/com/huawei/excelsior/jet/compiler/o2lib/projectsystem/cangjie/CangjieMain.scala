/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.o2lib.projectsystem.cangjie

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.common.{JetDirs, Language}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker
import com.huawei.excelsior.jet.compiler.cbc.CBCFileGenerator
import com.huawei.excelsior.jet.compiler.cbc.CBCFileGenerator.GenerationTarget.{EXE, STDLIB}
import com.huawei.excelsior.jet.compiler.chir.CHIRLoader
import com.huawei.excelsior.jet.compiler.debug.dwarf.Dwarf
import com.huawei.excelsior.jet.compiler.delayed.DelayedIntrinsicsUsageTracker
import com.huawei.excelsior.jet.compiler.driver.CompilationMode.O2
import com.huawei.excelsior.jet.compiler.driver.{CompilationMode, ProjectLogic}
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env.env
import com.huawei.excelsior.jet.compiler.o2lib.opt.{O2Env, VZCModule as VZC}
import com.huawei.excelsior.jet.compiler.o2lib.be_386.opCodeModule
import com.huawei.excelsior.jet.compiler.o2lib.fe.{CHIRSymLevelBuilderImpl, CangjieSymLevelBuilder, ExtraPassModule, pc, pcNamesModule as pcNames, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.jprof.JProf
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.cangjie.Errors.{MULTIPLE_MAINS_ERROR, NO_MAIN_ERROR, WRONG_MAIN_EQUATION_ERROR, error}
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xPDBModule as xPDB
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xPDBModule.*
import com.huawei.excelsior.jet.compiler.o2lib.u.xcMainModule.{initCompactProfileTypes, initCompactProfiles, starttime}
import com.huawei.excelsior.jet.compiler.o2lib.u.xiEnvModule.isProgressShowable
import com.huawei.excelsior.jet.compiler.o2lib.u.{CompilationDriverModule, JStringsModule, ReplacementLibrary, xcFModule, xcMain0Module, xcMainModule, xcMakeModule, CacheAPIModule as CacheAPI, xiEnvModule as xiEnv, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule as FS
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.options.StrOption.LibraryName
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.{methodByO2Object, typeToO2Class}
import com.huawei.excelsior.jet.compiler.{Pass, Stage}
import xscala.io.{DataInput, DataOutput, Path}
import xscala.util.UInt

import scala.collection.mutable
import scala.util.Using

object CangjieMain {

  var cangjieStdLibCompiling: Boolean = _
  var genLibrary: Boolean = _
  var genCbcStdLibrary: Boolean = _
  var genProfileLibrary: Boolean = _
  var genDwarf: Boolean = _
  var workerMode: Boolean = _
  var classAmountToCompile: Int = _
  var compiledClasses = 0

  def initialize(): Unit = {
    workerMode = xiEnv.config.equation("worker") != null
    genLibrary = xiEnv.config.option(s"$GenLibrary")
    genCbcStdLibrary = xiEnv.config.option(s"$GenCbcStdLib")
    genProfileLibrary = xiEnv.config.option(s"$GenProfileLibrary")

    if (genProfileLibrary && !genLibrary) {
      xiEnv.errors.fault(ErrMsg528);
    }

    val dwarfIsRequested = xiEnv.config.option(s"$GenDebug") || xiEnv.config.option(s"$ReuseRtDwarf")
    genDwarf = dwarfIsRequested && targetArch != CBC
    cangjieStdLibCompiling = genProfileLibrary && xiEnv.config.equation(s"$LibraryName").toString == "CangJieStdLib"

    pcO.makeClass = { name =>
      val clazz = xiEnv.loadType(name)
      if (clazz == null) {
        xiEnv.errors.fault(ErrMsg402, xiEnv.info.module.name, name)
      }
      clazz
    }

    pcO.iniOpt()

    // currently we reuse PDB in two cases
    //  - for workers (no smart for Cangjie is implemented) and all resources used by a worker must be reusable
    //  - when compiling std library to cbc format we use and append to pdb created for core std library
    // TODO: change it after implementing smart compilation
    val reused = xPDB.openMainPDB(reuse = workerMode || genCbcStdLibrary , new ResourceCleanupAdviser() {
      override def isResourceReusable(placename: XString) = true
      override def isResourceAlive(placename: XString) = true
    })
    assert(!workerMode || reused)
    xcMakeModule.setDefaultLookups()

    if (genDwarf) {
      Dwarf.setEquationsForRSP((eq, value) => xiEnv.config.setEquation2(eq, XString.ascii(value)))
    }

    xiEnv.loadType = { name =>
      val pcname = pcNames.newClassName(name)
      var cls = pcO.findClass(name, tryAbsent = false)
      if (cls == null) {
        cls = pcO.prjSys_getClassByName(pcname)
        loadImportClosure(cls)
      }
      cls
    }

    if (!isStandalone) {
      LightweightEnvironment.checkRTConstConsistency()
      CacheAPI.loadClasses(xiEnv.loadType(_))
    }

    if (cangjieStdLibCompiling) {
      DelayedIntrinsicsUsageTracker.deserialize(name => xiEnv.loadType(XString(name)))
    }

    if (languagePack.supports(Language.JAVA)) {
      initCompactProfiles()
      initCompactProfileTypes(name => loadImportClosure(pcO.prjSys_getClassByName(pcNames.parseMangledName(name))))
    }

    JProf.initJProf()
    ReplacementLibrary.deserialize()

    xcMainModule.initParallelism()

    xiEnv.config.setOption("nortvcf", value = true)
  }

  private def loadImportClosure(cls: pcO.Class): Unit = {
    // All import loaded for CHA reasons. Workaround for JET-16615
    // TODO: enable CHA for PGO mode only and remove import closure in standard compilation mode
    if (ProjectLogic.isCHAEnabled && (ProjectLogic.compilationMode == O2)) {
      val modCnt = pc.modules.size
      val impIt = cls.getImport
      if (modCnt != pc.modules.size) {
        impIt foreach loadImportClosure
      }
    }
  }

  private def printProgress(message: XString, curItemNum: Int, maxItems: Int): Unit = {
    if (isProgressShowable) {
      xiEnv.info.print("%d/%d: %S\n", curItemNum, maxItems, message)
    }
  }

  private def isPGOForStdLibEnabled: Boolean = {
    if (O2Env.env.enabled(PGO)) {
      assert(JProf.manager != null)
    }
    O2Env.env.enabled(PGO) && !xiEnv.config.option("NoJetRTGlobalOptim")
  }

  private def markPGORecompilationSet(): Unit = {
    if (isPGOForStdLibEnabled) {
      val blameData = JProf.manager.getOptimizedClasses
      for (i <- blameData.indices) {
        val c = pcO.findClass(blameData(i).className, tryAbsent = false, blameData(i).classLoaderSID, tryLambda = true)
        lazy val isNotCbcOrCurrentCompSet = targetArch != CBC || c.isInCompilationSet // TODO-CBC: hack, prevents recompilation of std methods that can contain AJ code
        if (c != null && !c.isJetRuntimeClass && isNotCbcOrCurrentCompSet) {
          c.markAsRequiredRecompilation()
        }
      }
      classAmountToCompile = classesToCompile.size
    }
  }

  def classesToCompile: Iterator[pcO.Class] = pcO.allClassesInReversedOrder.filter { c =>
    c.isInCompilationSet ||
      (!(genCbcStdLibrary && genProfileLibrary) && DelayedIntrinsicsUsageTracker.isClassUsedDelayedIntrinsics(c.name.toString)) ||
      c.requiredRecompilation
  }

  private def generateModule(c: pcO.Class, stage: Pass): Unit = {
    if (c.isJavaAnnotatedCangjieClass) {
      // @java classes are always deferred for dynamic (CBC) compilation
      return
    }

    opCodeModule.ini()
    opCodeModule.generateModule(c, stage)
    opCodeModule.exi()
  }

  private class CompilationActor extends CompilationDriverModule.CompilationActor {
    override def getErrorMessage = xiEnv.errors.lastError

    override def compile(name: XString) = {
      val c = pcO.findClassByNameObject(pcNames.newClassName(name))
      generateModule(c, Pass.Backend)
      pcO.symCache_gc_BackEndFinishedFor(c)
      true
    }

    override def startCompile(name: XString, worker: Int): Unit = {
      val message = if (worker == 0 || workerMode) name else XString.ascii(name.toString + " sent to worker " + worker)
      printProgress(message, compiledClasses, classAmountToCompile)
      compiledClasses += 1
    }
  }

  def doCompilation(iterator: CompilationDriverModule.ProjectIterator): Boolean = {
    val actor = new CompilationActor()
    CompilationDriverModule.doCompilation(iterator, actor)
  }

  private class ProjectIterator extends CompilationDriverModule.ProjectIterator {
    private val iter = classesToCompile

    override def next(): XString = iter.next().name
    override def hasNext: Boolean = iter.hasNext
  }

  def mergePDBsFromWorkers(): Unit = {
    if (ProjectLogic.parallelismEnabled) {
      val mainPDB = xPDB.manager.mainPDB
      mainPDB.mergeFromWorkers()
      if (!genDwarf) {
        val order = classesToCompile
          .map(v => xPDB.createPlaceName(v.getMangledName, xPDB.ContentType.OBJ))
          .filter(mainPDB.hasPlace)
        mainPDB.repackByOrder(xPDB.ContentType.OBJ, order)
      }
    }
  }

  private def parsingStage(p: CangjieProject): Unit = {
    xiEnv.info.print("\\n------------------------  Parsing Stage  ---------------------------------------\\n\\n")

    if (workerMode) {
      xPDB.manager.mainPDB.iterateAll(xPDB.ContentType.SYM, { name =>
        xiEnv.loadType(pcNames.demangleJavaName(name))
      })
      return
    }

    if (isStandalone) {
      val chirBuilder = new CHIRSymLevelBuilderImpl
      for ((f, i) <- p.files.zipWithIndex) {
        printProgress(f, i, p.files.size)
        val src = xfs.sys.createFileDescriptor(f)
        if (src.getName.toString.endsWith(".chir")) {
          chirBuilder.srcFD = src
          CHIRLoader.load(chirBuilder, src.getName.toString)
        } else {
          shouldNotReachHere(s"Unknown input file format: ${src.getName}")
        }
      }
      chirBuilder.build()

    } else {
      for ((f, i) <- p.files.zipWithIndex) {
        printProgress(f, i, p.files.size)
        val src = xfs.sys.createFileDescriptor(f)
        if (src.getName.toString.endsWith(".bc")) {
          typeToO2Class(CangjieSymLevelMaker.makeSymLevel(new CangjieSymLevelBuilder(src)))
        } else {
          shouldNotReachHere(s"Unknown input file format: ${src.getName}")
        }
      }
    }

    ExtraPassModule.exi()

    if (!genLibrary && !genCbcStdLibrary) {
      checkMain()
    }

    xPDB.manager.mainPDB.flush()
  }


  private def middleStage(): Unit = {
    if (workerMode || !ProjectLogic.useMiddleStage) {
      return
    }

    xiEnv.info.print("\\n------------------------  Middle Stage  ---------------------------------------\\n\\n")

    var i = 0
    for (c <- classesToCompile) {
      printProgress(c.name, i, classAmountToCompile)
      generateModule(c, Pass.Middle)
      i += 1
    }

    ReplacementLibrary.serialize()
    xPDB.manager.mainPDB.flush()

    val midtime = xiEnv.diffTimes(starttime, xiEnv.time())
    xiEnv.info.print("\\nTime spent so far %d:%02d.%02d\\n", ((midtime / UInt(100)) / UInt(60)).toInt, ((midtime / UInt(100)) % UInt(60)).toInt, (midtime % UInt(100)).toInt)
  }

  var coldStrings: collection.Map[String, Int] = _

  private def readColdStrings(): collection.Map[String, Int] = {
    val coldStringsInput = env.pdb.getFile(CBCFileGenerator.coldStringsForWorkersOut)

    if (coldStringsInput.exists) {
      val index = new mutable.HashMap[String, Int]

      Using.resource(DataInput.from(coldStringsInput)) { in =>
        val size = in.getW32()
        for (_ <- 0 until size) {
          val s = in.getUTF()
          val i = in.getW32()
          index(s) = i
        }
      }
      index
    } else {
      null
    }
  }

  private def codeGenStage(): Unit = {
    xiEnv.info.print("\\n------------------------  Codegen Stage  ---------------------------------------\\n\\n")

    if (workerMode) {
      coldStrings = readColdStrings()
      this.startWorker(regular = true)
      return
    }

    markPGORecompilationSet()

    if (!this.doCompilation(new ProjectIterator())) {
      return
    }

    mergePDBsFromWorkers()

    if (isPGOForStdLibEnabled) {
      // copy all obj files from profile and stdlib to main pdb
      val main = xPDB.manager.mainPDB
      val library = xPDB.manager.libraryPDB
      val profile = xPDB.manager.profilePDB
      library.iterateAll(xPDB.ContentType.OBJ, new ObjFilesCopier(library, main))
      profile.iterateAll(xPDB.ContentType.OBJ, new ObjFilesCopier(profile, main))
    }
  }

  private class ObjFilesConcatenator(pdb: xPDB.PDB, out: xfs.RawFile, passedEntries: mutable.HashSet[XString]) extends (XString => Unit) {
    override def apply(name: XString): Unit = {
      if (!passedEntries.contains(name)) {
        xfs.copy(pdb.findPlaceToReadFrom(name, xPDB.ContentType.OBJ).openAsRawForRead(), out, closeTo = false)
        passedEntries += name
      }
    }
  }

  private class ObjFilesCopier(fromPDB: xPDB.PDB, toPDB: xPDB.PDB) extends (XString => Unit) {
    override def apply(name: XString): Unit = {
      if (toPDB.findPlaceToReadFrom(name,xPDB.ContentType.OBJ) == null) {
        xfs.copy(fromPDB.findPlaceToReadFrom(name, xPDB.ContentType.OBJ).openAsRawForRead(),
          toPDB.findPlaceToWriteTo(name, xPDB.ContentType.OBJ).openAsRawForWrite())
      }
    }
  }

  private def concatObjFiles(): Unit = {
    val main = xPDB.manager.mainPDB
    val profile = xPDB.manager.profilePDB
    val out = main.findPlaceToWriteTo(XString("full"), xPDB.ContentType.OBJ_LIB).openAsRawForWrite()
    val passedEntries = mutable.HashSet[XString]()
    val objConactenator = new ObjFilesConcatenator(main, out, passedEntries)
    main.iterateAll(xPDB.ContentType.OBJ, objConactenator)
    val profileObjConactenator = new ObjFilesConcatenator(profile, out, passedEntries)
    profile.iterateAll(xPDB.ContentType.OBJ, profileObjConactenator)
    out.closeNew()
  }

  private def checkMain(): Unit = {
    var mainModule: XString = null
    var mainModuleFile: XString = null

    for (c <- classesToCompile) {
      if (c.hasMain) {
        val fileName = c.fileDescriptor.getName
        if (mainModule != null) {
          error(MULTIPLE_MAINS_ERROR, mainModuleFile, fileName)
        }
        mainModule = c.name
        mainModuleFile = fileName
      }
    }

    if (mainModule == null) {
      error(NO_MAIN_ERROR)
    }

    val mainEq = xiEnv.config.equation("main")
    if (mainEq == null || mainEq.isEmpty) {
      xiEnv.config.setEquation2("main", mainModule)
    } else if (!mainEq.equals(mainModule)) {
      error(WRONG_MAIN_EQUATION_ERROR, mainEq)
    }
  }

  private def setOutputName(): Unit = {
    val outputname = xiEnv.config.equation("outputname")
    if (outputname == null) {
      val lastArg = FS.cutExt(xiEnv.args.getArg(xiEnv.args.number() - 1))
      xiEnv.config.setEquation2("outputname", lastArg)
    }
  }

  private def link(): Unit = {
    // Project is needed by xcF (.rsp generator) to iterate over project items
    // There is no .bc iterators in jc.tem so it is safe
    // to pass just empty fake project to xcF
    val fakeProject = new xcMainModule.Project
    xcFModule.makeProject(fakeProject)

    xcMain0Module.compilationExit()
    xPDB.closeAll()

    var link = xiEnv.config.equation("LINK")
    if (link != null && link.nonEmpty) {
      link = link.concat(XString(s" \"@${xiEnv.config.equation("RSPFILENAME")}\""))
      xcFModule.runLinker(link)
    }
  }

  private def startWorker(regular: Boolean): Unit = {
    val actor = new CompilationActor()
    CompilationDriverModule.startWorker(actor)
    VZC.compiler.printFinalStatistics()
  }

  private def getOutputCbcPath: Path = {
    Path(xiEnv.config.equation("outputname").toString + ".cbc")
  }

  private def getMetaCbcPath: Path = {
    val outputName = xiEnv.config.equation("outputname").toString
    env.pdb.getFile(Path(outputName).name + ".cbc")
  }

  def checkMains(): Unit = {
    if (!xiEnv.config.option("GENDLL") && !O2Env.env.enabled(GenLibrary)) {
      for (cls <- pcO.allClasses if cls.hasMain) {
        val mainMethodIndex = methodByO2Object(cls.declaredMethods.find(_.isMainMethod).get).getHostedIndex
        xiEnv.config.setEquation2("MainMethodIndex", JStringsModule.format("%d", mainMethodIndex))
      }
    }
  }

  private def writeOrReadFileDescriptors(): Unit = {
    if (ProjectLogic.parallelismEnabled) {
      if (workerMode) {
        Using(DataInput.from(env.pdb.getFile(FileDescriptors.fileDescriptorName), buffered = true)) { in =>
          FileDescriptors.deserialize(in, env.getTypeProvider)
        }
      } else {
        Using(DataOutput.from(env.pdb.getFile(FileDescriptors.fileDescriptorName))) { out =>
          FileDescriptors.serialize(out, env.getTypeProvider)
        }
      }
    }
  }

  private def genAOTReflectionInfo(): Unit = {
    if (targetArch != CBC) {
      if (xiEnv.config.option("GenAOTReflectionInfo") && !workerMode && isDynamicBundle) {
        val stdlibCbcPath = JetDirs.jetHome / "profile/develop/lib/lres/stdlib.cbc"
        xiEnv.config.setEquation("stdlibCbcPath", stdlibCbcPath.toString)
        val cbcPath = if (cangjieStdLibCompiling) {
          stdlibCbcPath
        } else {
          val metaCbcPath = getMetaCbcPath
          xiEnv.config.setEquation("metaCbcPath", metaCbcPath.toString)
          metaCbcPath
        }
        if (isProgressShowable) {
          xiEnv.info.print("%s", s"Generating AOT metadata file (stdlib = $cangjieStdLibCompiling): $cbcPath\n")
        }

        // Gather cold strings and other meta-info into .cbc file before AOT codeGenStage.
        CBCFileGenerator.generate(cbcPath, generationTarget = if (cangjieStdLibCompiling) STDLIB else EXE)
      }
    }
  }

  def main(p: CangjieProject): Unit = O2Env.stage(Stage.CangjieMain) {
    // Here new options can be used
    initialize()
    parsingStage(p)
    setOutputName()

    checkMains()

    if (ProjectLogic.compilationMode != CompilationMode.ONoCode) {
      // Triggers inline planning stage or call graph closure.
      // Now each worker performs call graph closure analysis while result of the analysis performed by the driver could be resused by workers.
      // TODO: serialize call graph closure analysis result in driver and deserialize it in workers.
      VZC.compiler

      classAmountToCompile = classesToCompile.toSeq.size
      ProjectLogic.classesAmount = classAmountToCompile

      writeOrReadFileDescriptors()
      middleStage()
      genAOTReflectionInfo()
      codeGenStage()
    }

    xiEnv.config.setEquation2("CPURequirements", xiEnv.config.equation("CompilerCPURequirements"))

    if (targetArch == CBC) {
      O2Env.stage(Stage.CBCFileGenerator) {
        CBCFileGenerator.generate(getOutputCbcPath)
      }
      xPDB.closeAll()
    } else {
      if (genDwarf) {
        Dwarf.link()
      }
      if (!workerMode) {
        if (cangjieStdLibCompiling) {
          concatObjFiles()
        } else {
          link()
        }
      }
    }

    VZC.compiler.printFinalStatistics()
  }
}
