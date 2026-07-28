/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.common.Arch.*
import com.huawei.excelsior.common.Language.{CANGJIE, JAVA}
import com.huawei.excelsior.common.{JetDirs, Language, LanguagePack}
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.debug.dwarf.Dwarf
import com.huawei.excelsior.jet.compiler.driver.ProjectLogic
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.be_386.opCodeModule as opCode
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcJCAModule as pcJCA, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.CommandLineParser
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.cangjie.{CangjieMain, CangjieProject}
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.{RTCacheModule as RTCache, xPDBManagerModule as xPDBManager, xPDBModule as xPDB}
import com.huawei.excelsior.jet.compiler.o2lib.u.xiFilesModule.FileDescriptor
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, PackagerModule as Packager, PropertiesModule as Properties, RawFileModule as RawFile, TextFileModule as TextFile, TimeRecModule as TimeRec, xOptionsModule as opt, xcFModule as xcF, xcMain0Module as xcMain0, xcMainModule as xcMain, xcMakeModule as mk, xcModesModule as xcModes, xcResourcesModule as xcResources, xcVersionModule as xcVersion, xiEnvModule as env, xiFilesModule as xfs, xmArgsModule as xmArgs, xmErrorsModule as xmErrors, xmFS2Module as xmFS2}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.{FSModule as FS, MemoryManagementModule as mm}
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.options.BoolOption.{GenDebug, GenLibrary, GenMegaObj, Makefile, PackPDB, ReuseRtDwarf}
import com.huawei.excelsior.jet.compiler.options.Option.SmartKind
import com.huawei.excelsior.jet.compiler.options.Option.SmartKind.*
import com.huawei.excelsior.jet.compiler.options.StrOption.{FrontEnd, Locales, OPTRTFILES, PackExtraItems, TargetDir, TargetZip}
import com.huawei.excelsior.jet.compiler.xpackii.ArchiveUtils
import com.huawei.excelsior.jet.compiler.xpackii.Filter.RESET_MTIME
import com.huawei.excelsior.jet.compiler.{Env, Stage}
import com.huawei.excelsior.o2s.runtime.*
import xscala.io.{Path, stderr, stdout}
import xscala.util.{UByte, UInt}

import scala.collection.mutable

/** Main module (standalone utility) */
object xmModule { /* Ned 03-Mar-94. */

  private class MainEquation extends env.Equation(js.newJString("main")) {
    setNewStringValue(js.jstrEmpty)

    override def verify(): Unit = {
      val fname = this.getStringValue
      if (fname.indexOf('\\') >= 0) {
        env.errors.fault(mk.err_back_slash, fname)
      } else if (!Env.languagePack.supports(CANGJIE) && fname.indexOf('.') >= 0) {
        env.errors.fault(mk.err_dots, fname)
      }
    }

  }

  type SetResource = UByte
  private val SetJCA: SetResource = UByte(0)
  private val SetExcludeList: SetResource = UByte(3)

  /** As I am lazy (kit), I introduced this kind of equation
      to not introduce new equation for every type
      of SetResource enumeration
  */

  private class SetResourceEquation(name: XString, type0: SetResource, checked: SmartKind)
    extends env.Equation(name, checked) {

    def this(name: String, type0: SetResource, checked: SmartKind = Checked) = {
      this(js.newJString(name), type0, checked)
    }

    //@Override
    override def verify(): Unit = {
      val value = this.getStringValue
      this.type0 match {
        case SetJCA =>
          pcJCA.setJcaFile(js.TODO2(value))
        case SetExcludeList =>
          mk.parseExcludeList(value)
      }
    }

  }


  private class JetRTEquation
    extends opt.RestrictedEquation("JETRT", ErrMsg391)(
      "WORKSTATION",
      "SERVER",
      "DESKTOP",
      "CLASSIC",
    ) {

    //@Override
    override def verify(): Unit = {
      super.verify()
      val value = this.getStringValue
      if (value.equals2("WORKSTATION")) {
        env.errors.silentMessage(ErrMsg389)
        env.config.setEquation("jetrt", "DESKTOP")
      }
    }

  }


  private class SplashImageEquation extends env.Equation(js.newJString("splash"), Unchecked) {

    //@Override
    override def verify(): Unit = {
      val value = this.getStringValue
      if (!xfs.sys.exists(value)) {
        env.errors.fault(ErrMsg399, value)
      }
    }

  }


  private class VersionInfoEquation(name: XString) extends env.Equation(name, Unchecked) {

    //@Override
    override def verify(): Unit = {
      env.config.setOption("generateversioninfo", value = true)
    }

  }


  private class AppTypeEquation
    extends opt.RestrictedEquation("apptype", ErrMsg645)(
      "INVOCATIONDLL",
      "TOMCAT",
      "IDEA",
      "SPRINGBOOT",
      "PLAIN",
    ) {

    override def verify(): Unit = {
      super.verify()
      if (this.getStringValue.equals2("INVOCATIONDLL")) {
        env.config.setOption("gendll", value = true)
      }
    }

  }


  private class CompactProfileEquation
    extends opt.RestrictedEquation("compactprofile", ErrMsg665)(
      "COMPACT1",
      "COMPACT2",
      "COMPACT3",
      "FULL",
      "AUTO",
    ) {

    override def verify(): Unit = {
      super.verify()
      mk.javaseExcludedPackages = null
      mk.javaseIncludedPackages = null
      if (this.getStringValue.equals2("COMPACT1")) {
        mk.addJavaSEIncludedPackages(mk.compact1Packages)
        mk.addJavaSEExcludedPackages(mk.compact2Packages)
        mk.addJavaSEExcludedPackages(mk.compact3Packages)
        mk.addJavaSEExcludedPackages(mk.fulljrePackages)
      } else if (this.getStringValue.equals2("COMPACT2")) {
        mk.addJavaSEIncludedPackages(mk.compact1Packages)
        mk.addJavaSEIncludedPackages(mk.compact2Packages)
        mk.addJavaSEExcludedPackages(mk.compact3Packages)
        mk.addJavaSEExcludedPackages(mk.fulljrePackages)
      } else if (this.getStringValue.equals2("COMPACT3")) {
        mk.addJavaSEIncludedPackages(mk.compact1Packages)
        mk.addJavaSEIncludedPackages(mk.compact2Packages)
        mk.addJavaSEIncludedPackages(mk.compact3Packages)
        mk.addJavaSEExcludedPackages(mk.fulljrePackages)
      } else if (this.getStringValue.equals2("FULL")) {
        mk.addJavaSEIncludedPackages(mk.compact1Packages)
        mk.addJavaSEIncludedPackages(mk.compact2Packages)
        mk.addJavaSEIncludedPackages(mk.compact3Packages)
        mk.addJavaSEIncludedPackages(mk.fulljrePackages)
      }
      if (this.getStringValue.equals2("FULL")) {
        mk.javaseExcludeTypes = new mutable.HashSet[XString]
      } else {
        mk.javaseExcludeTypes = mk.fullJreIncludeTypes
      }
    }

  }


  private class ExcludePackagesEquation extends env.Equation(js.newJString("excludepackages")) {

    //@Override
    override def verify(): Unit = {
      mk.addJavaSEExcludedPackages(env.convValueToSet(this.getStringValue))
    }

  }

  private var totaltime: UInt = _
  private var curpro: mk.Project = _ /* global due bug in X2 garbage collector */
  private var appendedmod: mk.File = _
  private var packager: Packager.Packager = _
  private var exited: Boolean = _
  //--------------------------------------------------------------------
  private var jreVersionStr: XString = _
  private var profileStr: XString = _
  private var profileNameStr: XString = _

  /*----------------------------------------------------------------*/
  private def redirection(): Unit = {
    if (!isStandalone) {
      val fn = xfs.sys.sysLookup("red")
      val f = xfs.text.openToRead(fn)
      if (f != null) {
        val err = xcMain.readRedirection(f)
        f.close()
        if (err) {
          env.exit(1)
        }
      }
    }
  }

  private def set_JET_HOME(): Unit = {
    if (!isStandalone) {
      var dir = FS.getPath(env.args.programName)
      //  dir := FS.toPlatform(dir);
      dir = js.format("%S/..", dir)
      env.config.setEquation2("jet_home", dir)
    }
  }

  private def registerVersionInfoEquation(name: String): Unit = {
    if (targetOS.isWindows) {
      val e = new VersionInfoEquation(js.newJString(name))
      env.config.registerEquation(e)
    } else {
      env.config.registerEquation(new opt.DeniedEquation(name, ErrMsg357))
    }
  }

  private def setDefaultOptionsAndEquations(): Unit = {
    env.config.newEquation("jet_home", Unchecked)
    env.config.newEquation("profile")
    env.config.newEquation("profile_name")

    // This value is only written to CPB as JREHome
    env.config.newEquation("jet_jre_home", Unchecked)

    env.config.newEquation("libext")
    env.config.setEquation("libext", "lib")

    env.config.newEquation("dllpref_target")
    env.config.setEquation("dllpref_target", FS.TARGET.dllPrefix)
    env.config.newEquation("dllpref_host")
    env.config.setEquation("dllpref_host", FS.HOST.dllPrefix)

    env.config.newEquation("exeext_target")
    env.config.setEquation("exeext_target", FS.TARGET.exeExtension)
    env.config.newEquation("exeext_host")
    env.config.setEquation("exeext_host", FS.HOST.exeExtension)
    env.config.newEquation("dllext_target")
    env.config.setEquation("dllext_target", FS.TARGET.dllExtension)
    env.config.newEquation("dllext_host")
    env.config.setEquation("dllext_host", FS.HOST.dllExtension)
    env.config.newEquation("mobjext")
    env.config.setEquation("mobjext", "o")
    env.config.newEquation("expext")
    env.config.setEquation("expext", "expdef")
    env.config.newEquation("mkfext")
    env.config.setEquation("mkfext", "rsp")
    env.config.newEquation("jexpext")
    env.config.setEquation("jexpext", "jexp")
    env.config.newEquation("dllname")
    env.config.newEquation("outputname", Unchecked)

    env.config.newOption("ignoreclassduplication", value = true)

    env.config.newEquation("linkefs")
    env.config.setEquation("linkefs", "xlink -NoMessages -EmbeddedFileSys=\"%S\" -PackEmbeddedFileSys=\"%S\" -NoEFSTimeStamps -NoConsistencyInfo")

    env.config.registerEquation(new MainEquation())

    env.config.registerEquation(new SetResourceEquation(pcJCA.jcaOpt, SetJCA, Checked))

    env.config.registerEquation(new opt.RestrictedEquation("classabsence", ErrMsg607)(
      "HANDLE",
      "IGNORE",
      "ERR",
    ))
    env.config.setEquation("classabsence", "HANDLE")

    env.config.newEquation("jetvmprop", Unchecked)
    env.config.newEquation("componentclasspath")


    env.config.newOption("visibleresource", value = false, Unchecked)

    env.config.registerEquation(new opt.RestrictedEquation("pack", ErrMsg640, Unchecked)(
      "NONE",
      "NONEANDOMITCLASSES",
      "ALL",
      "NONCOMPILED",
      "ASDIRNONCOMPILED",
      "RESOURCES",
    ))

    // env.config.SetEquation("pack", "NONCOMPILED"); ---NIL value treated as NONCOMPILED
    val protectOptimizeChecking = Checked


    env.config.registerEquation(new opt.RestrictedEquation("optimize", ErrMsg642, protectOptimizeChecking)(
      "ALL",
      "AUTODETECT",
    ))

    // env.config.SetEquation("optimize", "ALL"); -- NIL value treated as ALL
    env.config.registerEquation(new opt.RestrictedEquation("protect", ErrMsg643, protectOptimizeChecking)(
      "ALL",
      "NOMATTER",
    ))

    // env.config.SetEquation("protect", "NOMATTER"); -- NIL value treated as NOMATTER
    env.config.newEquation("tmpresourcedir", Unchecked)
    env.config.setEquation("tmpresourcedir", "tmpres")

    env.config.newOption("SYSTEMCLASSES", value = false, Checked)         // changed in prj
    env.config.newOption("EXTENSIONCLASSLOADER", value = false, Checked)  // changed in prj
    env.config.newOption("RUNTIMECLASSES", value = false, Checked)        // changed in prj

    // Aggressive optimizations are applied to well-known controlled classes: runtime, rt.jar, compiler itself, ...
    env.config.newOption("OptimizeAggressively", value = false, Checked)

    env.config.newEquation("jetver")
    env.config.setEquation("jetver", xcVersion.JetVerEquationValue)

    env.config.newEquation("StandAloneResources")

    env.config.newEquation("VERSION_INFO")
    env.config.newEquation("COMPATIBILITY_INFO")

    env.config.newEquation("HEAPLIMIT", Unchecked) // backward compatibility

    set_JET_HOME()

    env.config.registerEquation(new JetRTEquation())
    env.config.setEquation("JETRT", "SERVER")

    env.config.newEquation("MajorJETVersion")
    env.config.setEquation("MajorJETVersion", xcVersion.MajorJETVersionStr)

    env.config.newEquation("MinorJETVersion")
    env.config.setEquation("MinorJETVersion", xcVersion.MinorJETVersionStr)

    env.config.newEquation("JETEdition")
    env.config.setEquation("JETEdition", xcVersion.JetEditionEquationValue)

    env.config.registerEquation(new SetResourceEquation("excludelist", SetExcludeList))

    env.config.registerEquation(new SplashImageEquation())

    if (targetOS.isWindows) {
      env.config.newOption("generateversioninfo", value = false, Unchecked)
      registerVersionInfoEquation("VersionInfoCompanyName")
      registerVersionInfoEquation("VersionInfoProductName")
      registerVersionInfoEquation("VersionInfoFileDescription")
      registerVersionInfoEquation("VersionInfoLegalCopyright")
      registerVersionInfoEquation("VersionInfoProductVersion")
    }

    env.config.newEquation("additionalclasspath")  // todo: try to remove

    env.config.newEquation("outputdir")

    env.config.newEquation("appdir")

    env.config.registerEquation(new AppTypeEquation())

    env.config.newEquation("springbootarchive", Unchecked)

    env.config.registerOption(new opt.ConfigBitOption("SuperImportOnly", env.superimportonly), value = false)

    env.config.registerOption(new opt.ConfigBitOption("regularbuild", env.regularbuild), value = true)

    env.config.registerEquation(new CompactProfileEquation())
    env.config.registerEquation(new ExcludePackagesEquation())  // TODO: try to remove

    env.config.newOption("localecomponent", value = false, Checked)   // changed by states in prj ???

    env.config.newOption("GenMetaInfoForRuntimeClasses", value = true, RuntimeRecompile)

    env.config.newOption("RecompileRuntime", value = false, RuntimeRecompile)

    env.config.newEquation("parallelism", Unchecked)
    env.config.newEquation("worker", Unchecked)

    env.config.newEquation("stdlibCbcPath", Unchecked)
    env.config.setEquation("stdlibCbcPath", "!stdlib.cbc")
    env.config.newEquation("metaCbcPath", Unchecked)
    env.config.setEquation("metaCbcPath", "!meta.cbc")
  }

  private def config(): Unit = {
    setDefaultOptionsAndEquations()
    if (!isStandalone) {
      val fn = xfs.sys.sysLookup("cfg")
      val f = xfs.text.openToRead(fn)
      if (f == null) {
        env.info.forcePrint("Configuration file %S is not opened: %S\\n", fn, xfs.text.errmsg)
        env.exit(1)
      }
      val err = xcMain.readConfig(f)
      f.close()
      if (err) {
        env.exit(1)
      }
      Properties.initJCProperties()
      xcMain.saveRuntimeContext()
      mk.initCompactProfilesPackages()
    }
  }

  /*----------------------------------------------------------------*/
  private def help(): Unit = {
    val helpFile = FS.HOST.toPlatform(js.format("%S/pdf/jc.pdf", env.config.equation("jet_home")))
    env.errors.message(ErrMsg250, helpFile)
  }

  private def printUsage(): Unit = {
    env.info.forcePrint("\\nUsage:\\n  Build project:\\n    jc =p[roject] { PROJECTFILE | OPTION | EQUATION }\\n  Compile:\\n    jc { FILENAME | OPTION | EQUATION }\\n\\nFor help, type:\\n  jc =help\\n\\n")
  }

  /*----------------------------------------------------------------*/
  private def collect(): Unit = {
    if (!pcO.isCangjie) {
      mm.compactHeap()
    }
  }

  private def recompile(curpro: mk.Project): Unit = O2Env.stage(Stage.CompileProject) {
    curpro.compile()
  }

  private def getEquationWithDefault(name: String, defaultValue: XString): XString = {
    val eq0 = env.config.equation(name)
    if (eq0 == null) {
      defaultValue
    } else {
      eq0
    }
  }

  private def createPackager(): Unit = {
    var jetJreHome: XString = null
    var ext: XString = null

    var outputZip = XString(O2Env.env.valueOfOrNull(TargetZip))
    val targetDir = XString(O2Env.env.valueOfOrNull(TargetDir))
    if (O2Env.env.enabled(BoolOption.Package) || outputZip != null || targetDir != null || pcO.isTomcat || pcO.isIdea) {
      val outputname = env.config.equation("outputname")
      val outputdir = env.config.equation("outputdir")
      val appdir = env.config.equation("appdir")

      if (env.config.option("gendll")) {
        ext = env.config.equation("dllext_target")
      } else if (O2Env.env.enabled(GenMegaObj)) {
        ext = env.config.equation("mobjext")
      } else {
        ext = env.config.equation("exeext_target")
      }
      val outputExe = FS.makeFileName(outputdir, outputname, ext)

      val locales = XString(O2Env.env.valueOfOrElse(Locales, "NONE"))
      val optRTFiles = XString(ProjectLogic.optRTFiles)

      var compactProfile = env.config.equation("compactprofile")
      if (compactProfile == null || compactProfile.toUpperCase.equals2("AUTO")) {
        compactProfile = js.newJString("FULL")
      }

      if (outputZip == null) {
        outputZip = js.format("%S-image.zip", outputname)
      } else if (!outputZip.endsWith(js.newJString(".zip"))) {
        outputZip = FS.addExt2(outputZip, "zip")
      }

      val splash = env.config.equation("splash")
      val hasSplash = splash != null && !splash.isEmpty || ProjectLogic.multiapp

      if (languagePack.supports(JAVA)) {
        jetJreHome = env.getProfileJREDir
      } else {
        jetJreHome = null
      }

      val packedBundles = getEquationWithDefault("packedbundles", js.jstrEmpty)
      val toHideClassesBundles = getEquationWithDefault("tohideclassesbundles", js.jstrEmpty)

      packager = new Packager.Packager()
      if (pcO.isTomcat) {
        packager.initForTomcat(outputExe, jetJreHome, outputZip, compactProfile, optRTFiles, locales, hasSplash, appdir, packedBundles, toHideClassesBundles, outputdir)
      } else {
        var extraFiles = XString(O2Env.env.valueOfOrElse(PackExtraItems, ""))
        if (isWorkMode) {
          // by default .map file is generated near the output binary with the same base name
          val mapFile = FS.makeFileName(outputdir, outputname, js.newJString(".map"))
          if (xfs.sys.exists(mapFile)) {
            if (extraFiles.isEmpty) {
              extraFiles = mapFile
            } else {
              extraFiles = extraFiles.concat(js.newJString(",")).concat(mapFile)
            }
          }
        }
        packager.init(outputExe, jetJreHome, outputZip, compactProfile, optRTFiles, locales, hasSplash, extraFiles)
      }
    }
  }

  private def packPDB(): Unit = {
    if (!O2Env.env.enabled(PackPDB)) {
      return
    }

    val pdbArchiveName = Path(js.format("%S.pdba", env.config.equation("outputname")).toString)
    val canonicalPdbDirPath = Path(xfs.sys.getCanonicalPath(env.config.equation("PDBNAME")).toString)

    ArchiveUtils.putDirectoryToArchive(Path.dot, canonicalPdbDirPath, canonicalPdbDirPath, pdbArchiveName, RESET_MTIME)
  }

  private def Package(): Unit = {
    if (packager != null) {
      packager.pack(stdout, stderr)
    }
  }

  private def exit(): Unit = {
    if (exited) {
      return
    }
    TimeRec.done()
    totaltime = env.diffTimes(xcMain.starttime, env.time())

    env.info.print("\\nTotal compilation time %d:%02d.%02d\\n", ((totaltime / UInt(100)) / UInt(60)).toInt, ((totaltime / UInt(100)) % UInt(60)).toInt, (totaltime % UInt(100)).toInt)

    if (env.errDetected) {
      env.info.print("\\nCompilation failed.\\nPlease check compilation log for more information.\\n")
      sys.exit(1)
    }

    packPDB()
    Package()

    exited = true
    //  HALT(0);
  }

  private def linkEfs(efscmd: XString): Unit = {
    if (efscmd != null && efscmd.nonEmpty && !env.errors.envErrDetected && xcResources.createdEfsName != null) {
      env.info.print("\\nPacking efs data...\\n")
      val a = js.format(js.TODO2(efscmd), xcResources.createdEfsName, xcResources.createdEfsName)
      xcF.runLinker(a)
    }
  }

  private def recompileAndLink(localcurpro: mk.Project, make: Boolean): Unit = {
    // Here new options can be used
    if (env.config.option(s"$GenDebug") || env.config.option(s"$ReuseRtDwarf")) {
      Dwarf.setEquationsForRSP((eq, value) => env.config.setEquation2(eq, XString.ascii(value)))
    }

    var efscmd: XString = null

    localcurpro.regulate()
    if (localcurpro.errs != 0 || env.config.option("__XDS_LIST__")) {
      xcMain.endProject(localcurpro)
      curpro = null
      return
    }

    recompile(localcurpro)
    if (localcurpro.errs == 0 && !xcModes.workerMode) {
      env.config.setOption("NOINVLOOKUPS", value = true)
      val rp = xcResources.newResourceProcessor(localcurpro, createefs = true)
      rp.processResources()
      if (O2Env.env.enabled(GenDebug)) {
        Dwarf.link()
      }
      if (O2Env.env.enabled(Makefile)) {
        xcF.makeProject(localcurpro)
      }
      xcMain0.compilationExit()
      if (env.config.option("ProcessEfs")) {
        efscmd = env.config.equation("LINKEFS")
      } else {
        efscmd = null
      }
      var link = env.config.equation("LINK")
      if (localcurpro.errs == 0 && link != null && link.nonEmpty && !O2Env.env.enabled(GenLibrary)) {
        link = link.concat(XString(s" \"@${env.config.equation("RSPFILENAME")}\""))
        createPackager()
        xcMain.endProject(localcurpro)
        curpro = null
        js.cleanStringsCache()
        // js.exit();
        collect()
        if (link != null && !env.config.option("nolink")) {
          val start_t = env.time()
          xcF.runLinker(link)
          val total_t = env.time() - start_t
          env.info.print("\\nLink time %d:%02d.%02d\\n", ((total_t / UInt(100)) / UInt(60)).toInt, ((total_t / UInt(100)) % UInt(60)).toInt, (total_t % UInt(100)).toInt)
        }
        linkEfs(efscmd)
        exit()
      } else {
        xcMain.endProject(localcurpro)
        curpro = null
        linkEfs(efscmd)
      }
    } else {
      xcMain0.compilationExit()
      xcMain.endProject(localcurpro)
      curpro = null
    }
  }

  private def Do(): Unit = {
    env.config.save()
    xfs.sys.saveRed()
    env.errDetected = false

    xcModes.init()

    var i = 0
    while (i < env.args.number()) {
      val jstr = env.args.getArg(i)
      if (xcModes.isModeSpecifier(jstr)) {
        xcModes.setMode(js.TODO2(jstr))
        env.args.deleteArg(i)
      } else {
        i += 1
      }
    }
    if (xcModes.job == xcModes.nothing) {
      xcModes.job = xcModes.make
    }
    if (xcModes.help) {
      help()
      return
    }
    if (xcModes.clean) {
      RTCache.cleanGlobalCache()
    }
    if (env.args.number() == 0) {
      printUsage()
      return
    }


    /*----------------------------------------------------------------*/

    pcO.isCangjie = O2Env.env.valueOf(FrontEnd).equalsIgnoreCase("CANGJIE")

    if (xcModes.tomcat || xcModes.idea) {
      if (env.args.number() != 1) {
        printUsage()
        sys.exit(1)
      }
      var jstr = env.args.getArg(0)

      // convert to absolute path
      jstr = FS.HOST.toPlatform(FS.HOST.fullPath(jstr))

      if (xcModes.tomcat) {
        env.info.print("Make Tomcat application \"%S\"\\n", jstr)
        env.config.setEquation("apptype", "tomcat")
      } else if (xcModes.idea) {
        env.info.print("Make IntelliJ IDEA application \"%S\"\\n", jstr)
        env.config.setEquation("apptype", "idea")
      } else {
        throw new AssertionError
      }

      env.config.setEquation2("appdir", jstr)

      var prj = env.config.equation("PRJ")
      assert(prj == null)
      
      env.config.setEquation2("PDBNAMEPREFIX", FS.getBaseName(FS.HOST.fromPlatform(jstr)))
      curpro = xcMain.useProject(js.jstrEmpty)

      recompileAndLink(curpro, make = false)

    } else if (xcModes.job == xcModes.pro) {
      assert(env.args.number() == 1, "JC does not support several projects")
      val jstr = FS.HOST.fromPlatform(env.args.getArg(0))
      env.info.print("Make project \"%S\"\\n", FS.HOST.toPlatform(jstr)) //- !!! to print name in host format

      if (pcO.isCangjie) {
        CangjieMain.main(CangjieProject.openProject(jstr))
      } else {
        curpro = xcMain.readProject(jstr)
        if (curpro == null) {
          sys.exit(1)
        }
        recompileAndLink(curpro, make = false)
      }

    } else {
      if (!pcO.isCangjie) {
        var jstr = env.config.equation("PRJ")
        assert(jstr == null)

        curpro = xcMain.useProject(js.jstrEmpty)

        if (curpro == null) {
          sys.exit(1)
        }
      }
      if (xcModes.job == xcModes.make) {
        if (pcO.isCangjie) {
          CangjieMain.main(CangjieProject.createProjectFromArgs())
        } else {
          var module: XString = null
          val modcou = env.args.number()
          for (i <- 0 until modcou) {
            var jstr = env.args.getArg(i)
            jstr = FS.HOST.fromPlatform(jstr)
            appendedmod = curpro.appendFile(jstr)
            if (appendedmod != null && (appendedmod.mode == mk.md_jbc)) {
              module = jstr
            }
          }
          if (module != null && modcou == 1) {
            env.config.setEquation2("INPUTMOD", module)
          }

          recompileAndLink(curpro, make = true)
        }

      }
    }

    assert(curpro == null) // Check that it was called EndProject and curpro is nulled
    //  ShowTailer;
    exit()
  }

  def run(): Unit = {
    withO2JSupportExceptions {
      xcMain.starttime = env.time()
      xmFS2.setManagers()
      RawFile.setManagers()
      TextFile.setManagers()
      xmErrors.setManagers()
      xmArgs.setManagers()
      xcMain0.declareOptions()
      xcMain.declareOptions2()
      opCode.init();
      env.loadType = name => xcMain.loadType(curpro, name)
      xPDBManager.initManager()
      redirection()
      config()
      CommandLineParser.parseCommandLine()
      if (languagePack == LanguagePack.JAVA) {
        jreVersionStr = env.config.equation("jre_version")
        profileStr = env.config.equation("profile")
        profileNameStr = env.config.equation("profile_name")
        if (profileStr == null || profileStr.isEmpty) {
          env.errors.fault(ErrMsg468, "PROFILE")
        }
        if (profileNameStr == null || profileNameStr.isEmpty) {
          env.errors.fault(ErrMsg468, "PROFILE_NAME")
        }

        env.info.print("Active Java SE Version %S (profile %S)\\n", jreVersionStr, profileStr)
      }
      TimeRec.init()
      exited = false

      Do()

      xPDB.closeAndCleanup()
    }
  }

  private def withO2JSupportExceptions(action: => Unit): Unit = {
    try {
      action
    } catch {
      case e: Throwable =>
        if (e.isInstanceOf[OutOfMemoryError]) {
          xmErrors.printMem(doPrintErr = true)
          env.errors.fault(ErrMsg950)
        }
        if (!isWorkMode) {
          xmErrors.abortCompilation(e)
        }

        // Do not close PDB in case of abnormal termination
        // if smart is enabled if will allow to not touch the previous version of PDB
        throw e
    }
  }
}
