/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.starter

import com.huawei.excelsior.common
import com.huawei.excelsior.common.Environment.{JC_STANDALONE, LANGUAGE_PACK}
import com.huawei.excelsior.common.JetDirs.jetHome
import com.huawei.excelsior.common.Language.JAVA
import com.huawei.excelsior.common.LanguagePack.CANGJIE_JAVA
import com.huawei.excelsior.common.ProcessUtils.sanitizeCommand
import com.huawei.excelsior.common.{DynamicBundle, JetDirs, Mode, XProcess}
import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler
import com.huawei.excelsior.jet.compiler.abi.{ABI, Platform}
import com.huawei.excelsior.jet.compiler.cbc.CBCFileGenerator
import com.huawei.excelsior.jet.compiler.delayed.DelayedIntrinsicsUsageTracker
import com.huawei.excelsior.jet.compiler.driver.*
import com.huawei.excelsior.jet.compiler.o2lib.opt.VZCModule
import com.huawei.excelsior.jet.compiler.o2lib.jprof.JProfManagerModule
import com.huawei.excelsior.jet.compiler.o2lib.u.*
import com.huawei.excelsior.jet.compiler.opt.Opt
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.options.StrOption.*
import com.huawei.excelsior.jet.compiler.options.{BoolOption, NumOption, Option, StrOption}
import com.huawei.excelsior.jet.compiler.starter.AOTStarter.{copyCangjieStaticPdb, runCommand}
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.*
import com.huawei.excelsior.jet.compiler.types.{CHA, ReferenceTypes}
import com.huawei.excelsior.jet.compiler.wrappers.CompilerWithAJWrappers
import com.huawei.excelsior.jet.compiler.{Compiler, CompilerWithStats, Env, Environment, LightweightCompiler}
import xscala.io.*
import xscala.properties.OS
import xscala.util.StringOps.asciiToLowerCase
import xscala.vm.VMConfig
import xscala.sync.Sync.{Lock, newLock}
import xscala.sync.XThread

import java.io.IOException
import java.security.NoSuchAlgorithmException
import scala.collection.mutable.ArrayBuffer
import scala.util.Using

/** Entry point for AOT compiler. Setups dependencies for compiler components and runs compilation process.
  *
  * @author conwor
  * @author ijorch
  */
object AOTStarter {
  private val INTERNAL_ERROR_CODE = 64

  private def env = LightweightEnvironment.getInstance

  def main(args: Array[String]): Unit = {
    try {
      run(args)
      sys.exit(0)
    } catch { case e: Throwable =>
      stderr.println()
      stderr.println("Unexpected internal error:")
      stderr.printStackTrace(e)
      stderr.println("Please contact with Excelsior JET Support")
      sys.exit(INTERNAL_ERROR_CODE)
    }
  }

  def run(allArgs: Array[String]): Unit = {
    VMConfig.init()
    val (args, dynamicBundle) = if (JC_STANDALONE) {
      (allArgs ++ Seq(s"+$GenCBC", "-frontend=cangjie"), DynamicBundle.ON)
    } else {
      (JetDirs.obtainJetHome(allArgs), DynamicBundle(retrieveEquationFromJcCfg(DYNAMIC_BUNDLE)))
    }

    // Check args for CBC platform enabling
    val impl = if (dynamicBundle.enabled && checkGenCBC(args)) new CBCStarterImpl else new PDStarterImpl
    Env.init(impl.getPlatform, false, common.Environment.MODE == Mode.WORK, dynamicBundle.enabled, LANGUAGE_PACK, JC_STANDALONE)

    // 0. From this point we can use Env API

    LanguagePackConfig.init()

    JProfManagerModule.impl = new JProfManagerImpl
    PackagerModule.setImpl(PackagerImpl)

    env // Force initialise env

    Profile.env = env
    CBCFileGenerator.env = env
    DelayedIntrinsicsUsageTracker.env = env
    ReplacementLibrary.env = env
    ProjectLogic.env = env
    ReferenceTypes.env = env

    VZCModule.compilerProvider = { () =>
      CHA.init(env)
      val opt = new Opt(env, impl.getOptPlatformConfig)

      initCompilationDriver(env, opt, args)

      val withWrappers = new CompilerWithAJWrappers(env, opt, impl.getWrappersPlatformConfig)
      new LightweightCompiler(withWrappers)
    }

    StdLibCompilerModule.setImpl(() => checkOrCompileStdlib(args, dynamicBundle))

    val resArgs = if (checkRequiresCangjieStdlib(args)) addCangjieStdlibExplicitDependency(args) else args
    xmArgsModule.setArgs(resArgs)
    xmModule.run()
  }

  private lazy val jcCfg = Files.readAllLines(JetDirs.bin / "jc.cfg")

  private def retrieveEquationFromJcCfg(eq: String) =
    jcCfg.find(_.toLowerCase.startsWith(eq)).map(_.substring(eq.length)).orNull

  //// Cangjie StdLib compilation ////

  private val STDLIB_EQ    = "-stdlib="
  private val CBCSTDLIB_EQ = "-cbcstdlib="
  private val O2_CBCSTDLIB_EQ = "-o2cbcstdlib="
  private val EXCLUDELIB_EQ = "-excludelib="

  private val STDLIB_NAME = "CangJieStdLib"

  private val STDLIB_PACKAGES_TO_O1 = Seq(
    "std.unittest.common",
    "std.unittest.prop_test",
    "std.unittest.diff",
    "std.unittest.mock.internal",
    "std.unittest.mock",
    "std.unittest",
    "std.unittest.testmacro",
    "std.unittest.mock.mockmacro",
  )

  private val USE_LIBRARY_EQ  = "-uselibrary="
  private val USE_LIBRARY_SEP = ","

  private val LAUNCHER_STUB_ARCHIVE = "Launcher.zip"
  private val CJ_DLL_NAME = "libcjvm"
  private val CJMAP_JAR = "jmap.zip"
  private val CJMAP_NAME = "cjmap"
  private val CJSTACK_JAR = "jstack.zip"
  private val CJSTACK_NAME = "cjstack"

  private val DYNAMIC_BUNDLE = "-dynamicbundle="

  private val RELATIVE_JET_JRE_HOME = "*{comp.dir}/jet/profile/jre"

  /** Returns true, iff `str` is a string defined given `option`. */
  private def isOption[T >: Null <: Any](_str: String, option: Option[T]): Boolean = {
    val str = _str.toLowerCase
    val optionName = option.name.toLowerCase

    option match {
      case option: BoolOption =>
        (str == s"+$optionName") || (str == s"-$optionName") || (str == s"-$optionName:+") || (str == s"-$optionName:-")

      case option: NumOption =>
        str.startsWith(s"-$optionName=")

      case option: StrOption =>
        str.startsWith(s"-$optionName=")
    }
  }

  private def addCangjieStdlibExplicitDependency(args: Array[String]): Array[String] = {
    args.indexWhere(_.toLowerCase.startsWith(USE_LIBRARY_EQ)) match {
      case -1 =>
        // command line doesn't have "uselibrary" equation so just add it with stdlib
        args :+ (USE_LIBRARY_EQ + STDLIB_NAME)

      case i =>
        // add stdlib to "uselibrary" equation if it is not added already
        val oldArg = args(i)
        val useLibraryVal = oldArg.stripPrefix(USE_LIBRARY_EQ)
        if (!useLibraryVal.split(USE_LIBRARY_SEP).contains(STDLIB_NAME)) {
          args(i) = oldArg + "," + STDLIB_NAME
        }
        args
    }
  }

  private def checkRequiresCangjieStdlib(args: Array[String]) =
    args.exists(_.equalsIgnoreCase(s"-$FrontEnd=cangjie")) && args.exists(_.toLowerCase.startsWith(STDLIB_EQ))

  private def profilePDBLocation = Path(xiEnvModule.getProfileLibraryPath(STDLIB_NAME).toString)

  private def splitArgValue(args: Array[String], equation: String): Array[String] = args.find(_.startsWith(equation)) match {
    case Some(arg) => arg.stripPrefix(equation).split(",")
    case None => Array.empty[String]
  }

  private def checkOrCompileStdlib(args: Array[String], dynamicBundle: DynamicBundle): Boolean = {
    val stdLibPDB = profilePDBLocation
    val stdLibPDBMD5 = stdLibPDB / s"$STDLIB_NAME.md5"
    val aotStdLibMD5 = stdLibPDB / "static/aot.md5"
    val forceCompile = args.exists(_.startsWith("=cl"))
    try {
      if (forceCompile || !stdLibPDBMD5.exists) {
        println("Compiling Cangjie standard library. Please wait ...")

        if (compileStdLib(args, stdLibPDB, dynamicBundle)) {
          if (!dynamicBundle.enabled) { // aot-compilation is finished
            Files.write(aotStdLibMD5, Array[Byte]())
          }
          Files.write(stdLibPDBMD5, Array[Byte]())
          true
        } else {
          false
        }
      } else if (!checkGenCBC(args) && !aotStdLibMD5.exists) {
        // It's supposed that aot+cbc build should be done first.
        // `--output-type=exe` should not be used as first run and together with `=cl` jc option
        aotCompileStdLib(args, stdLibPDB / "static")
      } else {
        true
      }
    } catch { case e @ (_: NoSuchAlgorithmException | _: IOException) =>
      stdout.printStackTrace(e)
      false
    }
  }

  private def createCompileLibCommandBase(args: Array[String]): ArrayBuffer[String] = {
    val command = ArrayBuffer.empty[String]

    // see JET-16961
    command += JetDirs.jc(true)

    command ++= args.filter { arg =>
      val argLowerCase = arg.asciiToLowerCase
      !arg.endsWith(".bc") &&
        !argLowerCase.startsWith(STDLIB_EQ) &&
        !argLowerCase.startsWith(CBCSTDLIB_EQ) &&
        !argLowerCase.startsWith(EXCLUDELIB_EQ) &&
        argLowerCase != OPTIMIZECBCSTDLIB_OP &&
        !isOption(arg, GenCBC) &&
        !isOption(arg, CleanCompilation) &&
        !isOption(arg, OutputName) &&
        !isOption(arg, PDBLocation)
    }

    command += s"-$JCAdvise=${jetHome / "bin/cangjie-stdlib.jca"}"
  }


  private def createAOTStdLibCommand(args: Array[String], stdlibs: Array[String]): ArrayBuffer[String] = {
    val command = createCompileLibCommandBase(args)
    command ++= Seq(
      s"+$GenLibrary",
      s"+$GenProfileLibrary",
      s"-$LibraryName=$STDLIB_NAME",
      STDLIB_PACKAGES_TO_O1.mkString(s"-$CangjiePackagesToO1=", ",", ""),
      s"-$OutputName=$STDLIB_NAME",
    )

    command ++= (stdlibs sortBy (lib => STDLIB_PACKAGES_TO_O1 exists (x => lib endsWith s"$x.bc")))
  }

  private def createCbcStdLibCommand(args: Array[String], pdbLocation: String): ArrayBuffer[String] = {
    val command = createCompileLibCommandBase(args)

    command ++= Seq(
      s"+$GenCbcStdLib",
      s"-$PDBLocation=$pdbLocation",
      s"-$CleanCompilation",
      s"+$GenCBC",
    )
  }

  private def createCbcStaticLibCommand(args: Array[String], stdlibs: Array[String], pdbLocation: String): ArrayBuffer[String] = {
    val command = createCompileLibCommandBase(args) filterNot { arg =>
      isOption(arg, GenAOTReflectionInfo) // Do not generate new reflection info. The existing one (created for aot+cbc library) will be used
    }

    command ++= Seq(
      s"+$GenLibrary",
      s"+$GenProfileLibrary",
      s"-$LibraryName=$STDLIB_NAME",
      s"+$GenCbcStdLib",
      s"-$PDBLocation=$pdbLocation",
    )

    command ++= stdlibs
  }

  private def compileStdLib(args: Array[String], stdlibPDB: Path, dynamicBundle: DynamicBundle): Boolean = {

    // lib paths to be AOT-compiled to stdlib
    var stdlibs = splitArgValue(args, STDLIB_EQ) ensuring (_.nonEmpty)
    // lib paths to be compiled to cbc format
    var cbclibs = splitArgValue(args, CBCSTDLIB_EQ)

    val excludedLibSuffixes = splitArgValue(args, EXCLUDELIB_EQ)
    def isLibExcluded(lib: String) = excludedLibSuffixes.exists(suffix => lib endsWith suffix)

    stdlibs = stdlibs filterNot isLibExcluded
    cbclibs = cbclibs filterNot isLibExcluded

    if (!dynamicBundle.enabled) { // aot-compile all libs
      stdlibs ++= cbclibs
      cbclibs = Array[String]()
    }

    if (checkONoCodeForCJStdLib(args)) {
      assert(dynamicBundle.enabled)
      cbclibs = stdlibs ++ cbclibs
      stdlibs = Array[String]()
    }

    try {
      Files.makeDir(stdlibPDB)
      val logFile = stdlibPDB / s"$STDLIB_NAME.log"

      Using.resource(TextOutput.from(logFile)) { log =>
        val errorBuf = new StringBuilder

        def failed(message: String) = {
          stderr.println(errorBuf.toString)
          stderr.println(s"Failed to $message. Check $logFile for details.")
          false
        }

        if (!checkONoCodeForCJStdLib(args)) {
          val command = createAOTStdLibCommand(args, stdlibs)

          log.println(command.mkString(" "))
          log.println()

          if (!runCommand(command.toSeq, log, errorBuf)) {
            return failed(s"compile Cangjie standard library")
          }

          copyCangjieStaticPdb()
        }

        // lib paths to be optimized in cbc
        val o2libs = splitArgValue(args, O2_CBCSTDLIB_EQ)

        for (libNamePath <- cbclibs) {
          // TODO: this becomes outdated after Cangjie package names change
          val libName = Path(libNamePath).name
          val name = libName.stripSuffix(".bc")
          val cbcCommand = createCbcStdLibCommand(args, s"$profilePDBLocation")
          if (!checkOptimizeCbcStdlib(args) || !o2libs.contains(libNamePath)) {
            cbcCommand += s"+$FastBackEnd"
          }
          if (checkONoCodeForCJStdLib(args)) {
            cbcCommand += s"+$ONoCode"
          }
          cbcCommand += s"-$OutputName=${jetHome / s"profile/develop/lib/$name"}"
          cbcCommand += s"-foreignlibs=cangjie-${name}FFI"
          cbcCommand += libNamePath
          log.println(cbcCommand.mkString(" "))
          log.println()
          if (!runCommand(cbcCommand.toSeq, log, errorBuf)) {
            return failed(s"compile Cangjie standard library $libName to CBC format")
          }
        }

        if (dynamicBundle.enabled && !copyCjVMLaunchers()) {
          return failed(s"copy Cangjie VM launcher")
        }

        copyCangjieJavaLib()

        if (!checkONoCodeForCJStdLib(args)) {
          if (checkNoGenCJStack(args)) {
            // skip building of cjstack
          } else if (!compileCJStack(log, errorBuf)) {
            // TODO (JET-13861): Drop this when we can compile AJ apps by CJ Jet.
            return failed("compile cjstack tool")
          }

          if (checkNoGenCJMap(args)) {
            // skip building of cjmap
          } else if (!compileCJMap(log, errorBuf)) {
            return failed("compile cjmap tool")
          }

          if (dynamicBundle.enabled) {
            if (!relinkCjVMLauncherLib(log, errorBuf)) {
              return failed("compile cj launcher")
            }
          }
        }
      }
      true

    } catch {
      case e: Throwable =>
        stdout.printStackTrace(e)
        false
    }
  }

  private def aotCompileStdLib(args: Array[String], stdlibStaticPDB: Path): Boolean = {
    val lockfilename = stdlibStaticPDB / "lock"
    var lockFileCreated: Boolean = false
    val cbclibs = splitArgValue(args, CBCSTDLIB_EQ)

    try {
      lockFileCreated = Files.createNewFile(lockfilename)
      if (!lockFileCreated) {
        stderr.println("Cangjie standard library is being compiled by another process.")
        return false
      }

      val aotStdLibMD5 = stdlibStaticPDB / "aot.md5"
      if (aotStdLibMD5.exists) {
        return true
      }

      println("Aot compiling Cangjie standard library. Please wait ...")
      val logFile = stdlibStaticPDB / s"$STDLIB_NAME.log"
      val command = createCbcStaticLibCommand(args, cbclibs, s"$profilePDBLocation/static")

      Using.resource(TextOutput.from(logFile)) { log =>
        log.println(command.mkString(" "))
        log.println()

        val errorBuf = new StringBuilder

        def failed(message: String) = {
          stderr.println(errorBuf.toString)
          stderr.println(s"Failed to $message. Check $logFile for details.")
          false
        }

        if (!runCommand(command.toSeq, log, errorBuf)) {
          return failed("aot compile cbc part of Cangjie standard library ")
        }

        copyObjLib()
      }
      Files.write(aotStdLibMD5, Array[Byte]())
      true

    } catch {
      case e: Throwable =>
        stdout.printStackTrace(e)
        false
    } finally {
      if (lockFileCreated) FileSystem.delete(lockfilename)
    }
  }

  private def runCommand(command: Seq[String], log: TextOutput, errorBuf: StringBuilder) = {
    val p = XProcess.start(sanitizeCommand(command))
    p.stdin.close() // started process doesn't expect an input

    val outReader = XThread { processOutput(p.stdout, log, errorBuf) }
    val errReader = XThread { processOutput(p.stderr, log, errorBuf) }
    outReader.setUncaughtExceptionHandler((t: Thread, e: Throwable) => logLine(t.getName + " : " + e.getMessage, log, errorBuf))
    errReader.setUncaughtExceptionHandler((t: Thread, e: Throwable) => logLine(t.getName + " : " + e.getMessage, log, errorBuf))
    outReader.start()
    errReader.start()
    outReader.join()
    errReader.join()
    p.waitFor() == 0
  }

  private def cjcPath = (JetDirs.cjcBin/"cjc").exe

  private def copyCjVMLaunchers() = {
    val launchersDir = jetHome/"profile/develop/bin"
    val cjPath = JetDirs.cjcBin/"cj"
    if (!cjPath.exists) {
      Files.copy(launchersDir/"cj", cjPath)
    }

    if (LANGUAGE_PACK.supports(JAVA)) {
      val javaPath = JetDirs.cjcBin/"java"
      if (!javaPath.exists) {
        Files.copy(launchersDir/"java", javaPath)
      }
    }
    true
  }

  private def copyCangjieJavaLib(): Unit = {
    if (LANGUAGE_PACK == CANGJIE_JAVA) {
      val sourceJarPath = jetHome/"lib/cangjie-java-lib.jar"
      val targetJarPath = JetDirs.cjcBin/"../lib/cangjie-java-lib.jar"
      if (!targetJarPath.exists) {
        Files.copy(sourceJarPath, targetJarPath)
      }
    }
  }

  private def copyCangjieStaticPdb(): Unit = {
    val pdbLocation = profilePDBLocation
    val targetPath = pdbLocation / "static"
    Files.makeDir(targetPath)
    for (f <- pdbLocation.listFiles if !f.isDirectory) {
      Files.copy(f, targetPath / f.name)
    }
  }

  // TODO: Investigate if we need full.objlib for the client build (aot + cbc std lib).
  //  This file is necessary for full aot build. Move it into the pdb directory
  private def copyObjLib(): Unit = {
    val pdbLocation = profilePDBLocation
    val source = pdbLocation / "static/full.objlib"
    val targetPath = pdbLocation / "full.objlib"
    Files.copy(source, targetPath, true)
  }

  /*
    As cjvm library contains Cangjie Standard Library in natively compiled form, it should be recompiled together with
    standard library and replaced.
  */
  private def relinkCjVMLauncherLib(log: TextOutput, errorBuf: StringBuilder): Boolean = {
    // we reuse Launcher.class just as a stub to simplify relink process
    val libDir = jetHome/"lib"
    if (!(libDir/LAUNCHER_STUB_ARCHIVE).exists) {
      return true
    }

    log.println(s"Compiling cj vm binary $CJ_DLL_NAME.so ...")

    // should correspond to LauncherLibEntryPoint.entryPoint() and CangjieInvokeInterface in runtime
    val rtExports = Seq(
      "JET_RT_LauncherLibMain",
      "CangjieRT_init",
      "CangjieRT_getCFunction",
      "CangjieRT_attachCurrentThread",
      "CangjieRT_detachCurrentThread",
      "initCangjieRuntime",
      "destroyCangjieRuntime",
      "findCangjieFunction",
      "vInvokeCJFunction",
      "invokeCJFunction",
      "vRunCJFunctionAsync",
      "runCJFunctionAsync",
      "getTaskResult",
      "releaseCJThreadHandle",

      // needed by bootstrap.cffi.RTEntryPoint.InitProtocol
      "mlvm_rt_init",
      "mlvm_get_rt_callbacks",
    )

    val jcCommand = Seq(
      JetDirs.jc(false),
      s"-stdlibCbcPath=${JetDirs.jetHome / "profile/develop/lib/lres/stdlib.cbc"}",
      s"-lookup=*.zip=$libDir",
      s"-pdbnameprefix=$CJ_DLL_NAME",
      s"-$CleanCompilation",
      LAUNCHER_STUB_ARCHIVE,
      s"-$OutputName=$CJ_DLL_NAME",
      s"+prelink",
      s"+runtimeclasses",
      s"-jet_jre_home=*{comp.dir}/../../jre", // cjvm lib will be located at jet/profile/develop/lib
      s"-$UseLibrary=$STDLIB_NAME",
      s"-add_export:=${rtExports mkString ","}",
      s"-componentClassPath=." // adding "current directory" to Java classpath (or ignored if Java not supported)
    )

    log.println(jcCommand.mkString(" "))
    log.println()
    if (!runCommand(jcCommand, log, errorBuf)) {
      return false
    }

    log.println(s"Linking cj vm binary $CJ_DLL_NAME.so ...")
    val cjvmDir = jetHome/"profile/develop/lib"
    val cjvmPath = cjvmDir/s"$CJ_DLL_NAME.so"
    val cjcCommand = Seq(
      cjcPath.toString,
      s"$CJ_DLL_NAME.o",
      "-o", cjvmPath.toString,
      "--output-type=dylib",
      "--link-options=--export-dynamic -z noexecstack",
      "-L" + jetHome/"lib/lres",
      "-ljetlowlevel",
    )

    log.println(cjcCommand.mkString(" "))
    log.println()
    val res = runCommand(cjcCommand, log, errorBuf)

    if (res) {
      Files.copy(
        Path(s"$CJ_DLL_NAME.map"),
        cjvmDir/s"$CJ_DLL_NAME.map",
        replaceExisting = true)
    }

    res
  }

  // TODO (JET-13861): Remove it when we can build pure AJ apps with CJ Jet.
  private def findProfileDirectory: Path = {
    val candidates = jetHome.listFiles.filter(pathname => pathname.isDirectory && pathname.name.startsWith("profile"))
    if (candidates.length == 1) {
      candidates.head.canonicalPath
    } else {
      null
    }
  }

  // TODO (JET-13861): Remove it when we can build pure AJ apps with CJ Jet.
  private def compileCJTool(toolName: String, jarName: String, log: TextOutput, errorBuf: StringBuilder): Boolean = {
    val jar = jetHome/"lib"/jarName
    val profileDir = findProfileDirectory
    if (profileDir == null) {
      return failed(s"compile $toolName: cannot find the profile directory")
    }
    val exeDir = profileDir / "develop" / "bin"
    val exeFile = exeDir / toolName

    if (!jar.exists) {
      return failed(s"compile $toolName: source file doesn't exist")
    }

    if (!exeDir.isDirectory) {
      return failed(s"compile $toolName: target directory doesn't exist or is not a directory")
    }

    log.println(s"Compiling cj tool: $toolName ...")
    val jcCommand = Seq(
      JetDirs.jc(false),
      "=a",
      jar.toString,
      s"-add_export:=main",
      s"-pdbnameprefix=$toolName",
      if (Env.isWorkMode) s"-$CleanCompilation" else s"+$CleanCompilation",
      s"-$OutputName=" + toolName,
      s"+genMegaObj",
      s"+$SoftFP16",
      s"+$IgnoreModuleChecksum",
      s"-jet_jre_home=$RELATIVE_JET_JRE_HOME",
      s"-$UseLibrary=$STDLIB_NAME",
    )
    log.println(jcCommand.mkString(" "))

    if (!runCommand(jcCommand, log, errorBuf)) {
      return false
    }

    log.println(s"Linking cj tool: $toolName ...")
    val cjcCommand = Seq(cjcPath.toString, s"$toolName.o", "-o", exeFile.toString, "--output-type=exe")
    log.println(cjcCommand.mkString(" "))
    runCommand(cjcCommand, log, errorBuf)
  }

  private def compileCJMap(log: TextOutput, errorBuf: StringBuilder) =
    compileCJTool(CJMAP_NAME, CJMAP_JAR, log, errorBuf)

  private def compileCJStack(log: TextOutput, errorBuf: StringBuilder) =
    compileCJTool(CJSTACK_NAME, CJSTACK_JAR, log, errorBuf)

  private def processOutput(input: TextInput, log: TextOutput, errorBuf: StringBuilder): Unit = {
    Using.resource(input) { in =>
      for (line <- in.getLines()) {
        logLine(line, log, errorBuf)
      }
    }
  }

  private def logLine(line: String, log: TextOutput, errorBuf: StringBuilder): Unit = {
    log.println(line)
    if (errorBuf != null) {
      errorBuf.append(line)
      errorBuf.append(OS.host.lineSeparator)
    }
  }

  private def failed(message: String): Boolean = {
    stderr.println(s"Failed to $message.")
    false
  }

  private val OPTIMIZECBCSTDLIB_OP = "+optimizecbcstdlib"

  private def checkGenCBC(args: Array[String]) = {
    val enablingCBC = s"+$GenCBC"
    args exists (_.equalsIgnoreCase(enablingCBC))
  }

  private def checkOptimizeCbcStdlib(args: Array[String]) =
    args exists (_.equalsIgnoreCase(OPTIMIZECBCSTDLIB_OP))

  private def checkONoCodeForCJStdLib(args: Array[String]) = {
    val option = s"+$ONoCodeForCJStdLib"
    (args exists (_.equalsIgnoreCase(option))) || (jcCfg exists (_.equalsIgnoreCase(option)))
  }

  private val NO_GENCJSTACK_OP = "+nogencjstack"
  private val NO_GENCJMAP_OP = "+nogencjmap"

  private def checkNoGenCJStack(args: Array[String]) =
    args exists (_.equalsIgnoreCase(NO_GENCJSTACK_OP))

  private def checkNoGenCJMap(args: Array[String]) =
    args exists (_.equalsIgnoreCase(NO_GENCJMAP_OP))

  private class CompilationActorImpl(private val actor: CompilationDriverModule.CompilationActor) extends CompilationActor {
    private val lock: Lock = newLock()

    override def startCompile(cuId: String, worker: Int): Unit = {
      lock.sync {
        actor.startCompile(XString(cuId), worker)
      }
    }

    override def compile(cuId: String) = actor.compile(XString(cuId))

    override def errorMessage = actor.getErrorMessage.toString
  }

  private def initCompilationDriver(env: compiler.Environment, stats: CompilerWithStats, compilerArgs: Array[String]): Unit = {
    val driverImpl: CompilationDriverModule.CompilationDriver = (projectIter, actor) => {
      new CompilationDriver(
        projectIter.map(_.toString),
        new CompilationActorImpl(actor),
        env,
        new WorkerProcessExecutor(compilerArgs, env),
        stats).doCompilation()
    }

    val workerImpl: CompilationDriverModule.CompilationWorker = actor => {
      new CompilationWorker(
        new CompilationActorImpl(actor),
        env,
        stats).startWorker()
    }

    CompilationDriverModule.setImpls(driverImpl, workerImpl)
  }
}

abstract class AOTStarter {
  protected def getPlatform: Platform[_ <: Location.IReg, _ <: Location.FReg, _ <: ABI[_ <: Location.IReg, _ <: Location.FReg]]

  protected def getOptPlatformConfig: com.huawei.excelsior.jet.compiler.opt.platforms.PlatformConfig

  protected def getWrappersPlatformConfig: com.huawei.excelsior.jet.compiler.wrappers.platforms.PlatformConfig
}
