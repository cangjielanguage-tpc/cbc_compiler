/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package build

import build.Utils.{ProjectOps, directorySize, showAllFiles}
import sbt.*
import sbt.Keys.*
import sbt.plugins.JUnitXmlReportPlugin
import sbtassembly.*
import sbtassembly.AssemblyKeys.*

import scala.sys.process._

object Build {

  lazy val projectRoot = file(".")

  lazy val env = Env.load(projectRoot / "env.properties")

  /** Describes whether Scala library be bootstrapped or not.
    *
    * Important for testing, since test couldn't be run in bootstrapped mode.
    */
  lazy val bootstrapped = env.xscalaBootstrapped match {
    case "true"  => true
    case "false" => false
  }

  /** Describes virtual machine that will execute compiler code. */
  lazy val hostVM = env.xscalaVM match {
    case "jet" => HostVM.JET
    case "jdk" => HostVM.JDK
  }

  /** Describes whether compiler is built independent from JET runtime or not. */
  lazy val jcStandalone = env.jcStandalone match {
    case "true"  => true
    case "false" => false
  }

  lazy val thisBuildSettings = Def.settings(
    scalaVersion := "3.3.3",
    organization := "com.huawei.excelsior",

    scalacOptions ++= Seq(
      "-release", "8",
      "-source", "3.0",
      "-explain",
      "-explain-types",
      "-deprecation",
      "-unchecked",
      s"-Xplugin:${projectRoot / "scala/plugins/java-friendly-enums/target/plugin.jar"}",
    ),

    javacOptions ++= {
      val javaVersion = System.getProperty("java.version")
      if (javaVersion != null && javaVersion.startsWith("1.")) {
        Seq.empty
      } else {
        Seq("--release", "8")
      }
    },

    // Setup `env` setting to observe environment settings from SBT REPL session
    Env.settingKey.withRank(KeyRanks.Invisible) := env,

    // Setup `bootstrapped` setting to observe it from SBT REPL session
    SettingKey[Boolean]("bootstrapped", "Describes should Scala library be bootstrapped or not.", KeyRanks.Invisible) := bootstrapped,

    // Do not add dependency to stdlib when we compile it by ourselves
    autoScalaLibrary := !bootstrapped,

    Global / onLoad := (Global / onLoad).value andThen { state =>
      if (!bootstrapped && hostVM == HostVM.JET) {
        System.err.println(s"[error] Compiler cannot be compiled with hostVM=$hostVM and bootstrapped=$bootstrapped")
        sys.exit(1)
      }
      state
    }
  )

  lazy val javaTestSettings = Def.settings(
    libraryDependencies ++= Seq(
      "junit" % "junit" % "4.12" % Test,
      "org.easymock" % "easymock" % "3.0" % Test,
    ),
  )

  lazy val commonTestSettings = Seq(
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % Test,
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oIC"),
    Test / run / javaOptions += "-ea",
    Test / parallelExecution := false,
  )

  lazy val asmSettings = Def.settings(
    libraryDependencies ++= Seq(
      "org.ow2.asm" % "asm" % "9.7",
      "org.ow2.asm" % "asm-commons" % "9.7",
      "org.ow2.asm" % "asm-tree" % "9.7"
    )
  )

  lazy val flatbuffersVersion: String = {
    val versionLine = s"${env.flatc} --version".!!.trim
    val version = versionLine.stripPrefix("flatc version ").trim
    println(s"[info] Flatbuffers version detected from flatc: $version")
    version
  }

  lazy val flatbuffersSettings = Def.settings(
    libraryDependencies += "com.google.flatbuffers" % "flatbuffers-java" % flatbuffersVersion,
  )

  lazy val commonSourceLayout = Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src")
  lazy val commonTestLayout   = Test    / unmanagedSourceDirectories := Seq(baseDirectory.value / "test")

  lazy val commonSourceTestLayout = Seq(commonSourceLayout, commonTestLayout)

  lazy val all = (project in projectRoot)
    .settings(
      addCommandAlias("test", "javaFriendlyEnums/assembly;" + (if (bootstrapped) "tests/testBootstrappedStub" else "tests/test")),
      addCommandAlias("jar", "javaFriendlyEnums/assembly;compiler/assembly"),
      addCommandAlias("compile", "javaFriendlyEnums/assembly;compiler/compile"),
      addCommandAlias("clean", "all/clean;all/cleanAll"),
      addCommandAlias("jit-test-jar", "javaFriendlyEnums/assembly;testCompilerJIT/assembly"),

      TaskKey[Unit]("cleanAll") := Def.task {
        val log = streams.value.log
        val baseDir = baseDirectory.value

        log.info("Removing all target dirs")
        val targetDirs = (baseDir ** "target").get.filter(_.isDirectory)
        for (dir <- targetDirs if !dir.getAbsolutePath.contains("project")) {
          log.info(s"Removing $dir")
          IO.delete(dir)
        }
      }.value,
    )

  lazy val tests = (project in (projectRoot / "target/tests"))
    .aggregate(assembler, commonJavaLib, commonRtCompiler, compilerCommon, newbaseline, opt, cbcAsm)
    .aggregateWhen(hostVM == HostVM.JET)(xscalaJET)
    .settings(
      TaskKey[Unit]("testBootstrappedStub") := Def.task {
        streams.value.log.info("Tests can only be run with bootstrapped=false.")
        sys.exit(1)
      }.value,
    )

  lazy val compiler = (project in file("core/compiler"))
    .aggregate(compilerAOT)
    .aggregateWhen(env.languagePack.contains("java"))(compilerJIT)
    .disablePlugins(AssemblyPlugin) // Leave JAR generation only for aggregated projects

  private def compilerAssemblySettings(alwaysExcludeScalaStdLib: Boolean = false, keepTasty: Boolean = false) =
    Def.settings(
      Compile / packageBin / packageOptions := Seq(),
      assembly / assemblyExcludedJars := {
        val classpath = (assembly / fullClasspath).value
        val excludeList = Seq("jdk8_rt") ++ (if (bootstrapped || alwaysExcludeScalaStdLib) Seq("scala-library", "scala3-library") else Seq.empty)
        classpath.filter { entry =>
          val absolutePath = entry.data.absolutePath
          excludeList exists (absolutePath.contains(_))
        }
      },

      assembly / assemblyMergeStrategy := {
        case PathList(ps *) if !keepTasty && {
          val lastEntry = ps.last
          lastEntry.endsWith(".tasty") || lastEntry == "module-info.class"
        } =>
          MergeStrategy.discard

        case x =>
          val oldStrategy = (assembly / assemblyMergeStrategy).value
          oldStrategy(x)
      },
    )

  lazy val compilerAOT = (project in file("core/compiler"))
    .aggregate(compilerAOTVMDependent)
    .dependsOn(starterAOT)
    .dependsOnWhen(jcStandalone)(xscalaVMDependent)
    .settings(
      compilerAssemblySettings(alwaysExcludeScalaStdLib = !jcStandalone),
      target := baseDirectory.value / "target/aot",
      assembly / mainClass := Some("com.huawei.excelsior.jet.compiler.starter.AOTStarter"),
      assembly / assemblyOutputPath := target.value / "aot.jar",
    )

  lazy val compilerAOTVMDependent = (project in file("core/compiler"))
    .dependsOn(xscalaVMDependent)
    .settings(
      compilerAssemblySettings(keepTasty = (hostVM == HostVM.JET)),
      target := baseDirectory.value / s"target/aot-$hostVM",
      assembly / assemblyOutputPath := target.value / s"aot-$hostVM.jar",
    )

  lazy val compilerJIT = (project in file("core/compiler"))
    .dependsOn(starterJIT, xscalaVMDependent)
    .settings(
      compilerAssemblySettings(),
      target := baseDirectory.value / "target/jit",
      assembly / assemblyOutputPath := target.value / "jit.jar",
    )

  lazy val testCompilerJIT = (project in file("core/compiler"))
    .dependsOn(starterJIT % "test->test", xscalaVMDependent % "test->test", assembler % "test->test", commonJavaLib % "test->test", commonRtCompiler % "test->test", compilerCommon % "test->test")
    .settings(
      compilerAssemblySettings(),
      target := baseDirectory.value / "target/jit-test",
      assembly / assemblyOutputPath := target.value / "jit-test.jar",
      assembly / fullClasspath := (assembly / fullClasspath).value ++ (Test / fullClasspath).value
    )

  lazy val xscalaVMDependent = hostVM match {
    case HostVM.JDK => xscalaVMDependentJDK
    case HostVM.JET => xscalaVMDependentJET
  }

  // Provided dependency won't be included in target JARs
  lazy val xscalaVMDependentProvided = xscalaVMDependent % "provided"

  lazy val xscalaVMDependentShare = (project in file("core/xscala-vm-dependent"))
    .dependsOn(xscalaVMDependentStub % "provided")
    .settings(
      target := baseDirectory.value / "target/share",
      Compile / unmanagedSourceDirectories := Seq(
        baseDirectory.value / "src" / "share"
      ),
      // Depend on standard library, since bootstrapped is not yet available
      autoScalaLibrary := true,
    )

  lazy val xscalaVMDependentShareBootstrapped = (project in file("core/xscala-vm-dependent"))
    .dependsOn(xscalaVMDependentStub % "provided")
    .dependsOn(xscala)
    .settings(
      target := baseDirectory.value / "target/share-bootstrapped",
      Compile / unmanagedSourceDirectories := Seq(
        baseDirectory.value / "src" / "share"
      ),
      // Now bootstrapped is available, so use it
      autoScalaLibrary := false,
    )

  lazy val xscalaVMDependentStub = (project in file("core/xscala-vm-dependent"))
    .settings(
      target := baseDirectory.value / "target/stub",
      Compile / unmanagedSourceDirectories := Seq(
        baseDirectory.value / "src" / "stub"
      ),
      // Depend on standard library, since bootstrapped is not yet available
      autoScalaLibrary := true,
    )

  lazy val xscalaVMDependentJET = (project in file("core/xscala-vm-dependent"))
    .dependsOn(
      if (bootstrapped) xscalaVMDependentShareBootstrapped
      else xscalaVMDependentShare
    )
    .settings(
      target := baseDirectory.value / "target/jet",
      Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src" / "jet"),
    )

  lazy val xscalaVMDependentJDK = (project in file("core/xscala-vm-dependent"))
    .dependsOn(
      if (bootstrapped) xscalaVMDependentShareBootstrapped
      else xscalaVMDependentShare
    )
    .settings(
      target := baseDirectory.value / "target/jdk",
      Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "src" / "jdk")
    )

  lazy val xscala = hostVM match {
    case HostVM.JDK => xscalaJDK
    case HostVM.JET => xscalaJET
  }

  private def xscalaImpl(hostVM: HostVM, suffix: String) = {
    Project(s"xscala${hostVM.toString.toUpperCase}$suffix", file("target/xscala"))
      .dependsOn(xscalaVMDependentShare % "provided")
      .settings(
        target := baseDirectory.value / s"target/$hostVM$suffix",
        Compile / sourceGenerators += Def.task {
          val dir = "src"
          val src = file(env.xscala) / dir
          val dst = baseDirectory.value / dir

          if (!src.exists) {
            println(s"Invalid xscala path '${env.xscala}'")
            sys.exit(1)
          }

          val log = streams.value.log
          if (!dst.exists() || (directorySize(src.toPath) != directorySize(dst.toPath))) {
            log.info(s"copying $src to $dst")
            IO.copyDirectory(source = src, target = dst)
          }
          val excludeDir = s"xscala-library-${hostVM.opposite}"
          showAllFiles(dst.toPath) filterNot (_.absolutePath.contains(excludeDir))
        }
      )
      .disablePlugins(JUnitXmlReportPlugin)
  }

  lazy val xscalaJDK = xscalaImpl(HostVM.JDK, suffix = "")
    .settings(
      // rt.jar is required to obtain `sun/misc/Unsafe` when running scalac on JDK9+
      //
      // We can't simply refer to rt.jar in Compile / unmanagedJars, since
      // for some reason SBT replaces such classpath entry with `file(".") / "rt.jar"`
      //
      // Therefore, we should copy it to `target` directory.
      Compile / unmanagedJars += Def.task {
        val src = file(sys.props("java.home")) / "lib/rt.jar"
        val dst = target.value / "jars/jdk8_rt.jar"

        val log = streams.value.log
        if (!dst.exists()) {
          log.info(s"copying $src to $dst")
          IO.copyFile(sourceFile = src, targetFile = dst)
        }

        dst
      }.value,
    )

  // Bootstrapped xscala-library
  // which uses non-bootstrapped xscala-library
  lazy val xscalaJET = xscalaImpl(HostVM.JET, suffix = "")
    .dependsOn(xscalaJET0 % "provided")
    .settings(
      // Manually strip out transitively included regular scala-library jars
      Compile / managedClasspath := Seq()
    )

  // Non-bootstrapped (phase zero) xscala-library
  // which uses regular scala-library
  lazy val xscalaJET0 = xscalaImpl(HostVM.JET, suffix = "0")

  lazy val chirLib = (project in file("core/chir-lib"))
    .settings(flatbuffersSettings)
    .settings(
      Compile / sourceGenerators += Def.task {
        val schema = file("core/chir-lib/PackageFormat.fbs")
        val generated = file("core/chir-lib/generated/src")
        val cache = streams.value.cacheDirectory / "chir-lib-cache"
        
        val cached = FileFunction.cached(cache, FilesInfo.hash, FilesInfo.exists) { _ =>
          val chirPackage = "com.huawei.excelsior.jet.compiler.chir"
          IO.delete(generated)
          Seq(env.flatc,
            "--no-warnings", "--java",
            "-o", generated.toString,
            "--java-package-prefix", chirPackage,
            schema.toString
            ).!!
          showAllFiles(generated.toPath).toSet
        }
        cached(Set(schema)).toSeq
      }.taskValue,
    )

  lazy val assembler = (project in file("core/assembler"))
    .dependsOn(xscalaVMDependentProvided, commonJavaLib)
    .settings(commonSourceTestLayout, commonTestSettings)
    .disablePlugins(JUnitXmlReportPlugin)

  lazy val cbcAsm = (project in file("core/cbc-asm"))
    .dependsOn(assembler % "test->test;compile->compile", commonJavaLib % "test->test;compile->compile", xscalaVMDependent)
    .settings(commonSourceTestLayout, commonTestSettings, javaTestSettings)
    .settings(
      compilerAssemblySettings(),
      assembly / mainClass := Some("com.huawei.excelsior.jet.assembler.cbc.AsmParser"),
      assembly / assemblyOutputPath := target.value / "cbc-asm.jar",
    )
    .disablePlugins(JUnitXmlReportPlugin)

  lazy val commonJavaLib = (project in file("core/common-java-lib"))
    .dependsOn(xscalaVMDependentProvided)
    .settings(commonTestSettings)
    .settings(
      Compile / unmanagedSourceDirectories := Seq(baseDirectory.value / "share" / "src"),
      Compile / sourceManaged := baseDirectory.value / "generated" / "src",
      Test    / unmanagedSourceDirectories := Seq(baseDirectory.value / "share" / "test"),

      Compile / sourceGenerators += Def.task {
        val file = (Compile / sourceManaged).value / "com/huawei/excelsior/common/JETConfig.scala"
        val content =
          s"""package com.huawei.excelsior.common
             |
             |private[common] object JETConfig {
             |  val targetOS = "${env.os}"
             |  val targetCPU = "${env.arch}"
             |  val buildMode = "${env.mode}"
             |  val languagePack = "${env.languagePack}"
             |  val jcStandalone = $jcStandalone
             |}
             |""".stripMargin

        if (!(file.exists() && IO.read(file) == content)) {
          IO.write(file, content)
        }
        Seq(file)
      }.taskValue,
    )
    .disablePlugins(JUnitXmlReportPlugin)

  lazy val commonRtCompiler = (project in file("core/common-rt-compiler"))
    .dependsOn(commonJavaLib, xscalaVMDependent % "test->test;provided")
    .settings(commonSourceTestLayout, commonTestSettings)
    .disablePlugins(JUnitXmlReportPlugin)

  lazy val cangjieJavaClassGenImpl = (project in file("core/compiler/src/cangjie-java-class-gen-impl"))
    .dependsOn(commonJavaLib, compilerCommon, xscalaVMDependentProvided)
    .settings(asmSettings, commonSourceLayout)

  lazy val compilerCommon = (project in file("core/compiler/src/common"))
    .dependsOn(
      assembler % "test->test;compile->compile", commonRtCompiler,
      commonJavaLib % "test->test;compile->compile", xscalaVMDependentProvided, chirLib)
    .settings(commonSourceTestLayout, commonTestSettings, javaTestSettings)
    .disablePlugins(JUnitXmlReportPlugin)

  lazy val lambdaTypeGenImpl = (project in file("core/compiler/src/lambda-type-gen-impl"))
    .dependsOn(compilerCommon, commonJavaLib, commonRtCompiler, assembler, o2Lib, xscalaVMDependentProvided)
    .settings(commonSourceLayout)

  lazy val lazyJitStubsGenerator = (project in file("core/compiler/src/lazy-jit-stubs-generator"))
    .dependsOn(assembler, compilerCommon, commonJavaLib, newbaselineCodeGenerator, commonRtCompiler, xscalaVMDependentProvided)
    .settings(commonSourceLayout)

  lazy val newbaseline = (project in file("core/compiler/src/newbaseline"))
    .dependsOn(
      assembler, compilerCommon % "test->test;compile->compile", commonJavaLib,
      newbaselineCodeGenerator, commonRtCompiler, xscalaVMDependentProvided, lazyJitStubsGenerator)
    .settings(commonSourceTestLayout, commonTestSettings)
    .disablePlugins(JUnitXmlReportPlugin)

  lazy val newbaselineCodeGenerator = (project in file("core/compiler/src/newbaseline-code-generator"))
    .dependsOn(assembler, compilerCommon, commonRtCompiler, commonJavaLib, xscalaVMDependentProvided)
    .settings(commonSourceLayout)

  lazy val o2Lib = (project in file("core/compiler/src/o2-lib"))
    .dependsOn(
      assembler, compilerCommon, commonRtCompiler, commonJavaLib, xscalaVMDependentProvided,
      lazyJitStubsGenerator, xpackii, xminizip, chirLib)
    .settings(
      Compile / unmanagedSourceDirectories := Seq(
        baseDirectory.value / "src",
        baseDirectory.value / ".." / "symlevel-light" / "src",
      )
    )

  lazy val opt = (project in file("core/compiler/src/opt"))
    .dependsOn(compilerCommon % "test->test;compile->compile", assembler, commonRtCompiler, commonJavaLib, xscalaVMDependentProvided, xminizip)
    .settings(commonSourceTestLayout, commonTestSettings, javaTestSettings)
    .disablePlugins(JUnitXmlReportPlugin)

  private def starter(component: String) = {
    require(component == "aot" || component == "jit")

    Project(s"starter${component.toUpperCase}", file("core/compiler/src/starter") / component)
      .dependsOnWhen(component == "aot" && env.languagePack == "cangjie-java")(cangjieJavaClassGenImpl)
      .dependsOnWhen(component == "aot" && (env.languagePack.contains("java") || env.languagePack == "scala"))(lambdaTypeGenImpl)
      .dependsOnWhen(env.languagePack.contains("java"))(verifier, verifierImpl)
      .dependsOnWhen(component == "aot")(
        o2Lib, opt, xpackii, xminizip
      )
      .dependsOnWhen(component == "jit")(
        newbaseline, lazyJitStubsGenerator
      )
      .dependsOn(
        compilerCommon, commonJavaLib, commonRtCompiler, assembler, newbaselineCodeGenerator,
        wrapperCompiler, xscalaVMDependentProvided
      )
      .settings(
        Compile / unmanagedSourceDirectories := Seq(
          baseDirectory.value / "share" / "src",
          baseDirectory.value / "language-pack" / env.languagePack / "src",
          baseDirectory.value / env.arch / "src",
        )
      )
  }

  lazy val starterAOT = starter("aot")
  lazy val starterJIT = starter("jit")

  lazy val symlevelLight = (project in file("core/compiler/src/symlevel-light"))
    .dependsOn(assembler, commonRtCompiler, compilerCommon, commonJavaLib, o2Lib, lazyJitStubsGenerator)
    .settings(commonSourceLayout)

  lazy val verifier = (project in file("core/compiler/src/verifier"))
    .dependsOn(assembler, commonRtCompiler, compilerCommon, commonJavaLib, xscalaVMDependentProvided)
    .settings(commonSourceLayout)

  lazy val verifierImpl = (project in file("core/compiler/src/verifier-impl"))
    .dependsOn(assembler, commonRtCompiler, compilerCommon, commonJavaLib, o2Lib, verifier, xscalaVMDependentProvided)
    .settings(commonSourceLayout)

  lazy val wrapperCompiler = (project in file("core/compiler/src/wrapper-compiler"))
    .dependsOn(assembler, commonRtCompiler, compilerCommon, commonJavaLib, newbaselineCodeGenerator, xscalaVMDependentProvided)
    .settings(commonSourceLayout)

  lazy val xminizip = (project in file("core/compiler/src/xminizip"))
    .dependsOn(commonJavaLib, xscalaVMDependentProvided)
    .settings(commonSourceLayout)

  lazy val xpackii = (project in file("core/compiler/src/xpackii"))
    .dependsOn(commonJavaLib, commonRtCompiler, compilerCommon, xminizip, xscalaVMDependentProvided)
    .settings(commonSourceLayout)

  lazy val javaFriendlyEnums = (project in file("scala/plugins/java-friendly-enums"))
    .settings(commonSourceLayout)
    .settings(
      version := "1.0",
      Compile / unmanagedResourceDirectories ++= Seq(baseDirectory.value / "resources"),
      Compile / unmanagedJars ++= scalaInstance.value.compilerJars.toSeq,
      scalacOptions := Seq(
        "-release", "8",
        "-source", "3.0",
      ),

      autoScalaLibrary := true,
      compilerAssemblySettings(),
      assembly / assemblyOutputPath := target.value / "plugin.jar",
    )

}
