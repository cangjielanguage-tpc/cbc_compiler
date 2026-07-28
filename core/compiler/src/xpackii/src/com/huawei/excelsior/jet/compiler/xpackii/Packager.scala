/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.xpackii

import com.huawei.excelsior.common.Arch.{AMD64, ARM64}
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.common.JetDirs
import com.huawei.excelsior.jet.compiler.Env.{targetArch, targetOS}
import com.huawei.excelsior.jet.compiler.xminizip.Minizip
import com.huawei.excelsior.jet.compiler.xminizip.Minizip.{ZwInMemHandle, xminizipLibName}
import com.huawei.excelsior.jet.compiler.xpackii.jreinfo.{CompactProfile, LocalesFileProcessor, RTSet}
import xscala.io.{Files, Path}
import xscala.properties.OS
import xscala.util.StringOps.*

import java.io.IOException
import scala.collection.mutable

/** Prepares resulting zip package containing jet-compiled executable, runtime folder according specified
  * compact profile, optional components and locales, extra files that have to be included to the resulting package.
  */
object Packager {
  private val RT_FOLDER = Path("rt")
  private val CLASS_EXT = ".class"
  private val EXTERNAL_JEXPORT_EXT = ".jexp"
  private val NO_COMPILED_CLASSES_SET: Set[String] = null

  private val tomcatDirsToFilter = Set("bin", "temp", "logs", "work") // the set of directories that should be filtered
  private val TOMCAT_BIN_FILTER_EXT = Array(".map", ".jexp", ".jetjrehome")

  private def getClassName(classFile: String) = classFile.substring(0, classFile.length - CLASS_EXT.length).replace('/', '.')

  private def checkExists(f: Path, logger: ProgressLogger) = {
    if (!f.exists) logger.fatalError(s"File ${f.absolutePath} does not exist")
    f
  }

  private def toSet(s: String, splitRegex: String) = s.split(splitRegex).toSet

  private def readCompiledClassesSet(executable: String) = {
    val ext = if (executable.endsWith(targetOS.getDllFileExtension)) {
      targetOS.getDllFileExtension
    } else if (executable.endsWith(targetOS.getExeFileExtension)) {
      targetOS.getExeFileExtension
    } else {
      ""
    }
    // must be the same as defined in jc.tem
    val jExportFilePath = executable.substring(0, executable.length - ext.length) + EXTERNAL_JEXPORT_EXT
    val jExportFile = Path(jExportFilePath)
    if (jExportFile.exists && jExportFile.isRegularFile) {
      Files.readAllLines(jExportFile).filter(_.nonEmpty).toSet
    } else {
      NO_COMPILED_CLASSES_SET
    }
  }

  private def packRT(jetJreHome: String, compactProfile: String, optRTFiles: String, locales: String,
                     hasSplash: Boolean, zipFile: Minizip.Writer,
                     compiledClassesSet: Set[String], logger: ProgressLogger): Unit = {
    val localeDataFile = checkExists(Path(jetJreHome) / "lib/ext/localedata.jar", logger)

    val jcProps = checkExists(JetDirs.bin / "jc.properties", logger)

    val rtSet = new RTSet(compactProfile,
      if (optRTFiles.equalsIgnoreCase("NONE")) Set.empty else toSet(optRTFiles, ","),
      locales.asciiToUpperCase, localeDataFile, jcProps, hasSplash, logger)

    val rtFilter = new Filter() {
      override def accept(file: Path) = rtSet.shouldFileBeIncluded(file)

      override def filterToBytes(f: Path) = if (!rtSet.shouldBeFiltered(f) && !rtSet.hasAllLocales && LocalesFileProcessor.isLocalesFile(f)) {
        new LocalesFileProcessor(rtSet).processLocalesFile(f).getBytes()
      } else {
        null
      }

      // in addition to the general filters, filter out jar index as it may become invalid after filtering
      private def jarFilter(entry: String): Boolean = !entry.equalsIgnoreCase("META-INF/INDEX.LIST")

      override def filterToZipInMem(f: Path): ZwInMemHandle = if (rtSet.shouldBeFiltered(f)) {
        val filter: String => Boolean = if ((compiledClassesSet eq NO_COMPILED_CLASSES_SET) || rtSet.isLocaleJar(f)) {
          entry => !entry.endsWith(CLASS_EXT)
        } else {
          def notCompiled(entry: String): Boolean = !compiledClassesSet.contains(getClassName(entry))
          def isCompactProfilesNotCompiled(entry: String): Boolean = rtSet.isCompactProfileClass(entry) && notCompiled(entry)
          entry => !entry.endsWith(CLASS_EXT) || isCompactProfilesNotCompiled(entry)
        }
        Minizip.filterZipToMem(f, entry => jarFilter(entry) && filter(entry))
      } else if (!rtSet.isFullJre && rtSet.isResourcesJar(f)) {
        Minizip.filterZipToMem(f, entry => jarFilter(entry) && rtSet.isResourceJar(entry))
      } else {
        Minizip.ZIP_IN_MEM_INVALID
      }
    }

    val jetJreHomeDir = checkExists(Path(jetJreHome), logger).canonicalPath

    ArchiveUtils.putDirectoryToArchive(RT_FOLDER, jetJreHomeDir,
      (jetJreHomeDir / "lib").canonicalPath, zipFile, rtFilter)
    ArchiveUtils.putDirectoryToArchive(RT_FOLDER, jetJreHomeDir,
      (jetJreHomeDir / "bin").canonicalPath, zipFile, rtFilter)

    if (rtSet.hasJDKTools) {
      zipFile.putFileToArchive(Path(jetJreHome) / "../lib" / RTSet.TOOLS_JAR,
        RT_FOLDER / "lib" / RTSet.TOOLS_JAR)
    }

    def packToLibDir(name: String): Unit = {
      val src = jetJreHomeDir / "../develop/lib" / name
      if (src.exists) {
        val libDir = if (targetOS.isWindows) {
          "bin"
        } else if (targetArch == AMD64) {
          "lib/amd64"
        } else if (targetArch == ARM64) {
          "lib/arm64"
        } else {
          shouldNotReachHere()
        }
        zipFile.putFileToArchive(src, RT_FOLDER / s"$libDir/$name")
      }
    }

    packToLibDir(xminizipLibName)
  }

  /** Packs general (non-Tomcat) application.
    *
    * @param executable     the application executable
    * @param jetJreHome     path to JET JRE
    * @param outputFile     the output file
    * @param compactProfile one of [[CompactProfile]]s names
    * @param optRTFiles     comma-separated list of optional RT components, or "ALL" to include all of them,
    *                       or "NONE" to include none
    * @param locales        comma-separated list of locales, or "ALL" to include all locales, or "NONE" to include none
    * @param hasSplash      whether the packed application has splash
    * @param extraFiles     the comma-separated list of extra files to pack
    * @param logger         the progress logger to use
    * @throws IOException if an I/O error occurred
    */
  def pack(executable: String, jetJreHome: String, outputFile: String, compactProfile: String,
           optRTFiles: String, locales: String, hasSplash: Boolean, extraFiles: String,
           logger: ProgressLogger): Unit = {
    logger.progressStart()
    val zipFile = Minizip.openWriter(outputFile)
    try {
      val compiledClassesSet = readCompiledClassesSet(executable)
      if (jetJreHome != null) {
        packRT(jetJreHome, compactProfile, optRTFiles, locales, hasSplash, zipFile, compiledClassesSet, logger)
      }
      val execFile = checkExists(Path(executable), logger)
      // Minizip is responsible for preserving executable attribute of the original file when put it into archive
      zipFile.putFileToArchive(execFile, Path(execFile.name))
      if (extraFiles.nonEmpty) {
        for (extra <- extraFiles.split(",")) {
          val extraFile = checkExists(Path(extra), logger)
          if (extraFile.isDirectory) {
            ArchiveUtils.putDirectoryToArchive(Path(extraFile.name), extraFile.canonicalPath, extraFile.canonicalPath, zipFile)
          } else {
            zipFile.putFileToArchive(extraFile, Path(extraFile.name))
          }
        }
      }
      logger.progressEnd(Path(outputFile))
    } finally {
      if (zipFile != null) zipFile.close()
    }
  }

  private def getToHideClassesMap(toHideClassesBundles: String) = {
    if (toHideClassesBundles.nonEmpty) {
      val toHideClassesMap = mutable.Map.empty[String, String]
      for (s <- toHideClassesBundles.split(";")) {
        val pos = s.indexOf('%')
        assert(pos > 0, "invalid tohideclassesbundles packaging option")
        toHideClassesMap.put(s.substring(0, pos), s.substring(pos + 1))
      }
      toHideClassesMap.toMap
    } else {
      Map.empty[String, String]
    }
  }

  private def hideEntry(relativeEntryName: String, toHideClassesBundles: Map[String, String],
                        compiledClassesSet: Set[String]): Option[Boolean] = {
    if (toHideClassesBundles.contains(relativeEntryName)){
      return Some(true)
    }
    for (hideClassesDir <- toHideClassesBundles.keys) {
      if (relativeEntryName.startsWith(hideClassesDir + "/") && relativeEntryName.endsWith(CLASS_EXT)) {
        if (compiledClassesSet == null) {
          return Some(false)
        }
        val className = getClassName(relativeEntryName.substring(hideClassesDir.length + 1)) + "%" +
          toHideClassesBundles.get(hideClassesDir)
        return Some(!compiledClassesSet.contains(className))
      }
    }
    None
  }

  // == Packing Tomcat web applications ==

  private def getTomcatFilter(sourceDir: Path, packedBundles: Set[String], toHideClassesBundles: Map[String, String],
                              compiledClassesSet: Set[String]): Filter = new Filter() {
    override def accept(f: Path): Boolean = {
      if (f == sourceDir) {
        true
      } else {
        val relativeName = f.relativeTo(sourceDir).name
        hideEntry(relativeName, toHideClassesBundles, compiledClassesSet).getOrElse(
          !packedBundles.contains(relativeName) && !tomcatDirsToFilter.contains(relativeName))
      }
    }
  }

  /** Packs Tomcat application.
    *
    * @param executable           the application executable
    * @param jetJreHome           path to JET JRE
    * @param outputFile           the output file
    * @param compactProfile       one of [[CompactProfile]]s names
    * @param optRTFiles           comma-separated list of optional RT components, or "ALL" to include all of them,
    *                             or "NONE" to include none
    * @param locales              comma-separated list of locales, or "ALL" to include all locales,
    *                             or "NONE" to include none
    * @param hasSplash            whether the packed application has splash
    * @param appHome              the home directory of Tomcat application
    * @param packedBundles        the Tomcat application bundles to pack
    * @param toHideClassesBundles semicolon-separated list of classes bundles to exclude
    * @param binFolder            Tomcat application "bin" directory
    * @param logger               the progress logger to use
    * @throws IOException if an I/O error occurred
    */
  def packTomcat(executable: String, jetJreHome: String, outputFile: String, compactProfile: String,
                 optRTFiles: String, locales: String, hasSplash: Boolean, appHome: String, packedBundles: String,
                 toHideClassesBundles: String, binFolder: String, logger: ProgressLogger): Unit = {
    logger.progressStart()
    val zipFile = Minizip.openWriter(outputFile)
    try {
      val compiledClassesSet = readCompiledClassesSet(executable)
      packRT(jetJreHome, compactProfile, optRTFiles, locales, hasSplash, zipFile, compiledClassesSet, logger)

      val binFolderCanonical = checkExists(Path(binFolder), logger).canonicalPath
      ArchiveUtils.putDirectoryToArchive(Path("bin"), binFolderCanonical, binFolderCanonical, zipFile, new Filter() {
        override def accept(f: Path) = !TOMCAT_BIN_FILTER_EXT.exists(f.name.endsWith(_))
      })

      val appHomeDir = checkExists(Path(appHome), logger)
      val appHomeCanonical = appHomeDir.canonicalPath

      val toHideClassesMap = getToHideClassesMap(toHideClassesBundles)

      ArchiveUtils.putDirectoryToArchive(Path.dot, appHomeCanonical, appHomeCanonical, zipFile,
        getTomcatFilter(appHomeDir, toSet(packedBundles, ";"), toHideClassesMap, compiledClassesSet))

      logger.progressEnd(Path(outputFile))
    } finally {
      if (zipFile != null) zipFile.close()
    }
  }

  /** Unzips `zipArchive` zip to `targetDir` directory.
    *
    * @param zipArchive  zip to extract
    * @param targetDir   target directory
    * @param cleanTarget whether to clean target directory in case it is not empty
    * @param logger      the progress logger to use
    */
  def unzip(zipArchive: String, targetDir: String, cleanTarget: Boolean, logger: ProgressLogger): Unit = {
    try {
      if (!ArchiveUtils.ensureIsEmptyDir(targetDir, cleanTarget)) {
        logger.fatalError(s"The target path doesn't denote an empty directory: \"$targetDir\"" + OS.host.lineSeparator +
          "Use +cleanTarget to force to clean it.")
      }
      ArchiveUtils.unzip(zipArchive, targetDir)
    } catch {
      case e: IOException => logger.fatalError(e)
    }
    logger.progress(s"The image is successfully created at \"$targetDir\"")
  }
}