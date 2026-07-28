/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.xpackii.jreinfo

import com.huawei.excelsior.common.util.Properties
import com.huawei.excelsior.jet.compiler.Env.targetOS
import com.huawei.excelsior.jet.compiler.xminizip.Minizip
import com.huawei.excelsior.jet.compiler.xpackii.ProgressLogger
import com.huawei.excelsior.jet.compiler.xpackii.jreinfo.CompactProfile.{COMPACT1, COMPACT2, COMPACT3, FULL}
import com.huawei.excelsior.jet.compiler.xpackii.jreinfo.JRE.jreVersion
import xscala.io.{DataInput, Files, Path}

import java.io.IOException
import scala.PartialFunction.cond
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Using

/** Describes optional components of Java SE which files should be included for the current package. */
object RTSet {
  private val JAVA_LAUNCHER_NAME = targetOS.mangleExeName("java")
  private val JAVAW_LAUNCHER_NAME = targetOS.mangleExeName("javaw")
  private val JAVA6SPLASH_LIBRARY_NAME = targetOS.mangleDllName("splashscreen")

  val LOCALES_FILE_NAME = "locales"
  val TOOLS_JAR = "tools.jar"
  private val CLASS_EXT = ".class"

  private def isJavaLauncher(f: Path) = cond(f.name) {
    case `JAVA_LAUNCHER_NAME` | `JAVAW_LAUNCHER_NAME` => true
  }

  private def isFontResource(name: String) = name == "fonts" || name.startsWith("fontconfig.")
}

/** Creates [[RTSet]] with given properties.
  *
  * @param compactProfile     one of [[CompactProfile]]s names
  * @param optionalComponents set of [[RTOptionalComponent]]s to use
  * @param locales            comma-separated list of locales, or "ALL" to include all locales, or "NONE" to include none
  * @param localeDataJar      the `localedata.jar` file
  * @param jcProperties       the `jc.properties` file
  * @param hasSplash          whether the packed application has splash
  * @param logger             the progress logger to use
  * @throws IOException if an I/O error occurred
  */
class RTSet(compactProfile: String, optionalComponents: Set[String], locales: String, localeDataJar: Path,
            jcProperties: Path, val hasSplash: Boolean, logger: ProgressLogger) {

  private val excludedLocaleResourcePackages = new mutable.HashSet[String]
  private val excludedJavaSEPackages = new mutable.HashSet[String]
  private val includedJavaSEPackages = new mutable.HashSet[String]

  private var excludedLocalesResources: Set[String] = _

  /** Each compact profile defines its own set of JRE files not intersecting with others.
    * So to have full set of JRE files for a compact profile, we need to have all smaller compact profiles in the set.
    */
  private val compactProfileSet = CompactProfile.fromString(compactProfile) match {
    case FULL     => Set(COMPACT1, COMPACT2, COMPACT3, FULL)
    case COMPACT3 => Set(COMPACT1, COMPACT2, COMPACT3)
    case COMPACT2 => Set(COMPACT1, COMPACT2)
    case COMPACT1 => Set(COMPACT1)
  }

  private val rtOptionalComponents = optionalComponents.map(RTOptionalComponent.fromString(_, logger))

  private val hasLocales = locales != "NONE"
  val hasAllLocales = locales == "ALL"

  private val localesSet: Set[String] = if (hasLocales) {
    locales.split(',').toSet
  } else {
    Set.empty
  }

  readJCProperies(jcProperties)

  if (!hasAllLocales) {
    fillExcludedLocaleResources(localeDataJar)
  }

  private def hasAPIClasses = rtOptionalComponents.contains(RTOptionalComponent.API_CLASSES)

  /** Returns whether the full JRE (rather than one of compact profiles) is included. */
  def isFullJre = compactProfileSet.contains(FULL)

  private def hasJavaFX = rtOptionalComponents.contains(RTOptionalComponent.JAVAFX)

  private def isCompactProfileFile(name: String) = jreVersion.getCompactProfileForFile(name) != null

  private def hasCompactProfileFile(name: String) = compactProfileSet.contains(CompactProfile.fromString(jreVersion.getCompactProfileForFile(name)))

  private def isExtAPIFile(name: String) = jreVersion.getExtApiForFile(name) != null

  private def hasExtAPIFile(name: String) = {
    val extApi = jreVersion.getExtApiForFile(name)
    extApi != null && rtOptionalComponents.contains(RTOptionalComponent.fromString(extApi, logger))
  }

  /** Returns whether the optional [[RTOptionalComponent.JDK_TOOLS]] component is included. */
  def hasJDKTools = rtOptionalComponents.contains(RTOptionalComponent.JDK_TOOLS)

  /** Returns whether the specified JRE file shall be included. The optional JRE files are included when the
    * respective optional component is included.
    *
    * @param f the file to check
    * @return whether to include the specified JRE file
    */
  def shouldFileBeIncluded(f: Path): Boolean = {
    val name = f.name

    if (RTSet.isJavaLauncher(f) ||
        jreVersion.isOptionalUtility(name) ||
        (RTSet.JAVA6SPLASH_LIBRARY_NAME == name && !hasSplash)) {
      false
    } else if (RTSet.isFontResource(name)) {
      isFullJre || hasJavaFX
    } else if (isCompactProfileFile(name)) {
      hasCompactProfileFile(name)
    } else if (isExtAPIFile(name)) {
      hasExtAPIFile(name)
    } else {
      val jarinfo = jreVersion.getRuntimeJarInfo(name)
      if (jarinfo != null) {
        !jarinfo.isLocale || hasLocales || (jarinfo.isAPI && hasAPIClasses)
      } else {
        true
      }
    }
  }

  /** Checks whether the contents of the specified file shall be filtered, e.g. by excluding some
    * classes/resources from a JAR file.
    *
    * @param file the file to check
    * @return `true` if the file contents shall be filtered
    */
  def shouldBeFiltered(file: Path): Boolean = {
    val name = file.name

    val jarInfo = jreVersion.getRuntimeJarInfo(name)
    if (jarInfo == null) {
      if (name.endsWith(".jar") && hasExtAPIFile(name)) {
        val extAPIInfo = jreVersion.getExtApiInfoForFile(name)
        // note special case: don't filter jfxswt.jar as we don't compile it in profile
        !extAPIInfo.copyOnlyJars && name != "jfxswt.jar"
      } else {
        false
      }
    } else if (jarInfo.isCopyOnly) {
      false
    } else if (jarInfo.isAPI && hasAPIClasses) {
      false
    } else {
      true
    }
  }

  /** Checks whether the given file is one of locales or charsets JAR files.
    *
    * @param file the file to check
    * @return whether the given file is one of locales or charsets JAR files
    */
  def isLocaleJar(file: Path) = {
    val jarInfo = jreVersion.getRuntimeJarInfo(file.name)
    (jarInfo != null) && jarInfo.isLocale
  }

  private def readJCProperies(jcProperties: Path): Unit = {
    val jcProps = Properties.load(jcProperties)

    def getListProperty(propName: String) = jcProps(propName).split(";").toList

    if (!hasAllLocales) {
      val excludedLocales = getListProperty("localeList") filterNot localesSet
      for (locale <- excludedLocales) {
        excludedLocaleResourcePackages ++= getListProperty(locale)
      }
    }
    for (cp <- CompactProfile.values) {
      val packages = getListProperty(cp.packagesProperty)
      if (compactProfileSet.contains(cp)) {
        includedJavaSEPackages ++= packages
      } else {
        excludedJavaSEPackages ++= packages
      }
    }
  }

  /** Iterates `jre/lib/ext/localedata.jar` for classes that are NOT included in current locales set.
    * Required to filter jre/lib/locales file from not included locales.
    *
    * @param localeDataJar the `localedata.jar` file
    * @throws IOException if an I/O error occurred
    */
  private def fillExcludedLocaleResources(localeDataJar: Path): Unit = {
    excludedLocalesResources = Using.resource(Minizip.openZipIterator(localeDataJar.absolutePath.toString)) { jar => jar
      .filter(s => s.endsWith(RTSet.CLASS_EXT) && (s.lastIndexOf('/') > 0))
      .filter(s => excludedLocaleResourcePackages.contains(s.substring(0, s.lastIndexOf('/'))))
      .map(s => s.substring(0, s.length - RTSet.CLASS_EXT.length))
      .toSet
    }
  }

  /** For given `basePackage` and `resourceName`, returns set of locales abbreviations (like `en-US`)
    * that are in excluded locales for the given resource to filter them out from `jre/lib/locales` file.
    *
    * @param basePackage  the base package
    * @param resourceName the resource name
    * @return the set of excluded locales for the resource
    */
  private[jreinfo] def getExcludedResourceLocales(basePackage: String, resourceName: String) = {
    assert(excludedLocalesResources != null)
    excludedLocalesResources
      .filter(s => s.startsWith(basePackage) && s.contains(resourceName))
      .map(s => s.substring(s.lastIndexOf('/') + resourceName.length + 2).replace('_', '-'))
  }

  /** Returns whether the given file is "resources.jar"
    *
    * @param f the file to check
    * @return whether the given file is "resources.jar"
    */
  def isResourcesJar(f: Path) = "resources.jar" == f.name

  @tailrec
  private def isFromIncludedPackage(pack: String): Boolean = {
    if (pack != null) {
      if (includedJavaSEPackages.contains(pack)) {
        true
      } else if (excludedJavaSEPackages.contains(pack)) {
        false
      } else {
        val lastSlash = pack.lastIndexOf('/')
        if (lastSlash > 0) {
          isFromIncludedPackage(pack.substring(0, lastSlash))
        } else {
          false
        }
      }
    } else {
      false
    }
  }

  /** Returns `true` if resource is from included Java SE packages. */
  def isResourceJar(fileInJar: String): Boolean = {
    if (fileInJar.startsWith("META-INF/services/")) {
      // we keep only XmlPropertiesProvider for Compact2 profile.
      isFullJre || (compactProfileSet.contains(COMPACT2) && "META-INF/services/sun.util.spi.XmlPropertiesProvider" == fileInJar)
    } else if (fileInJar.startsWith("META-INF/")) {
      true
    } else {
      val pack = fileInJar.substring(0, fileInJar.lastIndexOf('/'))
      isFromIncludedPackage(pack)
    }
  }

  /** Returns `true` if class is from included Java SE packages. */
  def isCompactProfileClass(fileInJar: String): Boolean = {
    val pack = fileInJar.substring(0, fileInJar.lastIndexOf('/'))
    isFromIncludedPackage(pack)
  }
}