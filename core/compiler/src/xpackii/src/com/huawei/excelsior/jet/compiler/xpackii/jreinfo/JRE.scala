/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.xpackii.jreinfo

import com.huawei.excelsior.common.JetDirs
import com.huawei.excelsior.jet.compiler.Env.targetOS
import com.huawei.excelsior.jet.compiler.xpackii.jreinfo.JRE.{CompactProfileInfo, ExtensionAPIInfo, RuntimeJarInfo}
import xscala.util.StringTokenizer

import scala.collection.mutable
import scala.collection.mutable.Buffer

object JRE {
  /** Information about a Java SE JAR file. */
  case class RuntimeJarInfo(name: String,
                            private val api: Boolean,
                            isLocale: Boolean, // means that this jar is a locale or charsets jar
                            isCopyOnly: Boolean) { // means that it can not be used filtering for this jar

    /** Means that this jar should be included if "api-classes" optional component is included. */
    def isAPI = api && !isCopyOnly
  }

  /** API that is located in lib/ext folder of JRE. It may have jars, resources and natives. */
  case class ExtensionAPIInfo(name: String, jars: List[String], copyOnlyJars: Boolean,
                              nativeLibs: List[String], resources: List[String], includeByDefault: Boolean)

  /** Compact Profile info. It may have resources and natives. */
  case class CompactProfileInfo(name: String, nativelibs: List[String], resources: List[String])

  private val COPYONLY_PREFIX = "copyonly:"
  private val NOJARS = "nojars"
  private val NONATIVELIBS = "nonativelibs"
  private val NORESOURCES = "noresources"

  lazy val jreVersion: JRE = {
    val iniFilesDir = JetDirs.versions
    val iniFilesDirPath = iniFilesDir.canonicalPath
    assert(iniFilesDir.exists, s"The directory \"$iniFilesDirPath\" does not exist")
    val iniFiles = iniFilesDir.listFiles.filter(_.name.endsWith(".ini"))

    assert(iniFiles != null, s"No ini files found in $iniFilesDirPath")
    assert(iniFiles.length == 1, s"Too many ini files found in \"$iniFilesDirPath\"")

    val sections = new XJETIniParser(iniFiles.head.absolutePath).sections

    val runtimeSec = sections.get("runtime").orNull
    assert(runtimeSec != null, "Wrong version.ini format: 'no runtime section'")

    val jrefilesetSection = sections.get("fileset").orNull
    assert(jrefilesetSection != null, "Wrong version.ini format: 'no jrefileset section'")

    val opts = jrefilesetSection("ro_opt_jars")
    val runtimeJars = opts.map { jar =>
      val tok = StringTokenizer(jar)
      assert(tok.countTokens <= 4, "Wrong version.ini format: 'ro_opt_jars'")
      var api = false
      var locale = false
      var copyonly = false
      val name = tok.nextToken()
      while (tok.hasMoreTokens) {
        val token = tok.nextToken()
        if (token.equalsIgnoreCase("api")) {
          api = true
        } else if (token.equalsIgnoreCase("locale")) {
          locale = true
        } else if (token.equalsIgnoreCase("copyonly")) {
          copyonly = true
        } else {
          assert(false, s"Wrong version.ini format: 'ro_opt_jars' - $token")
        }
      }
      RuntimeJarInfo(name, api, locale, copyonly)
    }

    val extApis = runtimeSec.getOrElse("ext_api", Buffer.empty)
    val extApiInfo = extApis.map { description =>
      val st = StringTokenizer(description)
      assert(st.countTokens >= 4, "Wrong version.ini format: Extension APIs section is invalid")
      val name = st.nextToken()
      var jars = st.nextToken()
      var copyOnlyJars = false
      if (jars.startsWith(COPYONLY_PREFIX)) {
        copyOnlyJars = true
        jars = jars.substring(COPYONLY_PREFIX.length)
      }
      val nativelibs = st.nextToken()
      val resourses = st.nextToken()
      val includeByDefault = st.hasMoreTokens && "includeByDefault" == st.nextToken()
      ExtensionAPIInfo(name,
        semicolonSeparatedToList(NOJARS, jars),
        copyOnlyJars,
        semicolonSeparatedToList(NONATIVELIBS, nativelibs),
        semicolonSeparatedToList(NORESOURCES, resourses),
        includeByDefault)
    }

    val profiles = runtimeSec.get("profile").orNull
    assert(profiles != null && profiles.nonEmpty, "Wrong version.ini format: no profile's section")
    val profileInfos = profiles.map { description =>
      val st = StringTokenizer(description)
      assert(st.countTokens == 3, "Wrong version.ini format: Profiles section is invalid")
      val name = st.nextToken()
      val nativelibs = st.nextToken()
      val resourses = st.nextToken()

      CompactProfileInfo(name,
        semicolonSeparatedToList(NONATIVELIBS, nativelibs),
        semicolonSeparatedToList(NORESOURCES, resourses))
    }

    new JRE(jrefilesetSection("roo"), runtimeJars, extApiInfo, profileInfos)
  }

  private def semicolonSeparatedToList(emptyValue: String, value: String): List[String] = if (emptyValue == value) {
    List.empty
  } else {
    value.split(";").toList
  }
}

final class JRE private(optionalUtilities: Buffer[String], rtJars: Buffer[RuntimeJarInfo],
                        extApiInfo: Buffer[ExtensionAPIInfo], profileInfo: Buffer[CompactProfileInfo]) {

  private val optionalUtilitiesSet = optionalUtilities.map(file => file.substring(file.lastIndexOf('/') + 1)).toSet

  private val runtimeJarsMap = new mutable.HashMap[String, RuntimeJarInfo]
  for (runtimeJar <- rtJars) {
    val jarName = runtimeJar.name
    val slash = jarName.lastIndexOf('/')
    runtimeJarsMap.put(jarName.substring(slash + 1), runtimeJar)
  }

  private val compactProfileFiles = new mutable.HashMap[String, String]
  for (cpInfo <- profileInfo) {
    for (natlib <- cpInfo.nativelibs) {
      compactProfileFiles.put(targetOS.mangleDllName(natlib), cpInfo.name)
    }
    for (resource <- cpInfo.resources) {
      val resourcePath = if (resource.endsWith("/*")) {
        resource.substring(0, resource.length - 2)
      } else {
        resource
      }
      val pos = resourcePath.lastIndexOf('/')
      val resourceName = if (pos > 0) {
        resourcePath.substring(pos + 1)
      } else {
        resourcePath
      }
      compactProfileFiles.put(resourceName, cpInfo.name)
    }
  }

  private val extAPIsFiles = new mutable.HashMap[String, String]
  private val extAPIs = new mutable.HashMap[String, ExtensionAPIInfo]
  for (extApi <- extApiInfo) {
    extAPIs.put(extApi.name, extApi)
    for (natlib <- extApi.nativeLibs) {
      extAPIsFiles.put(targetOS.mangleDllName(natlib), extApi.name)
    }
    for (resource <- extApi.resources) {
      val pos = resource.lastIndexOf('/')
      val resourceName = if (pos > 0) {
        resource.substring(pos + 1)
      } else {
        resource
      }
      extAPIsFiles.put(resourceName, extApi.name)
    }
    for (jar <- extApi.jars) {
      extAPIsFiles.put(jar, extApi.name)
    }
  }

  def isOptionalUtility(filename: String) = optionalUtilitiesSet(filename)

  /** Valid only for non-endorsed jars. */
  def getRuntimeJarInfo(name: String) = runtimeJarsMap.get(name).orNull

  def getCompactProfileForFile(name: String) = compactProfileFiles.get(name).orNull

  def getExtApiForFile(name: String) = extAPIsFiles.get(name).orNull

  def getExtApiInfoForFile(name: String) = extAPIs(getExtApiForFile(name))
}