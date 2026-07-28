/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.xpackii.jreinfo

import xscala.io.{Path, TextInput}
import xscala.properties.OS
import xscala.text.Utf8Encoding
import xscala.util.StringTokenizer

import scala.collection.mutable
import scala.util.Using

/** There is a special file `profile/jre/lib/locales` that enumerates all available locales.
  * Since user can exclude some locales, we should filter the contents of this file.
  */
object LocalesFileProcessor {
  /** Checks whether the given file seems to be the `locales` file, based on its name.
    *
    * @param file the file to check
    * @return `true` iff the file seems to be the `locales` file
    */
  def isLocalesFile(file: Path) = RTSet.LOCALES_FILE_NAME == file.name

  private def makeLocaleList(locales: Set[String]) = locales.toArray.sorted.mkString("", " ", " |  ")

  private def readStream(path: Path) = {
    Using.resource(TextInput.from(path, buffered = true, encoding = Utf8Encoding)) { in =>
      Array.from(in.getLines())
    }
  }
}

class LocalesFileProcessor(private var rtSet: RTSet) {
  /** Processes the contents of the `locales` file to filter out locales excluded by user, and returns the new filtered contents. */
  def processLocalesFile(path: Path) = {
    val localesList = LocalesFileProcessor.readStream(path)
    val out = new StringBuilder()
    val lineSep = OS.host.lineSeparator

    val availableLocales = new mutable.HashSet[String]

    // From <jdk-8-source>/jdk/make/gensrc/GensrcLocaleDataMetaInfo.gmk
    // ja-JP-JP and th-TH-TH need to be manually added, as they don't have any resource files.
    availableLocales.add("ja-JP-JP")
    availableLocales.add("th-TH-TH")

    var lineNum = 0

    def appendResource(basePackage: String, resourceName: String): Unit = {
      val filteredLocales = filterLocales(localesList(lineNum))
      filteredLocales.subtractAll(rtSet.getExcludedResourceLocales(basePackage.replace('.', '/'), resourceName))

      availableLocales.addAll(filteredLocales)

      out.append(LocalesFileProcessor.makeLocaleList(filteredLocales.toSet) + lineSep)
      lineNum += 1
    }

    appendResource("sun.text.resources", "FormatData")
    appendResource("sun.text.resources", "CollationData")
    appendResource("sun.text.resources", "BreakIteratorInfo")
    appendResource("sun.text.resources", "BreakIteratorRules")
    appendResource("sun.util.resources", "TimeZoneNames")
    appendResource("sun.util.resources", "LocaleNames")
    appendResource("sun.util.resources", "CurrencyNames")
    appendResource("sun.util.resources", "CalendarData")
    out.append(LocalesFileProcessor.makeLocaleList(availableLocales.toSet) + lineSep)
    out.result()
  }

  private def filterLocales(localesList: String) = {
    val filteredLocales = new mutable.HashSet[String]
    val st = StringTokenizer(localesList, Array(' '))

    while (st.hasMoreTokens) {
      val l = st.nextToken()
      if (l != "|") {
        filteredLocales.add(l)
      }
    }

    filteredLocales
  }
}