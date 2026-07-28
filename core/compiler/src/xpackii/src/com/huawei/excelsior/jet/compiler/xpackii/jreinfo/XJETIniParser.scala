/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.xpackii.jreinfo

import xscala.io.{Path, TextInput, stdout}
import xscala.text.PlatformEncoding
import xscala.util.StringOps.asciiIsWhitespace

import java.io.IOException
import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.util.Using

object XJETIniParser {
  private val VERSION = "14"
}

/** Parser of a primary JET `.ini` file, currently located at `bin\versions\version.ini` (former `xjet.ini`). */
class XJETIniParser private[jreinfo](path: Path) {
  val sections: mutable.Map[String, mutable.Map[String, Buffer[String]]] = mutable.Map.empty

  private var pos = 0
  private var line: String = _

  parseXJETIni(path)

  private def skip(s: String): Unit = {
    while ((pos < s.length) && s.charAt(pos).asciiIsWhitespace) {
      pos += 1
    }
  }

  private def readNext(in: TextInput): Boolean = {
    try {
      line = in.getLine()
      while (line != null) {
        pos = 0
        skip(line)
        if (pos < line.length) {
          if (line.charAt(pos) != '#') {
            return true
          }
        }
        line = in.getLine()
      }
    } catch {
      case e: IOException => stdout.printStackTrace(e)
    }
    false
  }

  private def parsePair(keys: mutable.Map[String, Buffer[String]]): Unit = {
    val eq = line.indexOf('=')
    if (eq == -1) {
      keys.put(line.trim, mutable.Buffer.empty)
    } else {
      val key = line.substring(pos, eq).trim
      keys.getOrElseUpdate(key, mutable.Buffer.empty).append(line.substring(eq + 1).trim)
    }
  }

  private def isSection = line.charAt(pos) == '['

  private def parseSection(in: TextInput, section: String): Unit = {
    val keys = mutable.Map.empty[String, Buffer[String]]
    while (readNext(in) && !isSection) {
      parsePair(keys)
    }
    sections.put(section, keys)
  }

  private def parseXJETIni(path: Path): Unit = {
    Using.resource(TextInput.from(path, buffered = true, encoding = PlatformEncoding.native)) { in =>
      if (!readNext(in)) {
        return
      }
      if (!isSection) {
        throw new IOException("syntax error: [section] expected")
      }

      while (line != null) {
        val end = line.indexOf(']')
        if (end == -1) {
          throw new IOException("syntax error")
        }
        parseSection(in, line.substring(pos + 1, end))
      }
    }

    val initSect = sections.get("init").orNull
    assert(initSect != null, "Unsupported version.ini format")

    val l = initSect.get("version").orNull
    assert(l != null && l.size == 1 && XJETIniParser.VERSION == l.head.trim,
      s"Unsupported version.ini version: ${XJETIniParser.VERSION} expected")
  }
}