/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common.util

import xscala.io.{Path, TextInput}
import xscala.text.{Latin1Encoding, USAsciiEncoding}

import scala.collection.mutable
import scala.util.Using

type Properties = Map[String, String]

object Properties {
  def load(path: Path): Properties = {
    Using.resource(TextInput.from(path, buffered = true, encoding = Latin1Encoding)) { in =>
      load(in)
    }
  }

  def load(in: TextInput): Properties = {
    val props = mutable.HashMap.empty[String, String]
    var line: String = null
    while ({ line = in.getLine(); line != null }) {
      line = line.trim()
      // TODO: this should decode Unicode escapes as defined in JLS
      if (line.nonEmpty && line.head != '#' && line.head != '!') { // skip empty lines
        // skip comments 
        while (line.last == '\\') {
          line = line.init
          // concat next line 
          val nextLine = in.getLine()
          if (nextLine != null) {
            line += nextLine.trim()
          }
        }
        val eqPos = line.indexOf('=')
        if (eqPos > 0) {
          val key = line.substring(0, eqPos).trim()
          val value = line.substring(eqPos + 1).trim()
          props += (key -> value)
        } else {
          props += (line -> "")
        }
      }
    }
    props.toMap
  }
}
