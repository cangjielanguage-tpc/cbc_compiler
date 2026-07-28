/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.fe_jbc

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.o2j.runtime.*
import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.util.UByte

// Module for checking Java identifiers.
object JavaIdentifierModule {

  type NameKinds = UByte
  val nk_class: NameKinds = UByte(0)
  val nk_method: NameKinds = UByte(1)
  val nk_field: NameKinds = UByte(2)


  def utf8PartIsIdentifier(name: XString, from: Int, to0: Int, slashAllowed: Boolean): Boolean = {
    if (from >= to0) {
      assert(from == to0) // for malformed names in bytecode 
      return false
    }

    val it = name.substring(from, to0).unicodeIterator
    var start = true
    while (it.hasNext) {
      val ch = it.next()
      if (ch <= 0x7F) {
        if (!(ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z' || !start && ch >= '0' && ch <= '9' || ch == '$' || ch == '_' || ch == '+')) {
          if (!start && slashAllowed && ch == '/') {
            if (it.hasNext) {
              start = true
            } else {
              return false
            }
          } else {
            return false
          }
        } else {
          start = false
        }
      } else if (start) {
        if (java.lang.Character.isJavaIdentifierStart(ch)) {
          start = false
        } else {
          return false
        }
      } else if (!java.lang.Character.isJavaIdentifierPart(ch)) {
        return false
      }
    }
    true
  }

  def check15Name(name: XString, namekind: NameKinds): Boolean = {
    val it = name.unicodeIterator
    while (it.hasNext) {
      val ch = it.next()
      if (ch <= 0x7F) {
        if (ch == '.' || ch == ';' || ch == '[') {
          return false
        } else if (ch == '/' && namekind != nk_class) {
          return false
        } else if ((ch == '<' || ch == '>') && namekind == nk_method) {
          return false
        }
      }
    }
    true
  }

}
