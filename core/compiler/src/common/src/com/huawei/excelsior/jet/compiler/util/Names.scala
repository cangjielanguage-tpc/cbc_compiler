/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.util

import com.huawei.excelsior.jet.compiler.symlevel.{Method, Type}
import xscala.io.Path
import xscala.properties.OS

/**
 * Utilities for methods and classes naming.
 *
 * @author conwor
 */
object Names { //TODO: refactor/rename
  def mangle(str: String): String = {
    str.
      replace('/', '@').
      replace('\\', '@').
      replaceAll("([A-Z])", "~$1").
      replaceAll("<", "@L").
      replaceAll(">", "@R").
      replace('*', '#')
  }

  def className(c: Type): String = {
    assert(c.isClassOrInterface || c.isAJArray || c.isCangjieArray || c.isRecord)
    assert(!c.isDeferred)
    c.getMangledName.replace('/', OS.host.fileSeparator)
  }

  def shortName(method: Method): String = {
    method.getUniqueNumberInClass.toString
  }

  object AJ {
    private val PREFIX = "__aj__"
    private val SEPARATOR = "__"
    private val MANGLE_REPLACE_PREFIX = '_'

    /** Constructs a new AJL field or method name by the original name and signature. */
    def mangleName(name: String, sig: String) = {
      assert(sig.nonEmpty)

      val mangledSig = if (sig.charAt(0) == '(') {
        // method
        val paramsEnd = sig.indexOf(')')
        assert(paramsEnd > 0)
        val params = sig.substring(1, paramsEnd)
        val result = sig.substring(paramsEnd + 1)
        s"${mangleString(params)}$SEPARATOR${mangleString(result)}"
      } else {
        // field
        mangleString(sig)
      }
      s"$PREFIX${mangleString(name)}$SEPARATOR$mangledSig"
    }

    /** Mangles a part of name or signature, replacing characters that are not allowed in AJL identifiers
      * to uniquely mapped sequences of allowed characters.
      */
    private def mangleString(str: String) = {
      assert(!str.contains("."))

      val mangled = StringBuilder()

      str.foreach {
        case '/' =>
          mangled.append(MANGLE_REPLACE_PREFIX)

        case '_' =>
          mangled.append(MANGLE_REPLACE_PREFIX)
          mangled.append('1')

        case ';' =>
          mangled.append(MANGLE_REPLACE_PREFIX)
          mangled.append('2')

        case '[' =>
          mangled.append(MANGLE_REPLACE_PREFIX)
          mangled.append('3')

        case '<' =>
          mangled.append(MANGLE_REPLACE_PREFIX)
          mangled.append('4')

        case '>' =>
          mangled.append(MANGLE_REPLACE_PREFIX)
          mangled.append('5')

        case c =>
          mangled.append(c)
      }
      mangled.toString
    }
  }
}
