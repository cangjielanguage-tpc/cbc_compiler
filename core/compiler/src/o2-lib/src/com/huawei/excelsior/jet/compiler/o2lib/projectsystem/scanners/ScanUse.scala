/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.projectsystem.scanners

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.RTConst
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.AbstractProject
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.ErrorMessages.msg_syntax_error
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.scanners.ScanUse.usgExtractClassName
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{StringTokenizerModule as strtok, xiEnvModule as env}
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.ConvertableInt
import xscala.util.Set32

object ScanUse {

  /*
    str - string from the usg of kind "[classloaderid%]classname[.method|field]"
    classloaderid can contain dots ('.')
     returns "[classloaderid%]classname"
  */
  def usgExtractClassName(str: XString): XString = {
    var from = str.indexOf('%')
    if (from < 0) {
      from = 0
    }
    val k = str.indexOf('.', from)
    if (k >= 0) {
      return str.substring(0, k)
    }
    str
  }
}

class ScanUse extends Scan {

  var project: AbstractProject[_] = _

  private def isFlags(f: XString): Boolean = {
    if (f.length != 32) {
      return false
    }

    for (i <- 0 until f.length) {
      if (!(f.charAt(i) == '0' || f.charAt(i) == '1')) {
        return false
      }
    }
    true
  }

  private def parseFlags(f: XString): Set32 = {
    assert(f.length == 32)

    var flags = Set32.empty
    for (i <- 0 to 31) {
      f.charAt(i) match {
        case '0' =>
        case '1' =>
          flags += (31 - i).toUByte
        case _ =>
          throw new AssertionError
      }
    }
    flags
  }

  override def preprocessor(): Boolean = {
    // p.srcbf contains string to parse
    var buf = this.srcbf.toJString
    buf = buf.trim()
    if (buf.length == 0 || buf.charAt(0) == '%') { // blank or comment line
      return false
    }

    val st = strtok.newStringTokenizer(buf, " \u0009\r\n")
    if (!st.hasMoreTokens) {
      return false
    }

    var s = st.nextToken()

    var locale = false
    var nativelib = false

    if (s.equals2("{")) {            // factor start
      wrongSyntax(this.in, this.lineno, -1, msg_syntax_error)
      return true
    } else if (s.equals2("}")) {            // factor end
      wrongSyntax(this.in, this.lineno, -1, msg_syntax_error)
      return true
    } else if (s.equals2(".default_classloader") || s.equals2(".not_found_type") || s.equals2(".not_found_method") || s.equals2(".not_found_field")) {
      // skip
      return false
    } else if (s.equals2(".locale_detected")) {
      locale = true
    } else if (s.equals2(".nativelibrary")) {
      nativelib = true
    } else if (s.charAt(0) == '.') {
      wrongSyntax(this.in, this.lineno, -1, msg_syntax_error)
      return true
      // ELSE s is class/field/method
    }

    if (locale || nativelib) {
      if (!st.hasMoreTokens) {
        wrongSyntax(this.in, this.lineno, -1, msg_syntax_error)
        return true
      }
    }

    if (locale) {
      s = st.nextToken()
      if (env.config.option("IncludeDetectedLocales")) {
        env.errors.silentMessage(ErrMsg520)
      }
    } else if (nativelib) { // skip
      s = st.nextToken()
    } else {  // s is class/field/method
      if (st.hasMoreTokens) {
        val f = st.nextToken()
        if (!isFlags(f)) {
          wrongSyntax(this.in, this.lineno, -1, msg_syntax_error)
          return true
        }
        parseFlags(f)
      } else {
        RTConst.UsageMask.USG_DEFAULT.intValue.toSet32
      }

      this.project.findClassAndAppend(usgExtractClassName(s), doError = false)
      // jira JET-510:
      // can not reproduce the user's
      // problem of incorrect writing
      // class references to .usg
      // so ignore not-found entries
    }

    if (st.hasMoreTokens) {
      wrongSyntax(this.in, this.lineno, -1, msg_syntax_error)
      return true
    }

    false
  }

}
