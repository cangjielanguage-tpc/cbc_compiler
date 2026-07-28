/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.o2lib.tools

import com.huawei.excelsior.common.Language.JAVA
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.languagePack
import com.huawei.excelsior.jet.compiler.RTConst
import com.huawei.excelsior.jet.compiler.o2lib.u.JStringsModule as js
import xscala.util.{Set32, UByte}

object NamesCommon {
  /* ---------------- Class names & signatures ------------------------ */
  private type STYLE_CONST = UByte
  private val st_class_name: STYLE_CONST = UByte(0) // simple "readable" class name
  private val st_class_uid: STYLE_CONST = UByte(1) // class id that unique in component
  private val st_delimit_slash: STYLE_CONST = UByte(5) // package/cls_a
  private val st_delimit_dot: STYLE_CONST = UByte(6) // package.cls_a
  private val st_delimit_uscope: STYLE_CONST = UByte(7) // package_cls_1a
  private val st_delimit_backquote: STYLE_CONST = UByte(8)

  val STYLE_SET = Set32
  type STYLE_SET = Set32
  // GetClassName() styles 
  val CL_uid: STYLE_SET = STYLE_SET.of(st_class_uid, st_delimit_dot)
  val CL_slash: STYLE_SET = STYLE_SET.of(st_class_name, st_delimit_slash)
  /* + CL_dots */
  // style for .obj-names 
  val DotObjStyle: STYLE_SET = STYLE_SET.of(st_class_uid, st_delimit_uscope)

  def simpleClassStyle(style: STYLE_SET): Boolean = (style contains st_class_name) && (style contains st_delimit_slash)

  def delimitByUnderscore(s: XString, /*VAR*/ buf: js.StringBuffer): Unit = {
    s foreach {
      case '_' =>
        buf.append("_1") //   '_' -> '_1'
      case '-' =>
        buf.append("_2") //   '-' -> '_2'
      case '/' =>
        buf.appendChar('_') //   '/' -> '_'
      case '.' =>
        buf.appendChar('_') //   '.' -> '_'
      case ch =>
        buf.appendChar(ch)
    }
  }

  def appendClassName(/*VAR*/ buf: js.StringBuffer, name: XString, style: STYLE_SET, isAnonymous: Boolean, classLoaderId: Int): Unit = {
    val pos = buf.length
    assert((style contains st_class_name) || (style contains st_class_uid))

    if (style contains st_delimit_uscope) {
      delimitByUnderscore(name, buf)
    } else {
      buf.appendString(name)
      if (style contains st_delimit_slash) {
        // do nothing 
      } else if (style contains st_delimit_dot) {
        buf.replaceInRegion(pos, buf.length - pos, '/', '.')
      } else if (style contains st_delimit_backquote) {
        buf.replaceInRegion(pos, buf.length - pos, '/', '`')
      } else {
        throw new AssertionError
      }
    }

    if (style contains st_class_uid) {
      if (isAnonymous) {
        // JVM can load two classes with the same name into one classloader  
        // provided that one of the class is anonymous. 
        // So we need to mangle anonymous classes somehow to not clash with  
        // usual classes.  
        buf.appendChar('|')
      }

      if (languagePack.supports(JAVA)) {
        // JetPackII extracts list of classes exported from executable and 
        // expects that class names are mangled with classloader ID if it is not standard. 
        if (classLoaderId > RTConst.ClassLoaderIDProvider.LAST_STD_CLID.intValue) {
          buf.appendChar('%')
          buf.appendInt(classLoaderId)
        }
      }
    }
  }

}
