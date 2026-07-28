/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.fe

import com.huawei.excelsior.common.CodeHelpers
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.common.Language.JAVA
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.languagePack
import com.huawei.excelsior.jet.compiler.RTConst
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule.ClassloaderIDGetter
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.tools.NamesCommon
import com.huawei.excelsior.jet.compiler.o2lib.tools.NamesCommon.*
import com.huawei.excelsior.jet.compiler.o2lib.u.JStringsModule as js

import scala.annotation.{nowarn, tailrec}

object ObjNamesModule { /* paul, 20 may 2003 */

  private def appendClassName(buf: js.StringBuffer, cls: pcO.Class, style: STYLE_SET): Unit = {
    NamesCommon.appendClassName(buf, cls.name, style, cls.isAnonymous, cls.getClassloaderID)
  }

  private def appendJBCPrimitiveName(buf: js.StringBuffer, t: pc.SymType.JBC.Primitive, style: STYLE_SET): Unit = {
    val primCLID = if (languagePack.supports(JAVA)) ClassloaderIDGetter.APP_CLID else ClassloaderIDGetter.SYSTEM_CLID
    NamesCommon.appendClassName(buf, XString(t.typeHandleName), style, false, primCLID)
  }

  def getClassName(cls: pcO.Class, style: STYLE_SET): XString = {
    val buf = new js.StringBuffer()

    // fast path
    if (simpleClassStyle(style)) {
      return cls.name
    }

    appendClassName(buf, cls, style)
    buf.toJString
  }

  // package name ends with '/'
  // returns NIL for 'default' (unnamed) package
  def getPackageName(cls: pcO.Class): XString = {
    val s = cls.name
    val pos = s.lastIndexOf('/')
    if (pos == -1) {
      null
    } else {
      s.substring(0, pos + 1)
    }
  }

  /* --------------------- names for .obj-files ----------------------- */
  @nowarn("msg=match may not be exhaustive")
  private def mk1(buf: js.StringBuffer, pre: Int, tpe: Option[pc.SymType]): Unit = tpe match {
    case None =>
      buf.append("_")
      buf.appendInt(pre)

    case Some(cls: pcO.Class) =>
      buf.append("_")
      buf.appendInt(pre)
      appendClassName(buf, cls, DotObjStyle)

    case Some(t: pc.SymType.JBC.Primitive) =>
      appendJBCPrimitiveName(buf, t, DotObjStyle)
  }

  private def mk2(buf: js.StringBuffer, pre: Int, tpe: Option[pc.SymType], nm2: XString): Unit = {
    mk1(buf, pre, tpe)
    if (tpe.nonEmpty) {
      buf.appendChar('_')
    }
    delimitByUnderscore(nm2, buf)
  }

  private def mk0(buf: js.StringBuffer, pre: Int, tpe: Option[pc.SymType], nm2: String): Unit = {
    mk1(buf, pre, tpe)
    if (tpe.nonEmpty) {
      buf.appendChar('_')
    }
    //  delimitByUnderscore(nm2, buf);
    buf.append(nm2)
  }

  private def mkJava(o: pc.Symbol, currClass: pcO.Class, class0: Boolean): XString = {
    val buf = new js.StringBuffer()
    var idescID: Int = 0

    o match {
      case _: pcO.Method | _: pcO.ModuleObject => shouldNotReachHere()

      case o: (pc.DataSymbol.RunTimeTypeInfo | pc.DataSymbol.TypeHandle | pc.DataSymbol.ThinTypeHandle) =>
        // type handle base / runtime type info / thin type desc
        assert(class0)
        o.tpe match {
          case t: pc.SymType.Array =>
            val dim = t.dim
            assert(dim <= 255)
            for (_ <- 1 to dim) {
              buf.appendChar('[')
            }
          case _ =>
        }

        pcO.getCoreType(o.tpe) match {
          case c: pcO.Class =>
            def mkFunction(pre: Int, postfix: String): Unit = o match {
              case _: (pc.DataSymbol.RunTimeTypeInfo | pc.DataSymbol.ThinTypeHandle) => mk0(buf, pre, Some(c), postfix)
              case _ => mk1(buf, pre, Some(c))
            }
            o match {
              case _: pc.DataSymbol.ThinTypeHandle =>
                mkFunction(1, "THIN")  // Note: typestable.cpp in linker relies on this format
              case _ =>
                if (c.isShielded) {
                  mkFunction(5, "RTTI")
                  buf.append("__A__")
                  appendClassName(buf, currClass, DotObjStyle)
                } else {
                  mkFunction(1, "RTTI")
                }
            }
          case t: pc.SymType.JBC.Primitive =>
            mk1(buf, 1, Some(t))
        }

      case o: pc.DataSymbol.InstanceDescriptor =>
        // instance descriptor
        o.tpe match {
          case t: pc.SymType.Array =>
            val dim = t.dim
            assert(dim <= 255)
            for (_ <- 1 to dim) {
              buf.appendChar('[')
            }
            idescID = 1
          case _ =>
            idescID = 0
        }

        mk0(buf, idescID, Some(pcO.getCoreType(o.tpe)), "IDESC")

      case o: pc.DataSymbol.RW =>
        val c = pcO.getClassRecord(o.mno)
        mk2(buf, 4, Some(c), o.name) // static variable
    }
    buf.toJString
  }

  // ExportNames should be used for fields and methods
  def makeDotObjName(o: pc.Symbol, currClass: pcO.Class, needClassName: Boolean = true): XString = o match {
    case _: pc.DataSymbol.Const => o.name
    case _: pc.DataSymbol.RW => mkJava(o, currClass, needClassName)
  }

  def makePackageName(cls: pcO.Class, jar: XString, system: Boolean, symbolName: Boolean): XString = {
    val buf = new js.StringBuffer()

    var pkg = getPackageName(cls)
    assert(pkg != null)

    if (symbolName) {
      assert(pkg.charAt(pkg.length - 1) == '/')
      pkg = pkg.substring(0, pkg.length - 1) // cut last '/'
      mk2(buf, 6, None, pkg)
      if (system) {
        buf.append("__S")
      } else {
        buf.append("__U")
      }
      if (jar != null) {
        buf.append("__")
        delimitByUnderscore(jar, buf)
      }
    } else {
      if (system) {
        buf.assign("SYSPKG")
      } else {
        buf.assign("USRPKG")
      }
      if (jar != null) {
        buf.appendString(jar)
        buf.appendChar('!')
      }
      buf.appendString(pkg)
    }
    buf.toJString
  }
}
