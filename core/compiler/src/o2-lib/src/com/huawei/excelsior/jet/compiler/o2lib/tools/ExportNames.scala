/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.o2lib.tools

import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.o2lib.be_386.opAttrsModule as at
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, ObjNamesModule as nms, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.tools.NamesCommon.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xiEnvModule as env}
import com.huawei.excelsior.jet.compiler.symlevel.CallConv.*
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.*
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.{fieldByO2Object, methodByO2Object}
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.{LightweightEnvironment, VersionedMethod}
import com.huawei.excelsior.jet.compiler.symlevel.{SignatureType, TypeKind}
import com.huawei.excelsior.jet.compiler.{TypeProvider, symlevel}

import scala.annotation.tailrec

object ExportNames {
  val MAX_LONGNAME: Int = 31 * 1024 // max. length of exported name

  private val globalNamePrefix = env.config.equation("globalNamePrefix")
  private val genCPrefix = env.config.option("genCPrefix")

  def linkageName(o: pc.Symbol, needClassName: Boolean = true): XString = {
    o match {
      case member: pcO.Member if member.getDeclaringClass eq pcO.x2cClass => member.name
      case field: pcO.Field => memberLinkageName(fieldByO2Object(field), needClassName)
      case method: pcO.Method => memberLinkageName(methodByO2Object(method), needClassName)
      case _ =>
        val name = nms.makeDotObjName(o, at.currClass, needClassName)
        assert(name.length <= MAX_LONGNAME)
        withGlobalPrefix(name)
    }
  }

  def symbolLinkageName(s: Symbol, needClassName: Boolean = true): XString = s match {
    case m: symlevel.Member => memberLinkageName(m, needClassName)
  }

  def memberLinkageName(o: symlevel.Member, needClassName: Boolean = true): XString = {
    var externalOrExportedName = o.getExternalName
    if (externalOrExportedName == null) {
      externalOrExportedName = o.getExportedName
    }

    if (externalOrExportedName != null) {
      o match {
        case m: symlevel.Method if genCPrefix && m.getCallConv == CCALL => js.format("_%S", externalOrExportedName)
        case _ => externalOrExportedName
      }
    } else {
      var name = o match {
        case f: symlevel.Field if f.isStatic && f.getCPPLinkageName != null => f.getCPPLinkageName
        case f: symlevel.Field if f.isStringTable => stringTableLinkageName(f, needClassName)
        case f: symlevel.Field => fieldLinkageName(f, needClassName)
        case m: symlevel.Method => methodLinkageName(m, needClassName)
      }
      if (name.length > MAX_LONGNAME) {
        val id = ExportIds.memberExportID(o)
        assert(id >= 0)
        name = makeVeryLongName(o.getDeclaringClass, id, needClassName)
        assert(name.length <= 2 * MAX_LONGNAME)
      }
      withGlobalPrefix(name)
    }
  }

  def fieldLinkageName(f: symlevel.Field, needClassName: Boolean): XString = {
    val buf = fieldLinkageName(pre = 4, f.getXName, f, needClassName)
    if (f.isOverloaded) {
      buf.appendChar('_')
      mangleSignature(f.getType.toJETSignature, buf)
    }
    buf.toJString
  }

  def stringTableLinkageName(strTableField: symlevel.Field, needClassName: Boolean): XString = {
    fieldLinkageName(pre = 0, fName = XString("strtable"), strTableField, needClassName).toJString
  }

  private def fieldLinkageName(pre: Int, fName: XString, f: symlevel.Field, needClassName: Boolean): js.StringBuffer = {
    val buf = newBuffer(pre)
    if (needClassName) {
      appendClassName(buf, f.getDeclaringClass, DotObjStyle)
      buf.appendChar('_')
    }
    delimitByUnderscore(fName, buf)
    buf
  }

  def methodLinkageName(method: symlevel.Method, needClassName: Boolean): XString = {
    val prefix = if (method.isConstructor || method.isClinit) 0 else if (method.isStatic) 3 else 2
    val name = if (method.isConstructor && !method.getDeclaringClass.isCangjieType) XString("init")
    else if (method.isClinit) XString("clinit")
    else method.getXName

    val buf = newBuffer(prefix)
    if (needClassName) {
      appendClassName(buf, method.getDeclaringClass, DotObjStyle)
      buf.appendChar('_')
    }
    delimitByUnderscore(name, buf)
    if (method.isOverloaded) {
      addMethodSig(buf, method)
    }
    buf.toJString
  }

  def versionedMethodLinkageName(vm: VersionedMethod, needClassName: Boolean = true): XString = {
    val buf = newBuffer(prefix = 8)
    if (needClassName) {
      appendClassName(buf, vm.getHostingClass, DotObjStyle)
      buf.appendChar('_')
    }
    delimitByUnderscore(vm.getXName, buf)
    vm.bodyObj match {
      case member: pcO.Member if member.isOverloaded => addMethodSig(buf, vm.method)
      case _ =>
    }
    buf.toJString
  }

  private def newBuffer(prefix: Int): js.StringBuffer = {
    val buf = new js.StringBuffer()
    buf.assign("_")
    buf.appendInt(prefix)
    buf
  }

  private def withGlobalPrefix(name: XString): XString = if (globalNamePrefix != null) js.format("%S%S", globalNamePrefix, name) else name

  private def makeVeryLongName(cls: symlevel.ClassType, num: Int, needClassName: Boolean): XString = {
    val buf = newBuffer(prefix = 7)
    if (needClassName) {
      appendClassName(buf, cls, DotObjStyle)
    }
    buf.append("%%")
    buf.appendInt(num)
    buf.toJString
  }

  private def appendClassName(buf: js.StringBuffer, cls: symlevel.ClassType, style: STYLE_SET): Unit = {
    NamesCommon.appendClassName(buf, cls.getXName, style, cls.isAnonymous, cls.getClassLoaderID)
  }

  private def addMethodSig(buf: js.StringBuffer, method: symlevel.Method): Unit = {
    mangleSignature(method.getSignature.toJETSignature, buf)
  }

  private def mangleSignature(s: String, buf: js.StringBuffer): Unit = {
    s foreach {
      case '(' => buf.append("__")
      case ')' => buf.append("_")
      case ';' => buf.append("_2")
      case ch  => buf.appendChar(ch)
    }
  }

}
