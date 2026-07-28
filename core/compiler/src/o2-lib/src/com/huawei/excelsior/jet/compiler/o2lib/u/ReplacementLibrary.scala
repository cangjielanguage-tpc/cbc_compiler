/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.common.LanguagePack.CANGJIE
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.PDB2.EntryKind
import com.huawei.excelsior.jet.compiler.delayed.DelayedIntrinsicsUsageTracker.{env, locationInPdb}
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule.{Class, Method, findClass}
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xPDBModule.PDBKind.Other
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.{xArchivePDBModule as xArchivePDB, xPDBModule as xPDB}
import com.huawei.excelsior.jet.compiler.symlevel.{JETSignatureParser, MethodSignature}
import com.huawei.excelsior.jet.compiler.{Env, Environment}
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.{break, loop}
import xscala.matching.Regex
import xscala.util.StringOps.r

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Using

object ReplacementLibrary {
  private val replacementLibrary: mutable.LinkedHashMap[String /* method to replace */ ,
                                                        String /* method to replace with */ ] = mutable.LinkedHashMap.empty
  private var modified = false

  var env: Environment = _

  private def locationInPdb = EntryKind.Repl.loc(s"repl")

  def deserialize(): Unit = {
    val input = env.pdb.getDataInputOrNull(locationInPdb)
    if (input != null) {
      Using.resource(input) { in =>
        val count = in.getW32()
        for (_ <- 0 until count) {
          val line = in.getUTF()

          val methods = line.split(':')
          replacementLibrary(methods(0)) = methods(1)
        }
      }
      modified = false
    }
  }

  def serialize(): Unit = {
    if (modified) {
      Using.resource(env.pdb.getDataOutput(locationInPdb)) { out =>
        out.putW32(replacementLibrary.size)
        for ((oldMethod, newMethod) <- replacementLibrary.toSeq) {
          out.putUTF(s"$oldMethod:$newMethod")
        }
      }
    }
  }

  private val methodRegex = """^(.*)\.(.*)(\(.*)$""".r

   def getReplacement(method: Method): Option[Method] = replacementLibrary.get(method2String(method)) collect {
    case methodRegex(clazzStr, methodNameStr, sigStr) =>
      val clazz = method.getDeclaringClass.resolveClass(XString(clazzStr), addImport = true)
      clazz.findLocalMethod(XString(methodNameStr), O2Env.env.parseMethodSignature(XString(sigStr)))
  }

  private def method2String(method: Method): String =
    if (method.getDeclaringClass.isCangjieType) {
      method.getReadableName(need_class_name = false, need_full_sign = false).toString
    } else {
      method.getReadableName(need_class_name = true, need_full_sign = true).toString
    }

  def setStringReplacement(className: XString, method: XString, sig: XString, newMethod: String): Unit = {
    val oldMethodClassNameStr = if (className.isEmpty) XString.empty else s"${className.replace('.', '/')}."
    val oldMethodStr = s"""$oldMethodClassNameStr$method$sig"""

    setReplacement(oldMethodStr, newMethod)
  }

  def setStringReplacement(className: XString, method: XString, sig: XString, newMethod: Method): Unit = {
    val oldMethodClassNameStr = if (className.isEmpty) XString.empty else s"${className.replace('.', '/')}."
    val oldMethodStr = s"""$oldMethodClassNameStr$method$sig"""

    setReplacement(oldMethodStr, method2String(newMethod))
  }

  private def setReplacement(oldMethod: String, newMethod: String): Unit = {
    if (!replacementLibrary.contains(oldMethod) || replacementLibrary(oldMethod) != newMethod) {
      modified = true
      replacementLibrary(oldMethod) = newMethod
    }
  }
}
