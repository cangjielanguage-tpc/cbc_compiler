/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.patterns

import com.huawei.excelsior.jet.compiler.symlevel.MethodReferenceAccessKind.*
import com.huawei.excelsior.jet.compiler.{PreparationRequired, StatsKind, TypeProvider}
import com.huawei.excelsior.jet.compiler.opt.ir.{Tag, Universe}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{JavaArray, Primitive}
import com.huawei.excelsior.jet.compiler.symlevel.{Method, MethodReference, MethodType, SignatureType, TypeKind, Type as SymType}

import scala.PartialFunction.condOpt

trait KeyStrings { self: Universe =>

  private def isStringClass(klass: SymType) = klass.getName == "java/lang/String"
  private def isStringMethod(m: Method, name: String) = isStringClass(m.getDeclaringClass) && (m.getName == name)

  private def formatString(ctor: MethodType): String = {
    def fmtChar(tpe: SignatureType): Char = {
      import TypeKind._
      tpe match {
        case Primitive(kind @ (BYTE | INT | CHAR)) => kind.getBCSignatureChar
        case _ => '?'
      }
    }

    val fmtbuf = new StringBuilder
    for (paramt <- ctor.dropFirstNParameters(1).parameterTypes) {
      val baset = paramt match {
        case JavaArray(baseType, _) =>
          fmtbuf.append('a')
          baseType
        case _ => paramt
      }
      fmtbuf.append(fmtChar(baset))
    }
    fmtbuf.toString()
  }

  private def isFormatSupported(format: String): Boolean = format match {
    case "" | "aC" | "aCII" | "aBI" | "aBIII" => true
    case _ => false
  }

  private def tryReplaceOneNewString(root: Call): Boolean = {

    /** Nullcheck(recv) just before non-static Invoke(recv) */
    object ReceiverCheck {
      def unapply(nc: NullCheck): Option[Call] = condOpt(nc.outCtrl) {
        case call: Call if call.targetRef.hasReceiverParameter && call.receiver == nc.obj => call
      }
    }

    object SNew {
      def unapply(obj: New): Boolean = isStringClass(obj.allocType.symType)
    }

    object SInit {
      def unapply(call: Call): Option[(New, String)] = condOpt(call) {
        case CallMethod(target, SPECIAL, Seq(obj @ SNew(), _*)) if isStringMethod(target, "<init>") =>
          (obj, formatString(target.getMethodType))
      }
    }

    case class KeyNewPattern(newNode: New, ctorNode: Call, format: String)

    def findKeyNewPattern(root: Call): Option[KeyNewPattern] = condOpt(root) {
      case ctor @ SInit(snew, format) if isFormatSupported(format) =>
        KeyNewPattern(snew, ctor, format)
    }

    def replaceKeyNewPattern(pattern: KeyNewPattern): Unit = {
      val (snew, ctor) = (pattern.newNode, pattern.ctorNode)
      val usesToRemove = snew.valueUses.collect { case sp: SpinalNode if sp dominates ctor => sp }.toList
      assert(usesToRemove forall {
        case `ctor` | ReceiverCheck(`ctor`) => true
        case _ => false
      })

      val ksmethodname = {
        val (prefix, suffix) = ("KeyStrings_newString", pattern.format)
        if (suffix == "") prefix else s"${prefix}_$suffix"
      }
      val ksmethod = env.getSpecStrConcatMethod(ksmethodname)
      assert(ksmethod != null, ksmethodname)
      val ksref = new MethodReference(ksmethod, STATIC)

      val ksnew = insertCodeBefore(ctor) {
        ensurePrepared(PreparationRequired.forInvoke(ksref))
        Invoke(ksref)(ctor.invokeArgs.tail :_*)
      }

      usesToRemove foreach strikeOut
      strikeOutWithValueUses(snew, ksnew)
      stats.count(StatsKind.StringOpt, s"newString(${pattern.format}) replaced", ksnew)
    }

    var changed = false
    for (pattern <- findKeyNewPattern(root)) {
      replaceKeyNewPattern(pattern)
      changed = true
    }
    changed
  }

  /** Find and replace `new String(...)` patterns to `KeyStrings.newString(...)`. */
  def replaceNewKeyStrings(): Boolean = {
    var changed = false
    for (x @ CallMethod(method, _, _) <- all[Call] if isStringMethod(method, "<init>")) { // fast path check is outlined
      changed |= tryReplaceOneNewString(x)
    }
    changed
  }

  private def tryReplaceOneNewKeyString(kscall: Call): Boolean = {
    val CallMethod(RT.KeyStrings.newKeyString0, STATIC, Seq(length)) = kscall
    replaceByCode(kscall) { NewString(length) }
    stats.count(StatsKind.StringOpt, "newKeyString0 replaced", kscall)
    true
  }

    /** Replace `KeyStrings.newKeyString0` call by `NewString` node. */
  def replaceKeyStringAlloc(): Boolean = {
    var changed = false
    for (x @ CallMethod(RT.KeyStrings.newKeyString0, _, _) <- all[Call]) {
      changed |= tryReplaceOneNewKeyString(x)
    }
    changed
  }
}
