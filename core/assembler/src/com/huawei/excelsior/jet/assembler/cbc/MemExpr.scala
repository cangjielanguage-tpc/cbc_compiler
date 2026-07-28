/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.assembler.cbc.MemExpr.*
import com.huawei.excelsior.jet.assembler.cbc.MemExpr.{Body, Head}
import com.huawei.excelsior.jet.assembler.cbc.Register.IR

import scala.annotation.nowarn

/** Memory Expression is a special addressing expression that consists of receiver object,
  * chain of sequentially accessed fields and optional extension.
  * They are used to eliminate pointers into the middle of objects and minimize address arithmetic expressions.
  *
  * E.g. `GetField(receiver, field)` could be assembled as `mov dst, MemExpr(receiver, [fieldId])`.
  */
object MemExpr {
  type Head = IR | StackSlot.Typed | StackSlot.Untyped | Head.RegImm | Head.StaticField.type | Head.RegPair | Head.RecordArray

  object Head {
    object StaticField
    case class RegImm(reg: IR, offset: Int)
    case class RegPair(obj: IR, offset: IR)
    case class RecordArray(obj: IR, idx: IR, arrayOrElemSigId: Symbol)
  }

  type Body = Array[Symbol] | CbcTypeKind

  extension (body: Body) {
    def length: Int = body match {
      case arr: Array[Symbol] => arr.length
      case _: CbcTypeKind => 0
    }
    // TODO rename, as memexpr may contain not only the field symbol, but ftc or ohm slot indices
    def hasFieldChain: Boolean = body match {
      case _: Array[Symbol] => true
      case _ => false
    }
  }
}

case class MemExpr(head: Head, body: Body, isGeneric: Boolean = false) { // TODO: support MemExpr.Tail
  private val hasHeadDesc: Boolean = head match {
    case _: IR => false
    case _ => true
  }
  val outlined: Boolean = head match {
    case _: IR if body.length > 3 => true
    case _: StackSlot.Typed if body.length > 2 => true
    case _: StackSlot.Untyped if body.length > 2 => true
    case _: Head.RegImm if body.length > 2 => true
    case Head.StaticField if body.length > 3 => true
    case _: Head.RegPair if body.length > 3 => true
    case _ => true // TODO: support inline mode
  }

  val mMode: Int = (if body.hasFieldChain then 1 else 0) << 3 | (if isGeneric then 1 else 0) << 2 | (if hasHeadDesc then 1 else 0) << 1 | (if outlined then 1 else 0)

  @nowarn("msg=match may not be exhaustive")
  private lazy val headType: Int = head match {
    case _: StackSlot.Typed => 1
    case _: StackSlot.Untyped => assert(!isGeneric); 2
    case _: Head.RegImm => assert(!isGeneric); 3
    case Head.StaticField => 4
    case _: Head.RegPair => 5
  }
  // TODO: refactor assembler code and eliminate type punning
  def headEncoding: IR = head match {
    case r: IR => r
    case _ => IR.fromOrdinal(headType)
  }
}
