/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir.v100

import com.google.flatbuffers.{IntVector, LongVector}
import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.v100.PackageFormat.{EnumCtorInfo, MemberVarInfo, VTableInType, VirtualMethodInfo}

import scala.reflect.ClassTag

object CHIRUtils {

  extension (xs: IntVector) {
    def toSeq: Seq[Long] = xs.iterator.toSeq
    def toValueSeq[T >: Null <: CHIR.Value : ClassTag](using provider: CHIRItemProvider): Seq[T] = toSeq.map(provider.getValue[T](_).get)
    def toTypeSeq[T >: Null <: CHIR.Type : ClassTag](using provider: CHIRItemProvider): Seq[T] = toSeq.map(provider.getType[T](_).get)
    def toExprSeq[T >: Null <: CHIR.Expression : ClassTag](using provider: CHIRItemProvider): Seq[T] = toSeq.map(provider.getExpr[T](_))
    def iterator: Iterator[Long] = if (xs == null) Iterator.empty else Iterator.tabulate(xs.length)(xs.getAsUnsigned)
  }

  extension (xs: LongVector) {
    def toSeq: Seq[Long] = xs.iterator.toSeq
    def iterator: Iterator[Long] = if (xs == null) Iterator.empty else Iterator.tabulate(xs.length)(xs.get)
  }

  extension (xs: MemberVarInfo.Vector) {
    def toSeq(using provider: CHIRItemProvider): Seq[CHIR.InstanceVar] = xs.iterator.map(InstanceVarImpl(_)).toSeq
    def iterator: Iterator[MemberVarInfo] = if (xs == null) Iterator.empty else Iterator.tabulate(xs.length)(xs.get)
  }

  extension (xs: EnumCtorInfo.Vector) {
    def toSeq(using provider: CHIRItemProvider): Seq[CHIR.EnumCtor] = xs.iterator.map(EnumCtorImpl(_)).toSeq
    def iterator: Iterator[EnumCtorInfo] = if (xs == null) Iterator.empty else Iterator.tabulate(xs.length)(xs.get)
  }

  extension (xs: VTableInType.Vector) {
    def toSeq(using provider: CHIRItemProvider): Seq[CHIR.VTable] = xs.iterator.map(VTableImpl(_)).toSeq
    def iterator: Iterator[VTableInType] = if (xs == null) Iterator.empty else Iterator.tabulate(xs.length)(xs.get)
  }

  extension (xs: VirtualMethodInfo.Vector) {
    def toSeq(using provider: CHIRItemProvider): Seq[CHIR.VMethod] = xs.iterator.map(VMethodImpl(_)).toSeq
    def iterator: Iterator[VirtualMethodInfo] = if (xs == null) Iterator.empty else Iterator.tabulate(xs.length)(xs.get)
  }
}
