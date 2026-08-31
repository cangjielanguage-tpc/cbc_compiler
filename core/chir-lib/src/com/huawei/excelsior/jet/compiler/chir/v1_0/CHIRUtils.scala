/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir.v1_0

import com.google.flatbuffers.{IntVector, LongVector, UnionVector}
import com.huawei.excelsior.jet.compiler.chir.v1_0.PackageFormat.{EnumCtorInfo, MemberVarInfo, VTableInType, VirtualMethodInfo}

object CHIRUtils {

  extension (xs: IntVector) {
    def toSeq: Seq[Long] = xs.iterator.toSeq
    def iterator: Iterator[Long] = if (xs == null) Iterator.empty else Iterator.tabulate(xs.length)(xs.getAsUnsigned)
  }

  extension (xs: LongVector) {
    def toSeq: Seq[Long] = xs.iterator.toSeq
    def iterator: Iterator[Long] = if (xs == null) Iterator.empty else Iterator.tabulate(xs.length)(xs.get)
  }

  extension (xs: MemberVarInfo.Vector) {
    def toSeq: Seq[MemberVarInfo] = xs.iterator.toSeq
    def iterator: Iterator[MemberVarInfo] = if (xs == null) Iterator.empty else Iterator.tabulate(xs.length)(xs.get)
  }

  extension (xs: EnumCtorInfo.Vector) {
    def toSeq: Seq[EnumCtorInfo] = xs.iterator.toSeq
    def iterator: Iterator[EnumCtorInfo] = if (xs == null) Iterator.empty else Iterator.tabulate(xs.length)(xs.get)
  }

  extension (xs: VTableInType.Vector) {
    def toSeq: Seq[VTableInType] = xs.iterator.toSeq
    def iterator: Iterator[VTableInType] = if (xs == null) Iterator.empty else Iterator.tabulate(xs.length)(xs.get)
  }

  extension (xs: VirtualMethodInfo.Vector) {
    def toSeq: Seq[VirtualMethodInfo] = xs.iterator.toSeq
    def iterator: Iterator[VirtualMethodInfo] = if (xs == null) Iterator.empty else Iterator.tabulate(xs.length)(xs.get)
  }
}
