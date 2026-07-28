/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cangjie

import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.symlevel.{Method, MethodSignature, SignatureType}

case class CHIRVTable(extDefs: Seq[CHIRVTable.ExtDef])

object CHIRVTable {
  case class ExtDef(extType: SignatureType, funcTable: Seq[Entry])
  case class Entry(name: String, sig: MethodSignature, genericParams: Seq[SignatureType], impl: Option[Method], modifiers: Modifiers,
                   originalSig: MethodSignature, instantiatedRefType: SignatureType, instantiatedReturnType: SignatureType)
}
