/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc.isa12.forked

import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.{BytecodeReference, FieldReference, FieldReferenceWithType, MethodReference, RawData, Signature, StringLiteral}

trait SymbolAdapter {
  def adapt(symbol: Symbol): BytecodeReference

  final def field(symbol: Symbol): FieldReferenceWithType = adapt(symbol).asInstanceOf[FieldReferenceWithType]
  final def method(symbol: Symbol): MethodReference = adapt(symbol).asInstanceOf[MethodReference]
  final def sigType(symbol: Symbol): Signature      = adapt(symbol).asInstanceOf[Signature]
  final def string(symbol: Symbol): StringLiteral   = adapt(symbol).asInstanceOf[StringLiteral]
  final def raw(symbol: Symbol): RawData            = adapt(symbol).asInstanceOf[RawData]
}
