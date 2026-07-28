/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.stubs

import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.codeemitter.CodeEmitter
import com.huawei.excelsior.jet.compiler.abi.DAIGenerator
import com.huawei.excelsior.jet.compiler.symlevel.{Field, MethodType, Type}
import com.huawei.excelsior.jet.compiler.{Environment, SymbolLinker, ThunkGeneratorBase}

abstract class ThunkGeneratorFactory(protected val env: Environment, protected val symbolLinker: SymbolLinker)
extends ThunkGeneratorBase {

  protected def createGenerator(methodType: MethodType): ThunkGenerator =
    createGenerator(methodType, null)

  protected def createGenerator(methodType: MethodType, frameDescriptor: Symbol): ThunkGenerator

  override final def genNonVirtualForwarder(target: Symbol, methodType: MethodType, receiverNullCheck: Boolean) =
    createGenerator(methodType).genNonVirtualForwarder(target, receiverNullCheck)

  override final def genVirtualForwarder(refClass: Type, vnum: Int, methodType: MethodType, isInvokeInterface: Boolean, receiverNullCheck: Boolean) =
    createGenerator(methodType).genVirtualForwarder(refClass, vnum, isInvokeInterface, receiverNullCheck)

  override final def genFieldOperation(field: Field, isWrite: Boolean, receiverNullCheck: Boolean) =
    createGenerator(DAIGenerator.methodTypeForDeferredFieldAccess(env.getTypeProvider, field, isWrite)).genFieldOperation(field, isWrite, receiverNullCheck)

  override final def genJSR292AppendixPlacer(methodType: MethodType, dai: Symbol, target: Symbol) =
    createGenerator(methodType).genJSR292AppendixPlacer(dai, target)
}
