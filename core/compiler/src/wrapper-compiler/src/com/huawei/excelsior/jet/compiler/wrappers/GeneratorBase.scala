/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.wrappers

import com.huawei.excelsior.jet.assembler.Location.{IReg, Mem, mem}
import com.huawei.excelsior.jet.compiler.{RTConst, TypeProvider}
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.Node
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind

abstract class GeneratorBase(val ctx: GeneratorContext) {
  val env = ctx.env
  implicit val typeProvider: TypeProvider = env.getTypeProvider
  val wrapper = ctx.wrapper
  val globalLocations = ctx.globalLocations
  val frame = ctx.frame
  val locations = ctx.locations
  val nodes = ctx.nodes
  val emit = ctx.emit
  val gen = ctx.gen

  def receiveParameters() = gen.receiveAllParameters()

  def handleReturnValue(returnValue: Node): Unit = {
    val returnType = wrapper.getReturnType
    if (!returnType.isZST) {
      assert(returnValue != null)
      gen.genReturnValue(returnValue)
    }
  }

  // Following two methods must be used with extreme care
  // as they only allow to handle instantiated exception, but not pending hardware ones.
  def getPendingExceptionLoc(tmp: IReg): Mem = {
    emit.load(tmp, mem(TypeKind.CLASS.toAsm, frame.EER, RTConst.ExecEnv.Offsets.threadEnv.intValue))
    mem(TypeKind.CLASS.toAsm, tmp, RTConst.ThreadEnv.exceptionContext.offset + RTConst.ExceptionContext.pendingExceptionObj.offset)
  }

  def genGetAndClearPendingException(result: IReg, tmp: IReg): Unit = {
    val xobjInEE = getPendingExceptionLoc(tmp)
    emit.load(result, xobjInEE)
    emit.storeNull(xobjInEE)
  }
}
