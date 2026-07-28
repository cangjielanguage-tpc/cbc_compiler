/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.compiler.bytecode.Slots
import com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking.StructuredLockingAnalysisResult
import com.huawei.excelsior.jet.compiler.ir.{InlineContext, XInfo}
import com.huawei.excelsior.jet.compiler.newbaseline.backend.GlobalInfo
import com.huawei.excelsior.jet.compiler.newbaseline.frontend.*
import com.huawei.excelsior.jet.compiler.newbaseline.platforms.PlatformConfig
import com.huawei.excelsior.jet.compiler.options.BoolOption.AlwaysGenerateStructuredLockingChecksInBaseline
import com.huawei.excelsior.jet.compiler.symlevel.{Method, MethodAJCallKind}
import com.huawei.excelsior.jet.compiler.{CodeUnit, Environment, RTConst}

import scala.collection.mutable.ArrayBuffer

class MethodGenerator(env: Environment, platformConfig: PlatformConfig) {

  def genNormalMethod(method: Method): Unit = {
    val cf = ControlFlow.parse(env, method)
    val entryBlock = cf.entryBlock
    val allBlocks = ArrayBuffer.from(cf.allBlocks)

    val hasStructuredLocking = if (env.enabled(AlwaysGenerateStructuredLockingChecksInBaseline)) {
      false
    } else {

      import StructuredLockingAnalysisResult.*
      cf.structuredLockingState match {
        case STRUCTURED =>
          true

        case NOT_PAIRED_DUE_TO_JSRS =>
          false // known limitation of StructuredLockingAnalyzer, should not be reported

        case POTENTIALLY_UNSTRUCTURED =>
          false
      }
    }

    val exceptionHandlersTreeRoot = if (cf.handlersTree.isEmpty) null else cf.handlersTree.root

    CriticalEdges.eliminate(allBlocks, entryBlock)

    val blocks = TopSort.sortAndRemoveUnreachable(allBlocks, entryBlock, exceptionHandlersTreeRoot)

    val hasExceptionHandlers = if (exceptionHandlersTreeRoot != null) {
      val result = blocks.exists(_.isHandler)
      assert(result == blocks.exists(_.hasHandler), "there are handlers iff there are handled blocks")
      result
    } else {
      false
    }

    val slots = new Slots(cf.maxLocals, cf.maxStack)

    assert(blocks.
      forall(b =>
        b.inputs.forall(_.outputs.contains(b)) &&
          b.end.outputs.forall(_.inputs.contains(b.end))),
      "control flow graph is inconsistent")
    assert(blocks.forall(b => b.end.block == b),
      "control flow graph is inconsistent")

    val inlineContext = InlineContext.newRoot(method)

    val globalLiveness = GlobalLiveness.analyze(method, slots, blocks, entryBlock, hasExceptionHandlers)
    val globalInfo = new GlobalInfo(slots, blocks.size, globalLiveness, hasExceptionHandlers, hasStructuredLocking)
    val generator = platformConfig.makeMethodBytecodeGenerator(env, inlineContext, slots, globalInfo)

    generator.startMethod(entryBlock, blocks.head, blocks, hasExceptionHandlers, exceptionHandlersTreeRoot)
    for ((curBlock, i) <- blocks.zipWithIndex) {
      val nextBlock = if ((i + 1) < blocks.size) blocks(i + 1) else null
      val blockLiveness = new BlockLivenessAnalyzer(method, slots, globalInfo, curBlock)
      generator.genBlock(curBlock, nextBlock, blockLiveness)
    }
    val result = generator.finishMethod

    codegen.MethodGenerator.sendMethodCode(env, method, result.body, result.frame, result.xinfo)
  }

  /** Baseline compiler shouldn't generate specialized versions for methods.
    * If optimizing compiler failed to generate method version, here we generate a forwarder to the original method instead.
    *
    * TODO: it would be better to just re-target fixups from the versioned method body to the original method,
    *       but in current o2 implementation it isn't that easy.
    */
  def genVersionedMethod(codeUnit: CodeUnit): Unit = {
    assert(codeUnit.isVersionedMethod)
    val method = codeUnit.method
    val gen = platformConfig.getThunkGenerator(env, env.getSymbolLinker(method))
    val body = gen.genNonVirtualForwarder(method, method.getRealMethodType(null), receiverNullCheck = false)
    env.sendMethodCode(codeUnit, body, new XInfo, null, RTConst.MethodInfoFrameDescriptor.UNKNOWN_SIBERIA_OFFSET.intValue,
      null, shouldNotCallThis(_))
  }
}
