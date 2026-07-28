/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.{Environment, Stage}
import com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking.StructuredLockingAnalysisResult
import com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking.StructuredLockingAnalyzer.LockingInformation
import com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking.onbytecode.{MonitorEnter, MonitorExit, StructuredLockingAnalyzerOnBytecode}
import com.huawei.excelsior.jet.compiler.verifier.VerifiableMethod

import scala.reflect.ClassTag

/** Performs the same actions as [[CompleteControlFlowParser]] and also analyzes structured locking on bytecode. */
abstract class CompleteControlFlowParserAndStructuredLockingAnalyzer[B >: Null : ClassTag](env: Environment, method: VerifiableMethod)
  extends CompleteControlFlowParser[B](env, method, verify = false) { self =>

  private var _structuredLockingInfo: LockingInformation[MonitorEnter[B], MonitorExit[B]] = _

  protected def structuredLockingInfo: LockingInformation[MonitorEnter[B], MonitorExit[B]] =
    _structuredLockingInfo ensuring (_ != null)

  private var hasMonitorOps: Boolean = false

  override def parse(): Unit = {
    super.parse()

    env.stage(Stage.StructuredLockingAnalysis) {
      if (hasMonitorOps) {
        _structuredLockingInfo = new StructuredLocking().analyzeLocking()

        structuredLockingInfo.state match {
          case StructuredLockingAnalysisResult.STRUCTURED => // Ignore

          case StructuredLockingAnalysisResult.POTENTIALLY_UNSTRUCTURED =>
            if (hadSubroutines) {
              // It's known that StructuredLockingAnalyzer fails to handle
              // synchronized block inside of finally block in old class files.
              // We ignore this problem possibly concealing some other problems. :/
              _structuredLockingInfo = LockingInformation.potentiallyUnstructuredDueToJSRs
            }

          case state => shouldNotReachHere(state)
        }
      } else {
        _structuredLockingInfo = LockingInformation.empty
      }
    }
  }

  private class StructuredLocking extends StructuredLockingAnalyzerOnBytecode[B](self.codeAttr) {
    override protected def blockStartPC(block: B)       = self.blockStartPC(block)
    override protected def blockEndPC(block: B)         = self.blockEndPC(block)
    override protected def entryBlock                   = self.entryBlock
    override protected def succBlocks(block: B)         = self.succBlocks(block)
    override protected def handlers(block: B)           = self.handlers(block)
    override protected def blockHasNormalExit(block: B) = self.blockHasNormalExit(block)
  }

  /** Returns whether `block` may normally exit from method (i.e. ends with `return` instruction). */
  protected def blockHasNormalExit(block: B): Boolean

  override final def addMonitorOp(bc: Int, block: B, isEnter: Boolean): Unit = {
    hasMonitorOps = true
  }
}
