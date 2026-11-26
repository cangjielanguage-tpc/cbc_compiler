/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.symlevel.{CallConv, SignatureType}

class SyncIntrinsicsSuite extends CompilerSuite with GlobalNodesBuilder {

  private def checkCall(node: Node, symbol: String, args: Seq[Node], params: Seq[SignatureType], result: SignatureType): Unit = {
    val call = node.asInstanceOf[Call]
    CJIntrinsic.unapply(call).map(_.name) shouldBe Some(symbol)
    call.target.asInstanceOf[CJIntrinsicTarget].target.name shouldBe symbol
    call.invokeArgs shouldBe args
    call.methodType.parameterTypes.toSeq shouldBe params
    call.methodType.returnType shouldBe result
    call.methodType.callConv shouldBe CallConv.MANAGED
    call.methodType.hasRetByValParameter shouldBe false
    call.methodType.hasOuterTypeInfoParameter shouldBe false
    call.methodType.hasThisTypeInfoParameter shouldBe false
  }

  test("Mutex operations forward to full runtime entry points with real Bool or empty Unit results") {
    val mutexType = tCangjieType.sigType
    currentScope.inState(entryBlock, entryMemory) {
      val mutex = Fake(TRefType)
      val args = Seq(mutex)
      val params = Seq(mutexType)
      checkCall(CJIntrinsic.mutexInit(mutexType)(mutex), "CJ_MCC_MutexInit", args, params, SignatureType.Boolean)
      checkCall(CJIntrinsic.mutexLock(mutexType)(mutex), "CJ_MCC_MutexLock", args, params, SignatureType.Void)
      checkCall(CJIntrinsic.mutexTryLock(mutexType)(mutex), "CJ_MCC_MutexTryLock", args, params, SignatureType.Boolean)
      checkCall(CJIntrinsic.mutexCheckStatus(mutexType)(mutex), "CJ_MCC_MutexCheckStatus", args, params, SignatureType.Boolean)
      checkCall(CJIntrinsic.mutexUnlock(mutexType)(mutex), "CJ_MCC_MutexUnlock", args, params, SignatureType.Void)
    }
  }

  test("Monitor and waitqueue initialization use distinct runtime symbols") {
    val monitorType = tCangjieType.sigType
    currentScope.inState(entryBlock, entryMemory) {
      val monitor = Fake(TRefType)
      val queue = Fake(TRefType)
      checkCall(CJIntrinsic.monitorInit(monitorType)(monitor), "CJ_MCC_WaitQueueForMonitorInit",
        Seq(monitor), Seq(monitorType), SignatureType.Boolean)
      checkCall(CJIntrinsic.waitQueueInit(monitorType)(queue), "CJ_MCC_WaitQueueInit",
        Seq(queue), Seq(monitorType), SignatureType.Boolean)
    }
  }

  test("Monitor wait forwards its nanosecond timeout and preserves the runtime Bool result") {
    val monitorType = tCangjieType.sigType
    currentScope.inState(entryBlock, entryMemory) {
      val monitor = Fake(TRefType)
      val timeout = LConst(123456789L)
      checkCall(CJIntrinsic.monitorWait(monitorType)(monitor, timeout), "CJ_MCC_MonitorWait",
        Seq(monitor, timeout), Seq(monitorType, SignatureType.Int64), SignatureType.Boolean)
      checkCall(CJIntrinsic.monitorNotify(monitorType)(monitor), "CJ_MCC_MonitorNotify",
        Seq(monitor), Seq(monitorType), SignatureType.Void)
      checkCall(CJIntrinsic.monitorNotifyAll(monitorType)(monitor), "CJ_MCC_MonitorNotifyAll",
        Seq(monitor), Seq(monitorType), SignatureType.Void)
    }
  }

  test("Multi-condition operations forward the waitqueue reference, not a ConditionID record or metadata") {
    val monitorType = tCangjieType.sigType
    val queueType = SignatureType.CangjieArray(SignatureType.UInt8)
    currentScope.inState(entryBlock, entryMemory) {
      val monitor = Fake(TRefType)
      val queue = Fake(TRefType)
      val timeout = LConst(Long.MaxValue)
      checkCall(CJIntrinsic.multiConditionWait(monitorType, queueType)(monitor, queue, timeout), "CJ_MCC_MultiConditionMonitorWait",
        Seq(monitor, queue, timeout), Seq(monitorType, queueType, SignatureType.Int64), SignatureType.Boolean)
      checkCall(CJIntrinsic.multiConditionNotify(monitorType, queueType)(monitor, queue), "CJ_MCC_MultiConditionMonitorNotify",
        Seq(monitor, queue), Seq(monitorType, queueType), SignatureType.Void)
      checkCall(CJIntrinsic.multiConditionNotifyAll(monitorType, queueType)(monitor, queue), "CJ_MCC_MultiConditionMonitorNotifyAll",
        Seq(monitor, queue), Seq(monitorType, queueType), SignatureType.Void)
    }
  }
}
