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

class FutureThreadIntrinsicsSuite extends CompilerSuite with GlobalNodesBuilder {

  private def checkCall(node: Node, target: CJIntrinsicType, args: Seq[Node], result: SignatureType): Call = {
    val call = node.asInstanceOf[Call]
    CJIntrinsic.unapply(call) shouldBe Some(target)
    call.invokeArgs shouldBe args
    call.methodType.returnType shouldBe result
    call.methodType.callConv shouldBe CallConv.MANAGED
    call.methodType.hasOuterTypeInfoParameter shouldBe false
    call.methodType.hasThisTypeInfoParameter shouldBe false
    call
  }

  test("Future intrinsics call the runtime with a reference and nanosecond timeout, not generic metadata") {
    val futureType = tCangjieType.sigType
    currentScope.inState(entryBlock, entryMemory) {
      val future = Fake(TRefType)
      val timeout = LConst(123456789L)
      val init = checkCall(CJIntrinsic.futureInit(futureType)(future), CJIntrinsicType.FutureInit, Seq(future), SignatureType.Void)
      val complete = checkCall(CJIntrinsic.futureIsComplete(futureType)(future), CJIntrinsicType.FutureIsComplete, Seq(future), SignatureType.Boolean)
      val wait = checkCall(CJIntrinsic.futureWait(futureType)(future, timeout), CJIntrinsicType.FutureWait, Seq(future, timeout), SignatureType.Void)
      val notify = checkCall(CJIntrinsic.futureNotifyAll(futureType)(future), CJIntrinsicType.FutureNotifyAll, Seq(future), SignatureType.Void)

      init.methodType.hasRetByValParameter shouldBe false
      complete.methodType.hasRetByValParameter shouldBe false
      wait.methodType.parameterTypes.toSeq shouldBe Seq(futureType, SignatureType.Int64)
      notify.methodType.hasRetByValParameter shouldBe false
      Seq(init, complete, wait, notify).map(_.target.asInstanceOf[CJIntrinsicTarget].target.name) shouldBe Seq(
        "CJ_MCC_FutureInit", "CJ_MCC_FutureIsComplete", "CJ_MCC_FutureWait", "CJ_MCC_FutureNotifyAll")
    }
  }

  test("Sleep and thread-object intrinsics preserve runtime results and argument order") {
    val threadType = tCangjieType.sigType
    currentScope.inState(entryBlock, entryMemory) {
      val thread = Fake(TRefType)
      val timeout = LConst(42L)
      val sleep = checkCall(CJIntrinsic.sleep(timeout), CJIntrinsicType.Sleep, Seq(timeout), SignatureType.Void)
      val set = checkCall(CJIntrinsic.setThreadObject(threadType)(thread), CJIntrinsicType.SetThreadObject, Seq(thread), SignatureType.Void)
      val inited = checkCall(CJIntrinsic.isThreadObjectInited(), CJIntrinsicType.IsThreadObjectInited, Seq.empty, SignatureType.Boolean)
      val get = checkCall(CJIntrinsic.getThreadObject(threadType)(), CJIntrinsicType.GetThreadObject, Seq.empty, threadType)

      Seq(sleep, set, inited, get).forall(!_.methodType.hasRetByValParameter) shouldBe true
      Seq(sleep, set, inited, get).map(_.target.asInstanceOf[CJIntrinsicTarget].target.name) shouldBe Seq(
        "CJ_MRT_Sleep", "CJ_MCC_SetCurrentCJThreadObject", "CJ_MCC_IsThreadObjectInited", "CJ_MCC_GetCurrentCJThreadObject")
    }
  }

  test("Snapshot intrinsics pass allocation TypeInfos and use sret only for the record result") {
    // The runtime fills ThreadSnapshotInfo records; this synthetic record tests the same ABI shape.
    val recordType = SignatureType.Tuple(Seq(SignatureType.Int64, SignatureType.CangjieArray(SignatureType.UInt8)))
    val arrayType = SignatureType.CangjieArray(recordType)
    currentScope.inState(entryBlock, entryMemory) {
      val arrayInfo = Fake(AddrType)
      val stackTraceInfo = Fake(AddrType)
      val charArrayInfo = Fake(AddrType)
      val sret = Fake(ValueType(recordType))
      val all = checkCall(CJIntrinsic.dumpAllThreadsInfo(arrayType)(arrayInfo, stackTraceInfo, charArrayInfo),
        CJIntrinsicType.DumpAllThreadsInfo, Seq(arrayInfo, stackTraceInfo, charArrayInfo), arrayType)
      val current = checkCall(CJIntrinsic.dumpCurrentThreadInfo(recordType)(sret, stackTraceInfo, charArrayInfo),
        CJIntrinsicType.DumpCurrentThreadInfo, Seq(sret, stackTraceInfo, charArrayInfo), recordType)

      all.methodType.hasRetByValParameter shouldBe false
      current.methodType.hasRetByValParameter shouldBe true
      current.methodType.getRetByValArgIdx shouldBe 0
      all.target.asInstanceOf[CJIntrinsicTarget].target.name shouldBe "CJ_MCC_GetAllThreadSnapshot"
      current.target.asInstanceOf[CJIntrinsicTarget].target.name shouldBe "CJ_MCC_GetCurrentThreadSnapshot"
    }
  }
}
