/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.frontend.bytecode

import com.huawei.excelsior.jet.codeemitter.BarrierKind.STORE_STORE
import com.huawei.excelsior.jet.compiler.RTSProc.JR_FatalError
import com.huawei.excelsior.jet.compiler.Stage
import com.huawei.excelsior.jet.compiler.opt.frontend.{AJReplacedLoading, LambdaLoading, XScalaLoading}
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, Types, Universe}
import com.huawei.excelsior.jet.compiler.opt.middle.{DCEComponent, UCEComponent}
import com.huawei.excelsior.jet.compiler.options.BoolOption.GenerateFatalErrorOnUnstructuredLockingInOpt
import com.huawei.excelsior.jet.compiler.symlevel.Method

/**
 * Java bytecode parser.
 *
 * @author paul
 * @author conwor
 * @author cypok
 */
trait JBCParser extends ControlFlow with DataFlow with UCEComponent with DCEComponent with AJReplacedLoading with LambdaLoading with XScalaLoading { self: Universe =>

  private def loadNormal(method: Method, args: Seq[Node]): Return = {
    val cfState = withFreeUnreachableBlocks { makeCFG(method, entryBlock) }
    dbgPrinter.debugCFG("CFG after parsing")

    processBackwardBranches()
    dbgPrinter.debugCFG("CFG after backward branches processing")
    dbgPrinter.debugNodes("Nodes after backward branches processing")
    dbgPrinter.debugGraphs("Graph after backward branches processing")
    checkGraphConsistency(CheckLevels.Important, cfg)
    checkIRConsistency(CheckLevels.Important)

    def convertNullAndProxy(tpe: Type, n: Node) = (n, n.tpe, tpe) match {
      case (_: AnyNull, EopType.Null, EopType.Plain | EopType.Eop(_)) => AnyNull(tpe)

      // Adjust type in case when target expects the rich value
      case (_: AnyNull, EopType.Plain, EopType.Eop(_)) => AnyNull(tpe)

      case (_: Proxy, EopType.Plain, EopType.Eop(_)) => ReinterpretCast(n.tpe, tpe)(n)
      case _ => n
    }

    val parseInfo = Node.withImplicitArgConversion(convertNullAndProxy) {
      parseMethod(method, cfState, args)
    }
    dbgPrinter.debugNodes("All graph after BCP")
    dbgPrinter.debugGraphs("Graph after BCP")

    if (isUnstructuredLocking && env.enabled(GenerateFatalErrorOnUnstructuredLockingInOpt) && !rootDeclaringClass.isCangjieType && !isO1Compiled) {
      // Methods from bytecode compiled with O2 must have structured locking (or not to be executed in run-time).
      // To not generate potentially incorrect code we replace these methods with FatalError invocation.
      val goto = Block.splitAfter(entryBlock)
      insertCodeBefore(goto) {
        RTSCall(JR_FatalError)(AJString.bstr(s"Method $rootMethod with unstructured locking executed"))
      }
      replaceByHalt(goto)
      dbgPrinter.debugNodes("All graph after unstructured locking fatal generated")
    }

    if (!isO1Compiled) {
      if (eliminateUnreachableCode()) {
        dbgPrinter.debugNodes("All graph after UCE")
      }
      // Cleanup proxies
      if (eliminateDeadCode()) {
        dbgPrinter.debugNodes("All graph after DCE")
      }
    }

    // Must be done after all parsing optimizations that might accidentally remove this unified return (see JET-16162).
    val ret = processReturns(method, parseInfo)
    dbgPrinter.debugNodes("All graph after returns unification")

    ret
  }

  private def loadSynchronized(method: Method, args: Seq[Node]) = {
    val syncObj = if (method.isStatic) {
      // Note: declaring class must be prepared inside static method
      // TODO: assert it
      ClassObject(method.getDeclaringClass)(entryBlock)
    } else {
      args(method.getReceiverArgIdx)
    }

    val outermostSyncRegion = SynchronizedRegion(None)

    val monitorEnter = MonitorEnter(entryBlock, entryMemory, syncObj, outermostSyncRegion)

    val (body, _) = createScope(Scope.createAnchor(monitorEnter), currentMethodSyntheticPos, None) {
      val ret = loadNormal(method, args)
      val thrw = constructSingleHandler()
      dbgPrinter.debugGraphs("after load normal for synchronized")

      // append MonitorExit just before Return and Throw
      def monitorExitBefore(exit: LowerPoint with HasInMemory): Unit = {
        if (exit != null) {
          val monitorExitBlock = Block.splitBefore(exit).target
          assert(!monitorExitBlock.hasXHandlers)
          insertCodeBefore(exit) {
            MonitorExit(syncObj, outermostSyncRegion)
          }
        }
      }

      monitorExitBefore(ret)
      monitorExitBefore(thrw)

      currentScope.setResult(ret)
    }

    if (isStructuredLocking) {
      val existingOuterRegions = body.allNodes collect { case s: SynchronizedRegion if s != outermostSyncRegion && s.isOutermost => s }
      existingOuterRegions foreach { _.outer = outermostSyncRegion }
    }

    body.merge()
    body.exitPoint
  }

  def loadJBCMethod(method: Method, args0: Seq[Node]): RTPartsInfo = stage(Stage.LoadJBC) {
    val args = depriveMethodArgs(method.getMethodType, args0)
    val (ret, message) = method match {
      case _ if method.isAJReplaced                    => (loadAJReplaced(method, args),   "after load AJReplaced)")
      case _ if method.isSynchronized                  => (loadSynchronized(method, args), "after load synchronized")
      case _ if method.getDeclaringClass.isLambdaClass => (loadLambda(method, args),       "after load lambda")

      case _ if method.getDeclaringClass.isXScalaType && method == XScala.AnyRef.`init` =>
        (loadXScalaAnyRefInit(), "after load xscala/AnyRef.init")

      case _                                           => (loadNormal(method, args),       "after load normal")
    }

    dbgPrinter.debugNodes(message)
    dbgPrinter.debugGraphs(message)
    currentScope.setResult(ret)


    RTPartsInfo(
      isDirtyForClassGC = method.getDeclaringClass.isAJType && method.isDirtyForClassGC,
    )
  }

  private def processReturns(method: Method, parseInfo: ParseInfo): Return = {
    val returnType = method.getReturnType
    val retValType = ValueType.fromSig(returnType, instantiateRich = true)
    val ret = unifyReturns(retValType)
    if (ret != null && parseInfo.needMemBarBeforeReturn) {
      insertCodeBefore(ret) { MemBarrier(Set(STORE_STORE))() }
    }
    ret
  }
}
