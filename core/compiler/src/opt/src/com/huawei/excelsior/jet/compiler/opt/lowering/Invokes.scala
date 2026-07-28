/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.lowering

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.{CallKind, Field, MethodAJCallKind, MethodReferenceAccessKind, MethodType}

/**
 * Lowering of Invoke operations.
 *
 * @author alexm
 * @author kit
 */
private[lowering] trait Invokes extends Toolbox { self: Universe with Lowering =>

  private[lowering] def lowerAnyInvokeTarget(invokeTarget: AnyInvokeTarget): Node = {
    import MethodReferenceAccessKind._

    val ref = invokeTarget.targetRef
    val target = ref.method
    val isVarArgs = target.isVarArgs

    if (isVarArgs) {
      assert(ref.methodType.areVarArgsInitialized, "Method' var.arg types should be specified before lowering.")
    }
    
    if (ref.hasNonRecordReceiverParameter) {
      invokeTarget.receiver match {
        case rcv: AnyNull =>
          if (isO1Compiled) {
            // Devirtualization typically handles such cases, but in this mode it is disabled
            return addrNull
          } else {
            shouldNotReachHere(rcv)
          }

        case _ =>
      }
    }

    invokeTarget match {
      case t: InvokeInterfaceTarget =>
        assert(target.getAJCallKind == MethodAJCallKind.NORMAL)
        assert(ref.hasVirtualMethodSlot)
        assert(target.getDeclaringClass.isInterface)
        assert(!isVarArgs)

        t.ciao match {
          case IntegralConst(enrichment) => assert(enrichment > 0, s"non-positive constant enrichment $enrichment in $t")
          case _ =>
        }

        val desc = genInstanceDescriptorAddr(t.receiver)
        genInterfaceMethodAddr(desc, ref.virtualMethodSlot, t.ciao)

      case t: InvokeTarget =>
        ref.accessKind match {
          case VIRTUAL if ref.hasVirtualMethodSlot =>
            assert(!isVarArgs)
            getVirtualMethodAddr(ref, t.receiver)

          case INTERFACE => shouldNotReachHere("Interface calls target should be represented with InvokeInterfaceTarget")

          case _ => SymbolAddress(target)
        }

      case t => shouldNotReachHere(t)
    }
  }

  def callFromManagedToForeign(call: Call) = {
    rootMethod.hasManagedExecEnv && call.targetRef.methodType.isCJForeign
  }

  private def handlePendingException() = {
    val ee = ExecEnv()
    val threadEnv = GetField(RT.ExecEnv.threadEnv)(ee)
    val exceptionContext = GetField(RT.ThreadEnv.exceptionContext)(threadEnv)
    val pendingException = PublishRef(GetField(RT.ExceptionContext.pendingException)(exceptionContext))

    val noException = If(Cmp(TRefType, Condition.EQ)(pendingException, Null()))

    continue(noException.falseExit)
    PutField(RT.ExceptionContext.pendingException)(exceptionContext, IntegralConst(AddrType)(0))
    Throw(pendingException)
    Halt.afterThrow("pending exception")()

    continue(noException.trueExit)
  }

  private[lowering] def lowerCall(call: Call): Node = {
    assert(callFromManagedToForeign(call))

    val callCopy = safeLoweredNodeClone(call)

    handlePendingException()

    callCopy
  }
}
