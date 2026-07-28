/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.lowering

import com.huawei.excelsior.jet.compiler.abi.DAIGenerator
import com.huawei.excelsior.jet.compiler.abi.DAIGenerator.DAITarget
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.common.BuiltInField
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.{BytecodeMethodReference, Field, MethodReference, MethodReferenceAccessKind, SignatureType}
import com.huawei.excelsior.jet.compiler.RTSProc
import com.huawei.excelsior.jet.compiler.bytecode.MethodAccessKind
import com.huawei.excelsior.jet.compiler.bytecode.MethodAccessKind.STATIC

import scala.annotation.nowarn

// TODO: remove when scala 3 is supported (see https://github.com/scala/bug/issues/4440)
@nowarn("msg=The outer reference in this type test cannot be checked at run time")
private[lowering] trait DeferredOps extends Toolbox with MiscOps { self: Universe =>

  private lazy val daiGenerator = DAIGenerator(env, symbolLinker, rootMethod)

  private[lowering] def lowerDeferredOp(op: Deferred): Node = {
    // TODO: extend or simplify
    sealed trait DeferredInfo
    case class Unresolved(rtCPIndex: Int) extends DeferredInfo

    val deferredInfo = op.proto match {
      case proto: Deferred.Unresolved =>
        Unresolved(hostingClass.getClassConstantPool.getRuntimeIndex(op.inlineContext.klass.getClassConstantPool, proto.cpIndex))

      case x => shouldNotReachHere(x)
    }

    op.proto match {
      case Deferred.ClassObject.Proto(_) =>
        assert(op.extraArgs.isEmpty)
        val Unresolved(rtCPIndex) = deferredInfo
        val dai = daiGenerator.forDirectCPEntryOperation(rtCPIndex)
        RTSCall(RTSProc.JR_DeferredGetClassObject)(daiAddr(dai))

      case Deferred.New.Proto(_) =>
        assert(op.extraArgs.isEmpty)
        val Unresolved(rtCPIndex) = deferredInfo
        val dai = daiGenerator.forDirectCPEntryOperation(rtCPIndex)
        RTSCall(RTSProc.JR_DeferredNew)(daiAddr(dai))

      case Deferred.NewArray.Proto(_, allDimNum, dimSpec) =>
        val dimLengths = op.extraArgs ensuring (_.length == dimSpec)
        val dimLenghtsArray = stackAllocArrayOfInts(dimLengths)
        val Unresolved(rtCPIndex) = deferredInfo
        val dai = daiGenerator.forDirectCPEntryOperation(rtCPIndex)
        RTSCall(RTSProc.JR_DeferredNewArray)(daiAddr(dai), IConst(allDimNum), IConst(dimSpec), dimLenghtsArray)

      case Deferred.InstanceOf.Proto(_) =>
        val Seq(obj) = op.extraArgs
        val Unresolved(rtCPIndex) = deferredInfo
        val dai = daiGenerator.forDirectCPEntryOperation(rtCPIndex)
        RTSCall(RTSProc.JR_DeferredInstanceof)(daiAddr(dai), obj)

      case Deferred.CheckCast.Proto(_) =>
        val Seq(obj) = op.extraArgs
        val Unresolved(rtCPIndex) = deferredInfo
        val dai = daiGenerator.forDirectCPEntryOperation(rtCPIndex)
        RTSCall(RTSProc.JR_DeferredCast)(daiAddr(dai), obj)

      case info @ Deferred.FieldOp.Proto(_, fieldType, isWrite, isStatic) =>
        val params = op.extraArgs
        val receiverNonNull = isStatic || isNonNull(info.objArg(params))
        val thunkMT = DAIGenerator.methodTypeForDeferredFieldAccess(typeProvider, fieldType, isStatic, isWrite)
        val Unresolved(rtCPIndex) = deferredInfo
        val dai = daiGenerator.forFieldOperation(rtCPIndex, isWrite, isStatic, receiverNonNull)
        val thunkRef = new MethodReference(thunkMT, MethodReferenceAccessKind.STATIC)
        DAICall(thunkRef, dai)(params: _*)

      case Deferred.DynamicOrSigPolyInvoke.Proto(_, refKind, methodType, hasAppendix) =>
        val Unresolved(rtCPIndex) = deferredInfo
        val dai = daiGenerator.forIndyOrSigpoly(refKind, rtCPIndex, hasAppendix)
        val targetRef = new MethodReference(methodType, MethodReferenceAccessKind.STATIC, null, null, null)
        DAICall(targetRef, dai)(op.extraArgs: _*)

      case Deferred.SigPolyInvokeBasic.Proto(_, methodType) =>
        val methodHandle = op.extraArgs.head
        val targetRef = new BytecodeMethodReference(methodType, STATIC, isMemberNameInvoke = true)

        val form = GetField(Java.Lang.Invoke.MethodHandle.form)(methodHandle)
        val vmentry = GetField(Java.Lang.Invoke.LambdaForm.vmentry)(form)
        RTSCall(RTSProc.JR_MemberNamePreparationCheck)(vmentry)
        
        val entryPoint = GetField(Java.Lang.Invoke.MemberName.entryPoint)(vmentry)
        assert(op.inlineContext.method.isManaged && targetRef.methodType.callConv.isManaged)

        // Code that uses this field to pass managed object must be written carefully so that 
        // there are no GCPoints between writing to that field and reading from it in the unmanaged part of hook.
        // TODO: move this PutField closer to codegen
        PutField(RT.ExecEnv.appendixArgumentOfHookInvoker)(ExecEnv(), ConcealRef(vmentry))

        // "Everybody lies" - it's better to never enrich params and always deprive result of such dynamic calls.
        depriveIfNeeded(Call(targetRef)(entryPoint +: op.extraArgs: _*))

      case Deferred.Invoke.Proto(targetRef) =>
        val params = op.extraArgs
        val dai = deferredInfo match {
          case Unresolved(rtCPIndex) =>
            val receiverNonNull = !targetRef.hasReceiverParameter || isNonNull(params(targetRef.getReceiverArgIndex))
            daiGenerator.forUnresolvedInvoke(targetRef.accessKind, rtCPIndex, receiverNonNull)
        }
        DAICall(targetRef, dai)(params: _*)

      case Deferred.MethodType.Proto(_) =>
        val Unresolved(rtCPIndex) = deferredInfo
        val dai = daiGenerator.forDirectCPEntryOperation(rtCPIndex)
        RTSCall(RTSProc.JR_LoadConstantMethodTypeThroughDAI)(daiAddr(dai))
        
      case Deferred.MethodHandle.Proto(_) =>
        val Unresolved(rtCPIndex) = deferredInfo
        val dai = daiGenerator.forDirectCPEntryOperation(rtCPIndex)
        RTSCall(RTSProc.JR_LoadConstantMethodHandleThroughDAI)(daiAddr(dai))
    }
  }

  /** Returns true if given node is always non-Null.
    *
    * TODO: make this check more precise by using context types
    *       One possible solution is to store context types info for deferred receivers *before* lowering.
    *       Another is to implement `loweredNodeTypeAt`.
    */
  private def isNonNull(n: Node) = !nodeType(n).mayBeNull

  private def daiAddr(dai: DAITarget) = SymbolAddress(dai.symbol)
}
