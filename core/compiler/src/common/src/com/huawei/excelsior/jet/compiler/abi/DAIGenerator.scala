/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.{Segment, Symbol}
import com.huawei.excelsior.jet.common.DAIRefKind
import com.huawei.excelsior.jet.common.DAIRefKind.*
import com.huawei.excelsior.jet.compiler.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.abi.DAIGenerator.{DAITarget, NO_INDEX}
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.MethodReferenceAccessKind.*
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.{SpecialParamSet, SpecialParameter}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.JBCReference

/** Deferred Access Info for Java.
  * TODO: move to Java-specific part.
  */
object DAIGenerator {
  private val NO_INDEX = -1
  val NO_CP_INDEX = NO_INDEX

  class DAITarget private[abi](val symbol: Symbol) {
    override def toString = getClass.getName // to avoid hash codes in logs
  }

  def apply(env: Environment, symbolLinker: SymbolLinker, rootMethod: Method) = new DAIGenerator(env, symbolLinker, rootMethod)

  /** For DAI-based field access, [[com.huawei.excelsior.jet.compiler.ThunkGeneratorBase]] generates managed thunks with following signatures:
    *  - `T getStaticField()`
    *  - `T getInstanceField(Object object)`
    *  - `void setStaticField(T value)`
    *  - `void setInstanceField(T value, Object object)`
    *
    * Code generators may represent method parameters in different ways
    * (e.g. as com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.Node or com.huawei.excelsior.jet.compiler.opt.ir.Nodes.Node)
    * and this utility class is used to unify parameter ordering among them. "N" type parameter corresponds to
    * underlying "Node" type. See [[methodTypeForDeferredFieldAccess]] for example.
    */
  object FieldAccessParametersOrdering {
    def getObject[N >: Null](parameters: collection.Seq[N], isWrite: Boolean, isStatic: Boolean): N = {
      (isWrite, isStatic) match {
        case (_, true)      => null
        case (true, false)  => parameters(1)
        case (false, false) => parameters(0)
      }
    }

    def getFieldValue[N >: Null](parameters: collection.Seq[N], isWrite: Boolean): N =
      if (isWrite) parameters(0) else null

    def forMethodInvocation[N](`object`: N, fieldValue: N, isWrite: Boolean, isStatic: Boolean): Seq[N] = {
      (isWrite, isStatic) match {
        case (true, true)   => Seq(fieldValue)
        case (true, false)  => Seq(fieldValue, `object`)
        case (false, true)  => Seq.empty
        case (false, false) => Seq(`object`)
      }
    }
  }

  def methodTypeForDeferredFieldAccess(typeProvider: TypeProvider, field: Field, isWrite: Boolean): MethodType =
    methodTypeForDeferredFieldAccess(typeProvider, field.getType, field.isStatic, isWrite)

  def methodTypeForDeferredFieldAccess(typeProvider: TypeProvider, fieldType: SignatureType, isStatic: Boolean, isWrite: Boolean): MethodType = {
    val objectType = JBCReference(typeProvider.getObjectType)
    val paramTypes = FieldAccessParametersOrdering.forMethodInvocation(objectType, fieldType, isWrite, isStatic)

    val voidType = SignatureType.Void
    val returnType = if (isWrite) voidType else fieldType

    val sig = MethodSignature(returnType, paramTypes)
    MethodType(sig, CallConv.MANAGED, CallKind.NORMAL, SpecialParamSet(), false, MethodType.UNINITIALIZED_FIRST_VAR_ARG)
  }

  private def encodeFlag(flag: Boolean, shift: Int) = {
    assert(0 <= shift && shift < 8)
    ((if (flag) 1 else 0) << shift).toShort
  }

  private def flagsEncodingDAI(isJIT: Boolean, isNonNull: Boolean, hasAppendix: Boolean) =
    (encodeFlag(isJIT, RTConst.KindAndFlags.BitNumber.IS_JIT.intValue) |
      encodeFlag(isNonNull, RTConst.KindAndFlags.BitNumber.IS_NON_NULL_RECEIVER.intValue) |
      encodeFlag(hasAppendix, RTConst.KindAndFlags.BitNumber.HAS_APPENDIX.intValue)).toShort
}

final class DAIGenerator private(env: Environment, symbolLinker: SymbolLinker, rootMethod: Method) {

  def forFieldOperation(cpIndex: Int, isWrite: Boolean, isStatic: Boolean, receiverNonNull: Boolean): DAITarget = {
    val refKind = (isWrite, isStatic) match {
      case (true, true)   => DAIRefKind.PUT_STATIC
      case (true, false)  => DAIRefKind.PUT_FIELD
      case (false, true)  => DAIRefKind.GET_STATIC
      case (false, false) => DAIRefKind.GET_FIELD
    }
    generate(refKind, cpIndex, RTSProc.JR_ResolveDeferred, receiverNonNull, hasAppendix = false, NO_INDEX, NO_INDEX)
  }

  def forUnresolvedInvoke(mak: MethodReferenceAccessKind, cpIndex: Int, receiverNonNull: Boolean): DAITarget = {
    val refKind = mak match {
      case STATIC => DAIRefKind.INVOKE_STATIC
      case VIRTUAL => DAIRefKind.INVOKE_VIRTUAL
      case INTERFACE => DAIRefKind.INVOKE_INTERFACE
      case SPECIAL => DAIRefKind.INVOKE_SPECIAL
      case MUT | STATIC_VIRTUAL => shouldNotReachHere(mak)
    }
    generate(refKind, cpIndex, RTSProc.JR_ResolveDeferred, receiverNonNull, hasAppendix = false, NO_INDEX, NO_INDEX)
  }

  def forIndyOrSigpoly(refKind: DAIRefKind, cpIndex: Int, hasAppendix: Boolean): DAITarget = {
    assert((refKind == DAIRefKind.INVOKE_DYNAMIC) || (refKind == DAIRefKind.INVOKE_SIGPOLY))
    generate(refKind, cpIndex, RTSProc.JR_ResolveJSR292, receiverNonNull = false, hasAppendix = hasAppendix, NO_INDEX, NO_INDEX)
  }

  def forDirectCPEntryOperation(cpIndex: Int): DAITarget =
    generate(DAIRefKind.DIRECT_CP_ENTRY_OPERATION, cpIndex, null, receiverNonNull = true, hasAppendix = false, NO_INDEX, NO_INDEX)

  private def generate(refKind: DAIRefKind, cpIndex: Int, resolver: RTSProc, receiverNonNull: Boolean, hasAppendix: Boolean, declClassIndex: Int, methodIndex: Int) = {
    assert(refKind != DAIRefKind.UNDEFINED)
    assert(cpIndex >= 0)

    val refKindEncoding = (refKind.ordinal & RTConst.KindAndFlags.REF_KIND_MASK.intValue).toByte
    assert((refKindEncoding & 0xFF) == refKind.ordinal, "refKind overflow")

    val daiSymbol = symbolLinker.makeDataSymbol()
    val dai = new Segment(daiSymbol)
    dai.alignStart(RTConst.DeferredAccessInfo.alignment)

    if (resolver != null) {
      dai.addDataAddress(env.getRTSProc(resolver), targetArch)
    } else {
      assert(refKind == DAIRefKind.DIRECT_CP_ENTRY_OPERATION)
      dai.putZeroes(addressSize)
    }

    val flags = DAIGenerator.flagsEncodingDAI(isJIT, receiverNonNull, hasAppendix)
    dai.putW16((refKindEncoding | flags) & 0xFFFF) // unsigned cast

    dai.putW16(cpIndex)

    val NO_VNUM = RTConst.DeferredAccessInfo.InvokeVirtualOrInterface.NO_VNUM.intValue & 0xFFFF

    refKind match {
      case INVOKE_SPECIAL | INVOKE_STATIC | DIRECT_CP_ENTRY_OPERATION =>

      case INVOKE_VIRTUAL | INVOKE_INTERFACE =>
        dai.putW16(NO_VNUM) // vnum
        // TODO: uncomment when needed
        // if (refKind == DAIRefKind.INVOKE_INTERFACE) {
        //   if (needParamSizeAfterResolve) {
        //     dai.addW16(0) // alignment
        //   }
        //   dai.addW16(0) // interface import idx
        //   dai.addNullAddress(addressSize)
        // }

      case GET_STATIC | PUT_STATIC =>
        // TODO: uncomment when needed (for non-specialized thunks on iOS)
        // dai.addZeroes(addressSize - 4)
        // dai.addNullAddress(addressSize)

      case GET_FIELD | PUT_FIELD =>
        // TODO: uncomment when needed (for non-specialized thunks on iOS)
        // dai.addW32(0)

      case INVOKE_DYNAMIC | INVOKE_SIGPOLY =>
        // always allocate place to store either `appendix` (only if `dai.hasAppendix`)
        // or an object encapsulating resolved target to be interpreted (together with its `appendix`, if any, or `null` otherwise).
        dai.putZeroes(addressSize - 4)
        dai.putZeroes(addressSize)

      case _ => shouldNotReachHere()
    }

    symbolLinker.sendData(dai, rootMethod)
    new DAITarget(daiSymbol)
  }
}
