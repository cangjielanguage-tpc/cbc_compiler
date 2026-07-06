/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.serialization

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.RTSProc
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.serialization.{BinaryIO, SerializationError}
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, Method}
import xscala.io.{DataInput, DataOutput}

import collection.mutable

/**
 * Serialized IR writer and reader.
 *
 * TODO: implement packing of int values
 *
 * @author cypok
 * @author conwor
 * @author alexm
 */
trait IOComponent extends BinaryIO { self: Universe =>

  /**
   * Version of serialized IR format.
   * Must be incremented if args/types of existing nodes are changed.
   * Adding of a new node to the end of SupportedProtos list does not require incrementing of the version.
   */
  val IRFormatVersion = 138

  /**
   * Node identifier.
   */
  type NodeId = Int

  /**
   * List of supported prototype objects.
   * A new object can be safely added to the end of this list (unless MaxSupportedProtos is exceeded).
   */
   private val SupportedProtos: Seq[AnyRef] = Seq(
    BBlock, XBlock, Goto, If, Return.Proto, Throw, Halt.Proto, Void, True, False,
    NullCheck.Proto, Clinit.Proto, ConstString.Proto,
    New.Proto, JavaArrayLength, ScalaArrayLength, AJArrayLength, CangjieArrayLength, ArrayIndexCheck.Proto, ArrayStoreCheck.Proto,
    IConst, LConst, FConst, DConst, ClassObject.Proto,
    AJString, StackAlloc,
    InstanceOf.Proto, ExecEnv, StackDescriptor, MemBarrier.Proto, ColdCodeMarker,
    Catch, CheckCast.Proto, NewArray.Proto,
    SymbolAddress, MonitorEnter, MonitorExit, VarArguments, Switch.Proto,
    ImportedIndex, MathIntrinsic.Proto, CondVal.Proto,
    Enrich.Proto, Deprive.Proto, WeakCast.Proto, GCPoint, TrapCheck,
    ICRegionEnter.Proto, ICRegionExit.Proto,
    NewString,
    FrameHeader, AJCallerClass.Proto, ClinitedAssert.Proto, InitializedAssert.Proto,
    LocalReachabilityShield,
    StoreLoadForCell, Prefetch.Proto,
    IsComputableAtCompileTime.Proto, ComputeAtCompileTime.Proto,
    CheckCastTrustedDelayed.Proto, AggressiveClinitAnalysisAssert.Proto,
    ThinCheckCast.Proto, ThinInstanceOf.Proto, ThinNullCheck.Proto, ThinNew.Proto,
    NewArrayCopy.Proto, NewArrayCopyRT.Proto, NewArrayRT, ConcealRef, PublishRef, Not,
    Add.Proto, Mul.Proto, Sub.Proto, IDivRemOp.Proto, FDiv.Proto, And.Proto, Or.Proto, Xor.Proto, Neg.Proto, Shift.Proto,
    Phi.Proto, Cmp.Proto, InvokeTarget.Proto, Call.Proto, PutField.Proto, GetField.Proto, PutStatic.Proto, GetStatic.Proto,
    ArrayGet.Proto, ArrayPut.Proto, ThreeCmp.Proto, LoadMemory.Normal.Proto, LoadMemory.Soft.Proto, StoreMemory.Proto,
    UArrayGet.Proto, UArrayPut.Proto, InvokeInterfaceTarget.Proto,
    ArrayFill.Proto, ReinterpretCast.Proto, ValueConvert.Proto, BitFieldExtract.Proto,
    StrConcat.Proto, MulH.Proto, UMulH.Proto, ErrorRTSCall.Proto,
    CAS.Proto, BitCount.Proto, AnyNull,
    Deferred.New.Proto, Deferred.NewArray.Proto, Deferred.InstanceOf.Proto, Deferred.CheckCast.Proto,
    Deferred.ClassObject.Proto, Deferred.FieldOp.Proto, Deferred.UnresolvedInvoke.Proto,
    InstanceDescriptor.Proto, PreparationCheck.Proto, GetFlatThinCheck, GetFlatThin.Proto,
    SynchronizedRegion, BoxedValue.Proto, DivisorCheck.Proto, GetClass, MemAtomic.Proto, NewArrayMimic.Proto,
    EscapeWriteBarrier.Instance.Proto, EscapeWriteBarrier.Static.Proto, VerificationInstanceWriteBarrier, VerificationStaticWriteBarrier,
    BeginLocalUnmovable, EndLocalUnmovable, InstanceDescriptorBy, FieldAddr.Proto, GetElementPtr.Proto,
    CopyStructure.Proto,
    IncHeldLocks, DecHeldLocks,
    ConvertDomain.Proto,
    VirtualMethodAddr.Proto,
    DelayedGet.Proto, DelayedPut.Proto, DelayedInstanceMethodVNum.Proto, DelayedInstanceFieldAddress.Proto, DelayedMethodAddr.Proto,
    AJArrayFill.Proto,
    DebugTextPosBreakpoint, DebugPrologueEndBreakpoint,
    AcquireRawData, ReleaseRawData,
    CFuncWrapperAddr.Proto,
    CheckedOp.Proto,
    BitcodeDeferred.New.Proto, BitcodeDeferred.NewArray.Proto,
    BitcodeDeferred.InstanceOf.Proto, BitcodeDeferred.CheckCast.Proto,
    BitcodeDeferred.InvokeTarget.Proto, BitcodeDeferred.GetField, BitcodeDeferred.PutField,
    TDBarrier.Proto,
    RawEnrich, RawDeprive, ExtractEnrichment,
    PackageInit.Proto, PackageInitCheck.Proto,
    BitSwap.Proto,
    ExecEnvInvalidationPoint,
    ZeroRefs.Proto,
    Deferred.DynamicOrSigPolyInvoke.Proto,
    Deferred.SigPolyInvokeBasic.Proto,
    Deferred.MethodType.Proto,
    Deferred.MethodHandle.Proto,
    UniversalGeneric.ToHolder.Proto, UniversalGeneric.FromHolder.Proto,
    UniversalGeneric.GetField.Proto, UniversalGeneric.GetFieldOHM.Proto, UniversalGeneric.PutField.Proto,
    UniversalGeneric.InvokeConstraintMethod.Target.Proto, UniversalGeneric.InvokeMethodWithGenericContext.Target.Proto,
    UniversalGeneric.CopyResultVST.Proto, UniversalGeneric.OffHeapMemorySlotPointer.Proto, UniversalGeneric.CopyUniversalVariable.Proto, UniversalGeneric.TypeVarIsRef.Proto,
    UniversalGeneric.HolderConst,
    MutFunc.Host, MutFunc.Offset.Proto, MutFunc.Combine.Proto,
    RunTimeTypeInfo.Proto,
    InitStringRecord.Proto,
    LightInterfCastCBC,
    ThisTypeInfo.Proto, ThisTypeInfoBy,
    InvokeVirtualStaticTarget.Proto,
    UniversalGeneric.GetElementPtr.Proto,
    GetFieldSeqRef.Proto,
    GetFieldSeqRefGeneric.Proto,
    GetStaticFieldSeqRef.Proto,
    LoadFieldSeq.Proto,
    LoadFieldSeqGeneric.Proto,
    LoadStaticFieldSeq.Proto,
    StoreFieldSeq.Proto,
    StoreFieldSeqGeneric.Proto,
    StoreStaticFieldSeq.Proto,
    DerivedPtr.Proto,
    DerivedPtr.Local,
    DerivedPtr.Global,
    LoadTypeInfo.Proto,
    LoadTypeInfoGeneric.Proto,
    GenericTypeArg.Proto,
    Box.Proto,
    Unbox.Proto,
    UnboxRec.Proto,
    SpawnFuture.Proto,
    SpawnClosure.Proto,
    OptionTagGeneric.Proto,
    OptionPayloadGeneric.Proto,
    NewNoneOptionGeneric.Proto,
    NewSomeOptionGeneric.Proto,
    SaveCallRefTypeInfo,
    AssignGeneric.Proto,
    InstanceOfGeneric.Proto,
    AtomicOps.Load.Proto,
    AtomicOps.Store.Proto,
    AtomicOps.CAS.Proto,
    AtomicOps.Simple.Proto,
    // Add new prototypes above this line.
    "dummy last element for ease of rebase"
  )

  /**
   * Reserved node identifiers: incoming memory, control and method arguments.
   */
  object ReservedNodeIds {
    val Invalid = 0
    val EntryBlock = 1
    private val ParamIdBase = 2

    def getParamId(paramNum: Int): NodeId = paramNum + ParamIdBase
    def getParamNum(paramNodeId: NodeId): Int = paramNodeId - ParamIdBase

    /**
     * Returns number of reserved node identifiers.
     */
    def getNumber(method: Method): Int = method.getParamsCount + ParamIdBase
  }

  /**
   * Index of `null` inline context.
   */
  protected val NullInlineContextIndex = -1

  /**
    * Index of `null` lexical block.
    */
  protected val NullLexicalBlockIndex = -1

  // In case of an error make sure that your newly added proto is listed in [[Serialization.Serializer.writeProto]]
  private def serializationError[A, B](key: A): B =
    SerializationError(s"key not found: ${key.getClass.getName}")

  private val id2object = mutable.HashMap.empty[Int, AnyRef].withDefault(serializationError)

  private val object2id = {
    assert(isUByte(SupportedProtos.size))

    val res = mutable.HashMap.empty[AnyRef, Int].withDefault(serializationError)

    for ((obj, id) <- SupportedProtos.zipWithIndex) {
      id2object(id) = obj
      res(obj) = id
      assert(isUByte(id))
    }

    res
  }

  class OptWriter(out: DataOutput, contextClass: ClassType) extends BinaryWriter(out, contextClass, env) {

    private def putObj(obj: AnyRef): Unit = { putUByte(object2id(obj)) }

    def proto(p: AnyRef): Unit = {
      putObj(p)
    }

    def tpe(tpe: Type): Unit = {
      tpe match {
        case TRefType => number(0)
        case ThinType => number(1)
        case ConditionType => number(2)
        case VoidType => number(3)
        case tpe: NumericType =>
          number(4)
          tkind(tpe.kind)
        case tpe: RecordAddrType =>
          number(5)
          sigType(tpe.sigType)
        case EopType.Eop(tpe) =>
          number(6)
          symType(tpe)
        case EopType.Null => number(7)
        case EopType.Any => number(8)
        case IntraReferenceType => number(9)
        case HolderType(sig) =>
          number(10)
          sigType(sig)
        case _ => shouldNotReachHere(s"unexpected IR type: $tpe")
      }
    }

    def id(x: NodeId): Unit = {
      unsignedNumber(x)
    }

    def vmstate(t: VMStateApprox): Unit = {
      map(t.states) { (tpe, state) =>
        symType(tpe)
        enumeration(state.kind)
      }
    }

    def reason(reason: Halt.Reason): Unit = {
      number(reason.ordinal)
      reason match {
        case Halt.Reason.Empty =>

        case Halt.Reason.Explained(msg) =>
          xstring(XString(msg))

        case Halt.Reason.AfterRTSCall(proc, msg) =>
          enumeration(proc)
          xstring(XString(msg))

        case Halt.Reason.AfterThrow(msg) =>
          xstring(XString(msg))
      }
    }
  }

  class OptReader(in: DataInput, contextClass: ClassType) extends BinaryReader(in, contextClass, env) {

    private def nextObj() = id2object(nextUByte())

    def proto() = nextObj()

    def tpe(): Type = {
      number() match {
        case 0 => TRefType
        case 1 => ThinType
        case 2 => ConditionType
        case 3 => VoidType
        case 4 => NumericType(tkind())
        case 5 => RecordAddrType(sigType())
        case 6 => EopType.Eop(symType())
        case 7 => EopType.Null
        case 8 => EopType.Any
        case 9 => IntraReferenceType
        case 10 => HolderType(sigType())
        case x => shouldNotReachHere(s"unexpected IR type id: $x")
      }
    }

    def id(): NodeId = {
      unsignedNumber()
    }

    def vmstate(): VMStateApprox = {
      VMStateApprox(map(() => (
        symType(),
        TypeState(enumeration(TypeState.Kind.fromOrdinal))
      )))
    }

    def reason(): Halt.Reason = {
      number() match {
        case 0 => Halt.Reason.Empty
        case 1 => Halt.Reason.Explained(xstring().toString)
        case 2 => Halt.Reason.AfterRTSCall(enumeration(RTSProc.fromOrdinal), xstring().toString)
        case 3 => Halt.Reason.AfterThrow(xstring().toString)
        case x => shouldNotReachHere(s"Unexpected halt generation reason with id $x")
      }
    }
  }

}
