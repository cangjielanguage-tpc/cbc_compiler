/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.frontend.bytecode.intrinsics

import com.huawei.excelsior.jet.compiler.options.BoolOption.{GCSafetyChecks, GenerateWriteBarriers}
import com.huawei.excelsior.jet.compiler.intrinsics.{IntrinsicWithBody, IntrinsicWithoutBody}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.{Method, SignatureType, TypeKind as TKind}
import com.huawei.excelsior.jet.compiler.{PreparationRequired, RTSProc, symlevel}
import com.huawei.excelsior.jet.util.ScalaCollections.singleElement
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.codeemitter.BarrierKind
import BarrierKind.{LOAD_LOAD, LOAD_STORE, STORE_LOAD, STORE_STORE}
import com.huawei.excelsior.jet.assembler.{AsmType, Width}
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind

import scala.PartialFunction.condOpt

/**
 * This class implements AJ intrinsics invocations.
 *
 * @author cypok
 * @author conwor
 * @author paul
 */
trait Intrinsics { self: Universe =>

  import BitFieldExtract._

  private def addressGet(tpe: SignatureType)(args: Seq[Node], atomic: Boolean): Node = args match {
    case Seq(addr) => LoadMemory(tpe.toAsm, tpe, atomic)(addr)
  }

  private def addressPut(tpe: SignatureType)(args: Seq[Node], atomic: Boolean): Node = args match {
    case Seq(addr, value) => StoreMemory(tpe.toAsm, tpe, atomic)(addr, value)
  }

  def loadAcquireImpl(tpe: SignatureType, args: Seq[Node]) = {
    val outValue = addressGet(tpe)(args, atomic = true)
    MemBarrier(Set(LOAD_LOAD, LOAD_STORE))()
    outValue
  }

  def storeReleaseImpl(tpe: SignatureType, args: Seq[Node]) = {
    MemBarrier(Set(LOAD_STORE, STORE_STORE))()
    val n = addressPut(tpe)(args, atomic = true)
    MemBarrier(Set(STORE_STORE, STORE_LOAD))()
    n
  }

  def calcAddr(base: Node, offset: Node): Node = Add(base, Extend(AddrType, AsmType.I32, signExtension = true, offset))

  final def loadIntrinsic(target: Method, itype: IntrinsicWithoutBody, caller: Method, args: Seq[Node]): Node = {
    import IntrinsicWithoutBody._

    def tpe: SignatureType.Primitive = itype match {
      case Address_getAddr | Address_putAddr | SoftExceptions_getAddrUIntFieldWithSoftException => SignatureType.Address
      case _ if itype.getOpType.isPrimitive => SignatureType.Primitive(itype.getOpType)
      case _ => shouldNotReachHere(itype)
    }

    def stackAlloc() =
      args match { case Seq(size: IConst, alignment: IConst) =>
        StackAlloc.raw(size.value, alignment.value)
      }

    def resolveJavaTypeByName(name: ConstString) = {
      typeProvider.resolveJavaTypeByName(caller.getDeclaringClass, name.stringValue.replace('.', '/'))
    }

    def resolveTypeByName(name: ConstString) = {
      typeProvider.resolveTypeByName(caller.getDeclaringClass, name.stringValue.replace('.', '/'))
    }

    def arrayElemOp[T <: Node](array: Node, idx: Node, op: SignatureType => T): T = {
      NullCheck(array)
      val arrayType = SignatureType.fromSymType(typeProvider.getAJArrayType(itype.getOpType))
      ArrayIndexCheck(arrayType, array, idx, AJArrayLength(array))
      op(arrayType)
    }

    itype match {
      case GCPoints_gcPoint => GCPoint()

      case Unchecked_cast | UncheckedJava_cast | UncheckedJava_cast_byName => args match { case Seq(obj, typeName) =>
        CheckCastTrustedDelayed(obj, typeName)
        obj
      }

      case EopUtils_asJavaObject | ScalaEopUtils_asScalaObject | PlainEop_asAJObject => args match { case Seq(eop) =>
        PublishRef(eop)
      }

      case PlainEop_fromAJObject => ConcealRef(args: _*)

      case ManagedObjects_asLockable => args match { case Seq(arg) => arg }

      case Address_getLong  | Address_getInt    | Address_getShort |
           Address_getChar  | Address_getByte   | Address_getBoolean |
           Address_getFloat | Address_getDouble | Address_getAddr =>
        addressGet(tpe)(args, atomic = false)

      case Address_putLong  | Address_putInt    | Address_putShort |
           Address_putChar  | Address_putByte   | Address_putBoolean |
           Address_putFloat | Address_putDouble | Address_putAddr =>
        addressPut(tpe)(args, atomic = false)

      case StackAlloc_rawAlloc => stackAlloc()

      case AJSyntax_bstr => args match { case Seq(cs: ConstString) => AJString.bstr(cs.stringValue) }
      case AJSyntax_ustr => args match { case Seq(cs: ConstString) => AJString.ustr(cs.stringValue) }
      case AJMSyntax_aj  => args match { case Seq(cs) => cs }

      case TypeHandle_of | RunTimeTypeInfo_of | InstanceDescriptor_of |
           RunTimeTypeInfo_raw | InstanceDescriptor_raw |
           JavaTypeHandle_of | JavaRunTimeTypeInfo_of | JavaInstanceDescriptor_of |
           JavaRunTimeTypeInfo_raw | JavaInstanceDescriptor_raw |
           ScalaTypeHandle_of | ScalaRunTimeTypeInfo_of | ScalaInstanceDescriptor_of |
           ScalaRunTimeTypeInfo_raw | ScalaInstanceDescriptor_raw |
           // TODO: delay resolve of Cangjie types
           CangjieTypeHandle_of | CangjieRunTimeTypeInfo_of | CangjieInstanceDescriptor_of |
           CangjieRunTimeTypeInfo_raw | CangjieInstanceDescriptor_raw =>
        args match { case Seq(cs: ConstString) =>
          val clazz = resolveJavaTypeByName(cs)
          assert(!clazz.isDeferred) // TODO: issue error
          itype match {
            case TypeHandle_of => TypeHandle(clazz)
            case RunTimeTypeInfo_raw | JavaRunTimeTypeInfo_raw | ScalaRunTimeTypeInfo_raw | CangjieRunTimeTypeInfo_raw =>
              RunTimeTypeInfo(clazz)
            case InstanceDescriptor_raw => RawInstanceDescriptor(clazz)

            case JavaTypeHandle_of | ScalaTypeHandle_of | CangjieTypeHandle_of => TypeHandle(clazz)
            case JavaInstanceDescriptor_raw | ScalaInstanceDescriptor_raw | CangjieInstanceDescriptor_raw => RawInstanceDescriptor(clazz)

            case RunTimeTypeInfo_of | JavaRunTimeTypeInfo_of | ScalaRunTimeTypeInfo_of | CangjieRunTimeTypeInfo_of =>
              ensurePrepared(PreparationRequired.forType(clazz))
              RunTimeTypeInfo(clazz)

            case InstanceDescriptor_of | JavaInstanceDescriptor_of | ScalaInstanceDescriptor_of | CangjieInstanceDescriptor_of =>
              ensurePrepared(PreparationRequired.forType(clazz))
              InstanceDescriptor(clazz)()

            case _ => shouldNotReachHere()
          }
        }

      case GCPoints_genTrapCheckOnAddress => TrapCheck(args: _*)

      case ManagedExecEnv_getManagedExecEnv | CJNativeThreadLocal_get => ExecEnv()
      case StackDescriptor_getCurrentStackDescriptor => StackDescriptor()
      case FrameHeader_getCurrentFrameHeader => FrameHeader()

      case CodeAddrIntrinsics_getMethodAddr =>
        args match { case Seq(className: ConstString, methodName: ConstString, methodSig: ConstString) =>
          val clazz = resolveTypeByName(className)
          assert(clazz.isClassOrInterface)
          assert(!clazz.isDeferred)

          val sig = env.getTypeProvider.parseMethodSignature(methodSig.stringValue)
          val target = clazz.findDeclaredMethod(methodName.stringValue, sig)
          ensurePrepared(PreparationRequired.forMethodAddr(target))
          SymbolAddress(target)
        }

      case DelayedIntrinsics_getMethodVNum =>
        args match {
          case Seq(className: ConstString, methodName: ConstString, methodSig: ConstString) =>
            DelayedInstanceMethodVNum(className.stringValue, methodName.stringValue, methodSig.stringValue)()
        }

      case DelayedIntrinsics_getFieldAddr =>
        args match {
          case Seq(rcv: Node, className: ConstString, fieldName: ConstString, fieldSig: ConstString) =>
            DelayedInstanceFieldAddress(className.stringValue, fieldName.stringValue, fieldSig.stringValue)(rcv)
        }

      case DelayedIntrinsics_getMethodAddr =>
        args match {
          case Seq(className: ConstString, methodName: ConstString) =>
            DelayedMethodAddr(className.stringValue, methodName.stringValue)()
        }

      case MethodInfoFrameDescriptor_of =>
        assert(rootMethod.isNeverInline && rootMethod.isMethodInfoFrameDescriptorGetter)

        args match { case Seq(className: ConstString, methodName: ConstString, methodSig: ConstString) =>
          val clazz = resolveTypeByName(className)
          assert(clazz.isClassOrInterface)
          assert(!clazz.isDeferred)
          assert(clazz == caller.getDeclaringClass)

          val sig = env.getTypeProvider.parseMethodSignature(methodSig.stringValue)
          val method = clazz.findDeclaredMethod(methodName.stringValue, sig)
          ensurePrepared(PreparationRequired.forType(clazz))
          SymbolAddress(method.getFrameDescriptor)
        }

      case CompiledLayoutInfo_size =>
        args match { case Seq(className: ConstString) => IConst(resolveTypeByName(className).getRawObjectSize) }

      case VarArgsBuilder_start => VarArgsList.start()

      case VarArgsBuilder_done =>
        args match { case Seq(builder: VarArgsList.Builder) => builder.done() }

      case VarArgsBuilder_arg_B | VarArgsBuilder_arg_C | VarArgsBuilder_arg_D | VarArgsBuilder_arg_F |
           VarArgsBuilder_arg_I | VarArgsBuilder_arg_J | VarArgsBuilder_arg_S |
           VarArgsBuilder_arg_Z =>
        args match { case Seq(value: Node, builder: VarArgsList.Builder) =>
          val kind = itype.getOpType

          val (convertedValue, convertedArgTKind) =
            if (kind.isShortIntegral) { // integers should be truncated
              (JavaShortIntegralExtend(SignatureType.Primitive(kind).toAsm, value), kind)
            } else if (kind == BytecodeTypeKind.FLOAT) { // but floats should be extended
              (ValueConvert(AsmType.F32, AsmType.F64)(value), BytecodeTypeKind.DOUBLE)
            } else {
              (value, kind)
            }

          builder.addVarArg(convertedValue, SignatureType.Primitive(convertedArgTKind))
        }

      case CVarArgs_getVarArgs | JETVarArgs_getVarArgs =>
        assert(rootMethod.isVarArgs)
        VarArguments()

      case GCSafetyValidatorIntrinsics_enterGCSafetyChecksFreeSection =>
        assert(env.enabled(GCSafetyChecks))
        RTSCall(RTSProc.JR_EnterGCSafetyChecksFreeSection)()

      case GCSafetyValidatorIntrinsics_leaveGCSafetyChecksFreeSection =>
        assert(env.enabled(GCSafetyChecks))
        RTSCall(RTSProc.JR_LeaveGCSafetyChecksFreeSection)()


      case ThinTypeInternals_thinAlloc => args match { case Seq(cs: ConstString) =>
        ReinterpretCast(ThinType, AddrType)(StackAlloc.Local(SignatureType.fromSymType(resolveTypeByName(cs))))
      }

      case ThinTypeInternals_newThin => args match {
        case Seq(addr: Node, typeName: ConstString) =>
          val clazz = resolveTypeByName(typeName)
          assert(clazz.isThinClass)
          ensurePrepared(PreparationRequired.forThinNewOp(clazz))
          val thin = ReinterpretCast(AddrType, ThinType)(addr)
          ThinNullCheck(thin)
          ThinNew(clazz)(thin)
          thin
      }
      case ThinTypeInternals_thinAsAddress => ReinterpretCast(ThinType, AddrType)(singleElement(args))
      case ThinTypeInternals_addressAsThin => ReinterpretCast(AddrType, ThinType)(singleElement(args))

      case ThinTypeInternals_requireNonNull => args match {
        case Seq(arg) =>
          ThinNullCheck(arg)
          arg
      }

      case ManagedTypeInternals_requireNonNull => args match {
        case Seq(arg) =>
          NullCheck(arg)
          arg
      }

      case ManagedArrayIntrinsic_allocRefArray | ManagedArrayIntrinsic_allocByteArray |
           ManagedArrayIntrinsic_allocBooleanArray | ManagedArrayIntrinsic_allocCharArray |
           ManagedArrayIntrinsic_allocShortArray | ManagedArrayIntrinsic_allocIntArray |
           ManagedArrayIntrinsic_allocLongArray | ManagedArrayIntrinsic_allocFloatArray |
           ManagedArrayIntrinsic_allocDoubleArray =>
        args match { case Seq(len) =>
          val arrayType = typeProvider.getAJArrayType(itype.getOpType)
          ensurePrepared(PreparationRequired.forType(arrayType))
          NewArray(SignatureType.fromSymType(arrayType))(len)
        }

      case AJRefArray_get | AJByteArray_get | AJBooleanArray_get |
           AJCharArray_get | AJShortArray_get | AJIntArray_get |
           AJLongArray_get | AJFloatArray_get | AJDoubleArray_get =>
        args match { case Seq(array, idx) =>
          arrayElemOp(array, idx, ArrayGet(_)(array, idx))
        }

      case AJRefArray_set | AJByteArray_set | AJBooleanArray_set |
           AJCharArray_set | AJShortArray_set | AJIntArray_set |
           AJLongArray_set | AJFloatArray_set | AJDoubleArray_set =>
        args match { case Seq(array, idx, value) =>
          arrayElemOp(array, idx, arrayType => {
            if (env.enabled(GenerateWriteBarriers) && arrayType.getArrayElemType.isTraceableReference) {
              if (currentInlineContext.method.isManaged) {
                WriteBarrier.instance(array, value)
              } else {
                VerificationInstanceWriteBarrier(array, value)
              }
            }
            ArrayPut(arrayType)(array, idx, value)
          })
        }

      case AJRefArray_length | AJByteArray_length | AJBooleanArray_length |
           AJCharArray_length | AJShortArray_length | AJIntArray_length |
           AJLongArray_length | AJFloatArray_length | AJDoubleArray_length =>
        args match { case Seq(array) =>
          NullCheck(array)
          AJArrayLength(array)
        }

      case SoftExceptions_getIntFieldWithSoftException | SoftExceptions_getAddrUIntFieldWithSoftException =>
        args match { case Seq(obj, offset, IConst(kind)) =>
            LoadMemory.soft(tpe.toAsm, tpe, kind)(calcAddr(ConcealRef(obj), offset))
        }

      case FieldUpdaterIntrinsics_fieldOffset => {
        args match { case Seq(className: ConstString, fieldName: ConstString) =>
          val clazz = typeProvider.resolveTypeByName(caller.getDeclaringClass, className.stringValue.replace('.', '/'))
          val field = clazz.findField(fieldName.stringValue)
          IConst(field.getInstanceFieldOffset)
        }
      }

      case CheckedMath_checkedAdd8  => CheckedOp(IntType,  Width.W8,  CheckedOp.Kind.ADD, signed = true, caller.isManaged)(args: _*)
      case CheckedMath_checkedAdd16 => CheckedOp(IntType,  Width.W16, CheckedOp.Kind.ADD, signed = true, caller.isManaged)(args: _*)
      case CheckedMath_checkedAdd32 => CheckedOp(IntType,  Width.W32, CheckedOp.Kind.ADD, signed = true, caller.isManaged)(args: _*)
      case CheckedMath_checkedAdd64 => CheckedOp(LongType, Width.W64, CheckedOp.Kind.ADD, signed = true, caller.isManaged)(args: _*)
      case CheckedMath_checkedSub8  => CheckedOp(IntType,  Width.W8,  CheckedOp.Kind.SUB, signed = true, caller.isManaged)(args: _*)
      case CheckedMath_checkedSub16 => CheckedOp(IntType,  Width.W16, CheckedOp.Kind.SUB, signed = true, caller.isManaged)(args: _*)
      case CheckedMath_checkedSub32 => CheckedOp(IntType,  Width.W32, CheckedOp.Kind.SUB, signed = true, caller.isManaged)(args: _*)
      case CheckedMath_checkedSub64 => CheckedOp(LongType, Width.W64, CheckedOp.Kind.SUB, signed = true, caller.isManaged)(args: _*)
      case CheckedMath_checkedMul8  => CheckedOp(IntType,  Width.W8,  CheckedOp.Kind.MUL, signed = true, caller.isManaged)(args: _*)
      case CheckedMath_checkedMul16 => CheckedOp(IntType,  Width.W16, CheckedOp.Kind.MUL, signed = true, caller.isManaged)(args: _*)
      case CheckedMath_checkedMul32 => CheckedOp(IntType,  Width.W32, CheckedOp.Kind.MUL, signed = true, caller.isManaged)(args: _*)
      case CheckedMath_checkedMul64 => CheckedOp(LongType, Width.W64, CheckedOp.Kind.MUL, signed = true, caller.isManaged)(args: _*)
      case CheckedMath_checkedDiv8  => CheckedOp(IntType,  Width.W8,  CheckedOp.Kind.DIV, signed = true, caller.isManaged)(args: _*)
      case CheckedMath_checkedDiv16 => CheckedOp(IntType,  Width.W16, CheckedOp.Kind.DIV, signed = true, caller.isManaged)(args: _*)
      case CheckedMath_checkedDiv32 => CheckedOp(IntType,  Width.W32, CheckedOp.Kind.DIV, signed = true, caller.isManaged)(args: _*)
      case CheckedMath_checkedDiv64 => CheckedOp(LongType, Width.W64, CheckedOp.Kind.DIV, signed = true, caller.isManaged)(args: _*)

      case CheckedMath_checkedUAdd8  => CheckedOp(IntType,  Width.W8,  CheckedOp.Kind.ADD, signed = false, caller.isManaged)(args: _*)
      case CheckedMath_checkedUAdd16 => CheckedOp(IntType,  Width.W16, CheckedOp.Kind.ADD, signed = false, caller.isManaged)(args: _*)
      case CheckedMath_checkedUAdd32 => CheckedOp(IntType,  Width.W32, CheckedOp.Kind.ADD, signed = false, caller.isManaged)(args: _*)
      case CheckedMath_checkedUAdd64 => CheckedOp(LongType, Width.W64, CheckedOp.Kind.ADD, signed = false, caller.isManaged)(args: _*)
      case CheckedMath_checkedUSub8  => CheckedOp(IntType,  Width.W8,  CheckedOp.Kind.SUB, signed = false, caller.isManaged)(args: _*)
      case CheckedMath_checkedUSub16 => CheckedOp(IntType,  Width.W16, CheckedOp.Kind.SUB, signed = false, caller.isManaged)(args: _*)
      case CheckedMath_checkedUSub32 => CheckedOp(IntType,  Width.W32, CheckedOp.Kind.SUB, signed = false, caller.isManaged)(args: _*)
      case CheckedMath_checkedUSub64 => CheckedOp(LongType, Width.W64, CheckedOp.Kind.SUB, signed = false, caller.isManaged)(args: _*)
      case CheckedMath_checkedUMul8  => CheckedOp(IntType,  Width.W8,  CheckedOp.Kind.MUL, signed = false, caller.isManaged)(args: _*)
      case CheckedMath_checkedUMul16 => CheckedOp(IntType,  Width.W16, CheckedOp.Kind.MUL, signed = false, caller.isManaged)(args: _*)
      case CheckedMath_checkedUMul32 => CheckedOp(IntType,  Width.W32, CheckedOp.Kind.MUL, signed = false, caller.isManaged)(args: _*)
      case CheckedMath_checkedUMul64 => CheckedOp(LongType, Width.W64, CheckedOp.Kind.MUL, signed = false, caller.isManaged)(args: _*)

      case CriticalSection_incHeldLocks => IncHeldLocks()
      case CriticalSection_decHeldLocks => DecHeldLocks()

      case ManagedEop_constrPlainImpl =>
        args match { case Seq(obj) => obj }

      case ManagedEop_copyEnrichment =>
        args match { case Seq(original, enriched) =>
          RawEnrich(original, ExtractEnrichment(enriched))
        }

      case ManagedEop_extractValue =>
        args match { case Seq(arg) => RawDeprive(arg) }

      case InvalidateExecEnv => ExecEnvInvalidationPoint()

      case _ =>
        target match {
          case Java.Lang.MathIntrinsic(kind) =>
            MathIntrinsic(kind)(args: _*)

          case _ =>
            notImplemented("intrinsic", itype)
        }
    }
  }

  def loadIntrinsicWithBody(target: Method, itype: IntrinsicWithBody, caller: Method, args: Seq[Node]): Option[Node] = {
    import IntrinsicWithBody._

    def tpe: SignatureType.Primitive = itype match {
      case Address_loadAcquireAddr | Address_storeReleaseAddr | ArrayAccessHelper_setStruct => SignatureType.Address
      case _ if itype.getOpType.isPrimitive => SignatureType.Primitive(itype.getOpType)
      case _ => shouldNotReachHere(itype)
    }

    def memBar(kind: BarrierKind) = { assert(args.isEmpty); MemBarrier(Set(kind))() }

    def fieldGet(args: Seq[Node], atomic: Boolean): Node = args match {
      case Seq(base, offset) => LoadMemory(tpe.toAsm, tpe, atomic)(calcAddr(base, offset))
    }

    def fieldSet(args: Seq[Node], atomic: Boolean): Node = args match {
      case Seq(base, offset, value) => StoreMemory(tpe.toAsm, tpe, atomic)(calcAddr(base, offset), value); value
    }

    import MemAtomic.Kind._
    condOpt(itype) {
      case MemoryHints_prefetchForWrite => Prefetch(forWrite = true)(args: _*)

      case JavaCallStackUtils_getCallerClassHandle => args match { case Seq(depth) => AJCallerClass(currentInlineContext)(depth) }

      case MemoryBarrier_L_L   => memBar(LOAD_LOAD)
      case MemoryBarrier_L_S   => memBar(LOAD_STORE)
      case MemoryBarrier_S_L   => memBar(STORE_LOAD)
      case MemoryBarrier_S_S   => memBar(STORE_STORE)

      case Address_loadAcquireBoolean | Address_loadAcquireByte   | Address_loadAcquireShort |
           Address_loadAcquireChar    | Address_loadAcquireInt    | Address_loadAcquireLong  |
           Address_loadAcquireFloat   | Address_loadAcquireDouble | Address_loadAcquireAddr  =>
        loadAcquireImpl(tpe, args)

      case Address_storeReleaseByte  | Address_storeReleaseBoolean | Address_storeReleaseShort |
           Address_storeReleaseChar  | Address_storeReleaseInt     | Address_storeReleaseLong  |
           Address_storeReleaseFloat | Address_storeReleaseDouble  | Address_storeReleaseAddr  =>
        storeReleaseImpl(tpe, args)

      case FieldAccessHelper_getLong  | FieldAccessHelper_getInt  | FieldAccessHelper_getShort |
           FieldAccessHelper_getChar  | FieldAccessHelper_getByte | FieldAccessHelper_getBoolean |
           FieldAccessHelper_getFloat | FieldAccessHelper_getDouble =>
        fieldGet(args, atomic = false)

      case FieldAccessHelper_setLong  | FieldAccessHelper_setInt  | FieldAccessHelper_setShort |
           FieldAccessHelper_setChar  | FieldAccessHelper_setByte | FieldAccessHelper_setBoolean |
           FieldAccessHelper_setFloat | FieldAccessHelper_setDouble =>
        fieldSet(args, atomic = false)

      case ArrayAccessHelper_getLong  | ArrayAccessHelper_getInt  | ArrayAccessHelper_getShort |
           ArrayAccessHelper_getChar  | ArrayAccessHelper_getByte | ArrayAccessHelper_getBoolean |
           ArrayAccessHelper_getFloat | ArrayAccessHelper_getDouble =>
        UArrayGet(tpe.toAsm)(args: _*)

      case ArrayAccessHelper_setLong  | ArrayAccessHelper_setInt    | ArrayAccessHelper_setShort |
           ArrayAccessHelper_setChar  | ArrayAccessHelper_setByte   | ArrayAccessHelper_setBoolean |
           ArrayAccessHelper_setFloat | ArrayAccessHelper_setDouble | ArrayAccessHelper_setStruct =>
        UArrayPut(tpe.toAsm)(args: _*)

      case FieldAccessHelper_getFlat =>
        args match { case Seq(base, offset) => calcAddr(base, offset) }

      case ArrayAccessHelper_getFlat =>
        args match { case Seq(base, elemSize, index) =>
          Add(base, Mul(Extend(AddrType, AsmType.I32, signExtension = false, index), Extend(AddrType, AsmType.I32, signExtension = false, elemSize)))
        }

      case ComputableAtCompileTime_isNull |
           ComputableAtCompileTime_hasTraceableFields |
           ComputableAtCompileTime_isArray |
           ComputableAtCompileTime_getReferenceArrayElementFormalTypeName |
           ComputableAtCompileTime_getComponentType |
           ComputableAtCompileTime_getArrayDimNum |
           ComputableAtCompileTime_getArrayElemLog2Size |
           ComputableAtCompileTime_hasPrimitiveArrayBaseType |
           ComputableAtCompileTime_isArrayClass |
           ComputableAtCompileTime_isInstance |
           ComputableAtCompileTime_isAssignable =>

        import CompileTimeOp.Kind._
        val kind = itype match {
          case ComputableAtCompileTime_isNull => IsNull
          case ComputableAtCompileTime_hasTraceableFields => HasTraceableFields
          case ComputableAtCompileTime_isArray => IsArray
          case ComputableAtCompileTime_getReferenceArrayElementFormalTypeName => GetReferenceArrayElementFormalType
          case ComputableAtCompileTime_getComponentType => GetComponentType
          case ComputableAtCompileTime_getArrayDimNum => GetArrayDimNum
          case ComputableAtCompileTime_getArrayElemLog2Size => GetArrayElemLog2Size
          case ComputableAtCompileTime_hasPrimitiveArrayBaseType => HasPrimitiveArrayBaseType
          case ComputableAtCompileTime_isArrayClass => IsArrayClass
          case ComputableAtCompileTime_isInstance => IsInstance
          case ComputableAtCompileTime_isAssignable => IsAssignable

          case _ => shouldNotReachHere()
        }
        CondVal(IsComputableAtCompileTime(kind)(args: _*))

      case CompileTime_isArray |
           CompileTime_hasPrimitiveArrayBaseType |
           CompileTime_isNull |
           CompileTime_hasTraceableFields |
           CompileTime_isArrayClass |
           CompileTime_isInstance |
           CompileTime_isAssignable =>

        import CompileTimeOp.Kind._
        val kind = itype match {
          case CompileTime_isNull => IsNull
          case CompileTime_hasTraceableFields => HasTraceableFields
          case CompileTime_isArray => IsArray
          case CompileTime_hasPrimitiveArrayBaseType => HasPrimitiveArrayBaseType
          case CompileTime_isArrayClass => IsArrayClass
          case CompileTime_isInstance => IsInstance
          case CompileTime_isAssignable => IsAssignable

          case _ => shouldNotReachHere()
        }
        CondVal(ComputeAtCompileTime(kind)(args: _*))

      case CompileTime_getReferenceArrayElementFormalTypeName |
           CompileTime_getComponentType |
           CompileTime_getArrayDimNum |
           CompileTime_getArrayElemLog2Size =>

        import CompileTimeOp.Kind._
        val kind = itype match {
          case CompileTime_getReferenceArrayElementFormalTypeName => GetReferenceArrayElementFormalType
          case CompileTime_getComponentType => GetComponentType
          case CompileTime_getArrayDimNum => GetArrayDimNum
          case CompileTime_getArrayElemLog2Size => GetArrayElemLog2Size

          case _ => shouldNotReachHere()
        }
        ComputeAtCompileTime(kind)(args: _*)

      case AllocArray_newArray => args match { case Seq(klass, length) => NewArrayRT(klass, length) }

      case UInt32_div => DivisorCheck(trusted = true)(args(1)); UDiv(IntType)(args: _*)
      case UInt32_mod => DivisorCheck(trusted = true)(args(1)); URem(IntType)(args: _*)
      case UInt32_leq => CondVal(Cmp(IntType, Condition.ULE)(args: _*))
      case UInt32_lss => CondVal(Cmp(IntType, Condition.ULT)(args: _*))

      case MemAtomic_casByte   => CAS(AsmType.I8)(args: _*)
      case MemAtomic_casShort  => CAS(AsmType.I16)(args: _*)
      case MemAtomic_casInt    => CAS(AsmType.I32)(args: _*)

      case MemAtomic_addByte    => MemAtomic(ADD,   AsmType.I8)(args: _*)
      case MemAtomic_andByte    => MemAtomic(AND,   AsmType.I8)(args: _*)
      case MemAtomic_orByte     => MemAtomic(OR,    AsmType.I8)(args: _*)
      case MemAtomic_xorByte    => MemAtomic(XOR,   AsmType.I8)(args: _*)
      case MemAtomic_swapByte   => MemAtomic(SWAP,  AsmType.I8)(args: _*)

      case MemAtomic_addShort    => MemAtomic(ADD,   AsmType.I16)(args: _*)
      case MemAtomic_andShort    => MemAtomic(AND,   AsmType.I16)(args: _*)
      case MemAtomic_orShort     => MemAtomic(OR,    AsmType.I16)(args: _*)
      case MemAtomic_xorShort    => MemAtomic(XOR,   AsmType.I16)(args: _*)
      case MemAtomic_swapShort   => MemAtomic(SWAP,  AsmType.I16)(args: _*)

      case MemAtomic_addInt    => MemAtomic(ADD,   AsmType.I32)(args: _*)
      case MemAtomic_andInt    => MemAtomic(AND,   AsmType.I32)(args: _*)
      case MemAtomic_orInt     => MemAtomic(OR,    AsmType.I32)(args: _*)
      case MemAtomic_xorInt    => MemAtomic(XOR,   AsmType.I32)(args: _*)
      case MemAtomic_swapInt   => MemAtomic(SWAP,  AsmType.I32)(args: _*)

      case UnmanagedMath_numberOfLeadingZeros_I => BitCount.leadingZeros(IntType, singleElement(args))
      case UnmanagedMath_numberOfLeadingZeros_L => BitCount.leadingZeros(LongType, singleElement(args))
      case UnmanagedMath_numberOfTrailingZeros_I => BitCount.trailingZeros(IntType, singleElement(args))
      case UnmanagedMath_numberOfTrailingZeros_L => BitCount.trailingZeros(LongType, singleElement(args))

      case UnmanagedMath_bitCount_I => BitCount.bitCount(IntType, singleElement(args))
      case UnmanagedMath_bitCount_L => BitCount.bitCount(LongType, singleElement(args))

      case UnmanagedMath_reinterpretCast_F2I => ReinterpretCast(FloatType, IntType)(singleElement(args))
      case UnmanagedMath_reinterpretCast_I2F => ReinterpretCast(IntType, FloatType)(singleElement(args))
      case UnmanagedMath_reinterpretCast_D2L => ReinterpretCast(DoubleType, LongType)(singleElement(args))
      case UnmanagedMath_reinterpretCast_L2D => ReinterpretCast(LongType, DoubleType)(singleElement(args))

      case UnmanagedMath_mulh32  => args match { case Seq(l, r) => MulH(l, r) }
      case UnmanagedMath_umulh32 => args match { case Seq(l, r) => UMulH(l, r) }
      case UnmanagedMath_mulh64  => args match { case Seq(l, r) => MulH(l, r) }
      case UnmanagedMath_umulh64 => args match { case Seq(l, r) => UMulH(l, r) }

      case CompilerHintIntrinsics_coldCode => ColdCodeMarker(); NoValue()
      case CompilerHintIntrinsics_hotCode  => NoValue() // TODO: add hot code property

      case GCBarriers_localReachabilityShield => LocalReachabilityShield(args: _*)
      case GCBarriers_beginLocalUnmovable => BeginLocalUnmovable(args: _*)
      case GCBarriers_endLocalUnmovable => EndLocalUnmovable(args: _*)

      case EscapeWriteBarriers_instanceWriteBarrier =>
        args match { case Seq(r, v) => EscapeWriteBarrier.Instance(r, v) }
      case EscapeWriteBarriers_staticWriteBarrier   =>
        args match { case Seq(v)    => EscapeWriteBarrier.Static(v) }

    } orElse {
      condOpt(target) {
        case Java.Lang.MathIntrinsic(kind) => MathIntrinsic(kind)(args: _*)
      }
    }
  }

}
