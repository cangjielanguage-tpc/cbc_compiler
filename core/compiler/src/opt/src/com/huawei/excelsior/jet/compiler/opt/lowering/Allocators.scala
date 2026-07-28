/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.lowering

import com.huawei.excelsior.jet.compiler.options.BoolOption.IdescHigh16BitsCleaning
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.sync.SyncParamsAnalysis
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.{Domain, Env, RTConst, RTSProc}
import com.huawei.excelsior.jet.compiler.symlevel.{Member, Method, SignatureType, ClassType as SymClassType, Type as SymType}
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.*
import xscala.util.MathUtils

import scala.annotation.tailrec

/**
 * Lowering of New/NewArray/NewString operations.
 *
 * @author alexm
 * @author paul
 */
private[lowering] trait Allocators extends Toolbox with SyncParamsAnalysis { this: Universe =>

  private[lowering] def lowerNew(newOp: New): Node = {
    val klass = asClassType(newOp.allocType)
    assert(!klass.isDeferred)
    assert(klass.isClass)
    val desc = RawInstanceDescriptor(klass)

    val obj = if (newOp.shouldBeInlined) {
      val proc = newOp.inlinedAllocator
      val tsWord = nonLargeClassTSWord(klass, inHeap = true)
      val noNeedForClinitValue = 1 // TODO: remove this parameter?
      val isLockable = if (klass.isAJLockable) 1 else 0

      inlinedCall(proc)(desc, IConst(tsWord), IConst(noNeedForClinitValue), IConst(isLockable))

    } else {
      val size = klass.getHeapObjectSize
      if (!klass.finalizable && size <= RTConst.Allocator.MAX_SIZE_OF_SPECIALIZED_OBJECT.intValue) {
        assert(8  <= RTConst.Allocator.MIN_SIZE_OF_SPECIALIZED_OBJECT.intValue)
        assert(96 == RTConst.Allocator.MAX_SIZE_OF_SPECIALIZED_OBJECT.intValue)
        val proc = size match {
          case 8  => RTSProc.JR_NEW8
          case 16 => RTSProc.JR_NEW16
          case 24 => RTSProc.JR_NEW24
          case 32 => RTSProc.JR_NEW32
          case 40 => RTSProc.JR_NEW40
          case 48 => RTSProc.JR_NEW48
          case 56 => RTSProc.JR_NEW56
          case 64 => RTSProc.JR_NEW64
          case 72 => RTSProc.JR_NEW72
          case 80 => RTSProc.JR_NEW80
          case 88 => RTSProc.JR_NEW88
          case 96 => RTSProc.JR_NEW96
          case _ => shouldNotReachHere(size)
        }
        RTSCall(proc)(desc)

      } else {
        RTSCall(RTSProc.JR_NEW)(desc)
      }
    }

    obj
  }

  private[lowering] def lowerNewString(newString: NewString): Node = {
    def genericNewString() = {
      RTSCall(RTSProc.JR_NEW_STRING)(IConst(0), newString.length)
    }

    val MaxSpecializedStringLen = 144

    newString.length match {
      case IConst(len) =>
        if (len > MaxSpecializedStringLen) {
          genericNewString()

        } else {
          def specializedBySize(rtsProc: RTSProc) = {
            val valueTags = IConst(nonLargeArrayTSWord(typeProvider.get1DimArrayType(CHAR), len, inHeap = true))
            RTSCall(rtsProc)(valueTags, newString.length)
          }

          val keyObjectSize = RTConst.JavaString.SIZE.intValue + typeProvider.get1DimArrayType(CHAR).getArrayObjectSize(len, true)
          keyObjectSize match {
            case 40 =>  specializedBySize(RTSProc.JR_NEW_STRING40)
            case 48 =>  specializedBySize(RTSProc.JR_NEW_STRING48)
            case 56 =>  specializedBySize(RTSProc.JR_NEW_STRING56)
            case 64 =>  specializedBySize(RTSProc.JR_NEW_STRING64)
            case 72 =>  specializedBySize(RTSProc.JR_NEW_STRING72)
            case 80 =>  specializedBySize(RTSProc.JR_NEW_STRING80)
            case 88 =>  specializedBySize(RTSProc.JR_NEW_STRING88)
            case 96 =>  specializedBySize(RTSProc.JR_NEW_STRING96)
            case 104 => specializedBySize(RTSProc.JR_NEW_STRING104)
            case 112 => specializedBySize(RTSProc.JR_NEW_STRING112)
            case 120 => specializedBySize(RTSProc.JR_NEW_STRING120)
            case 128 => specializedBySize(RTSProc.JR_NEW_STRING128)
            case 136 => specializedBySize(RTSProc.JR_NEW_STRING136)
            case 144 => specializedBySize(RTSProc.JR_NEW_STRING144)
            case _   => genericNewString()
          }
        }

      case _ => genericNewString()
    }
  }

  private[lowering] def lowerNewArray(newArray: NewArray): Node = {
    val arrayType = newArray.allocType.symType
    assert (arrayType.isArray)
    val arrayDesc = RawInstanceDescriptor(arrayType)

    if (arrayType.isCangjieArray) {
      assert(newArray.lengths.size == 1)
      val length = newArray.lengths(0)
      return genNewCangjieArray(arrayType, arrayDesc, length, newArray.uninitialized)
    }

    if (arrayType.isAJArray) {
      assert(newArray.lengths.size == 1)
      val length = newArray.lengths(0)
      return genNewAJArray(arrayType, arrayDesc, length)
    }

    if (arrayType.isXScalaArray) { // TODO: implement size-specialized allocators
      assert(newArray.lengths.size == 1)
      val length = newArray.lengths(0)
      val inlineContext = newArray.inlineContext
      assert(inlineContext != null, "inlineContext must not be null")
      val method = inlineContext.method

      val elemType = arrayType.getArrayElemType.symType
      if (elemType.isPrimitive) {
        val log2size = IConst(elemType.log2Size)
        return RTSCall(RTSProc.JR_NEW_SCALA_PRIMARRAY)(log2size, arrayDesc, length)
      } else {
        return RTSCall(RTSProc.JR_NEW_SCALA_REFARRAY)(/* dummy */ IConst(0), arrayDesc, length)
      }
    }

    assert(arrayType.isJBCArray)
    val dimNum = arrayType.getArrayDimnum
    val dimSpec = newArray.lengths.size
    assert ((dimNum >= dimSpec) && (dimSpec >= 1))

    val baseType = arrayType.getArrayBase
    assert(!baseType.isJavaArray)
    assert(!baseType.isDeferred)

    if (dimSpec == 1) {
      val length = newArray.lengths(0)

      guardedInlinedAllocator(newArray, length) { unit =>
        inlinedCall(newArray.inlinedAllocator)(arrayDesc, length, LConst(unit))
      } {
        def genericNewArray() = {
          if (baseType.isPrimitive && dimNum == 1) {
            val log2size = IConst(baseType.log2Size)
            RTSCall(RTSProc.JR_NEW_PRIMARRAY)(log2size, arrayDesc, length)
          } else {
            RTSCall(RTSProc.JR_NEW_REFARRAY)(/* dummy */ IConst(0), arrayDesc, length)
          }
        }

        val MaxSpecializedArrayLen = 112

        length match {
          case IConst(len) =>
            if (len <= MaxSpecializedArrayLen) {
              def specializedBySize(rtsProc: RTSProc) = {
                val objTags = IConst(nonLargeArrayTSWord(arrayType, len, inHeap = true))
                RTSCall(rtsProc)(objTags, arrayDesc, length)
              }

              arrayType.getArrayObjectSize(len, true) match {
                case 16 => specializedBySize(RTSProc.JR_NEW_ARRAY16)
                case 24 => specializedBySize(RTSProc.JR_NEW_ARRAY24)
                case 32 => specializedBySize(RTSProc.JR_NEW_ARRAY32)
                case 40 => specializedBySize(RTSProc.JR_NEW_ARRAY40)
                case 48 => specializedBySize(RTSProc.JR_NEW_ARRAY48)
                case 56 => specializedBySize(RTSProc.JR_NEW_ARRAY56)
                case 64 => specializedBySize(RTSProc.JR_NEW_ARRAY64)
                case 72 => specializedBySize(RTSProc.JR_NEW_ARRAY72)
                case 80 => specializedBySize(RTSProc.JR_NEW_ARRAY80)
                case 88 => specializedBySize(RTSProc.JR_NEW_ARRAY88)
                case 96 => specializedBySize(RTSProc.JR_NEW_ARRAY96)
                case 104 => specializedBySize(RTSProc.JR_NEW_ARRAY104)
                case 112 => specializedBySize(RTSProc.JR_NEW_ARRAY112)
                case _ => genericNewArray()
              }
            } else {
              genericNewArray()
            }

          case _ => genericNewArray()
        }
      }

    } else { // dimSpec != 1
      val dimLengths = stackAllocArrayOfInts(newArray.lengths)
      RTSCall(RTSProc.JR_NEW_ARRAY_MD)(arrayDesc, IConst(dimSpec), dimLengths)
    }
  }

  private def genNewCangjieArray(arrayType: SymType, arrayDesc: Node, length: Node, uninitialized: Boolean): Node = {
    val elemType = arrayType.getArrayElemType.symType
    if (arrayType.getArrayElemType.isZST) {
      RTSCall(RTSProc.JR_NEW_CJ_ARRAY)(arrayDesc, length)
    } else if (uninitialized) {
      RTSCall(RTSProc.JR_NEW_CJ_ARRAY_NO_INIT)(arrayDesc, length)
    } else if (elemType.isPrimitive) {
      val log2size = IConst(elemType.log2Size)
      RTSCall(RTSProc.JR_NEW_CJ_PRIMARRAY)(log2size, arrayDesc, length, LConst(0))
    } else if (elemType.isRecord) {
      val size = elemType.getRawObjectSize
      if (size > 0 && MathUtils.isPowerOf2(size) && size <= RTConst.SmallCangjieAllocator.MAX_DISPATCHED_ARRAY_SIZE.intValue) {
        // elemType.log2size works correctly only for primitives, so we should recalculate it here
        val log2size = IConst(MathUtils.log2(size))
        if (elemType.hasNoRefFields) {
          RTSCall(RTSProc.JR_NEW_CJ_RECORD_ARRAY_WITHOUT_REFS)(log2size, arrayDesc, length, LConst(0))
        } else {
          RTSCall(RTSProc.JR_NEW_CJ_RECORD_ARRAY_WITH_REFS)(log2size, arrayDesc, length, LConst(0))
        }
      } else {
        RTSCall(RTSProc.JR_NEW_CJ_ARRAY)(arrayDesc, length)
      }
    } else {
      RTSCall(RTSProc.JR_NEW_CJ_REFARRAY)(/* dummy */ IConst(0), arrayDesc, length, LConst(0))
    }
  }

  private def genNewAJArray(arrayType: SymType, arrayDesc: Node, length: Node): Node = {
    val elemType = arrayType.getArrayElemType.symType
    if (elemType.isPrimitive) {
      val log2size = IConst(elemType.log2Size)
      RTSCall(RTSProc.JR_NEW_AJ_PRIMARRAY)(log2size, arrayDesc, length, LConst(0))
    } else {
      RTSCall(RTSProc.JR_NEW_AJ_REFARRAY)(/* dummy */ IConst(0), arrayDesc, length, LConst(0))
    }
  }

  private[lowering] def lowerNewArrayCopy(newArray: NewArrayCopy): Node = {
    val arrayType = newArray.allocType.symType
    // TODO: JET-17408
    if (arrayType.isCangjieArray) {
      val inlineContext = newArray.inlineContext
      assert(inlineContext != null, "inlineContext must not be null")
      val domain = inlineContext.method.getDomain.ordinal
      if (newArray.uninitialized && newArray.length != newArray.count) {
        // the rest of created array will be left as is
        RTSCall(RTSProc.CJ_ArrayCopyDirty)(newArray.src, newArray.srcPos, newArray.length, newArray.count, IConst(domain))

      } else if (newArray.length == newArray.count || newArray.value == LConst(0)) {
        // the rest of created array (if not fully copied) will be zeroed by specialized AddrUInt.ZERO
        RTSCall(RTSProc.CJ_ArrayCopyGeneric)(newArray.src, newArray.srcPos, newArray.length, newArray.count, IConst(domain))

      } else {
        assert(arrayType.getArrayElemType.isPrimitive)
        val log2Size = arrayType.getArrayElemType.symKindErased.log2Size
        val patternToMul = log2Size match {
          case 0 => 0x0101010101010101L
          case 1 => 0x0001000100010001L
          case 2 => 0x0000000100000001L
          case 3 => 0x0000000000000001L
        }
        val fillValue = Mul(newArray.value, LConst(patternToMul)) // convert one element value to W64 filling pattern
        RTSCall(RTSProc.CJ_ArrayCopyInitGeneric)(newArray.src, newArray.srcPos, newArray.length, newArray.count, fillValue, IConst(domain))
      }

    } else {
      assert(arrayType.isJBCArray)
      val baseType = arrayType.getArrayBase
      val arrayDesc = RawInstanceDescriptor(arrayType)
      assert(newArray.value == LConst(0))

      guardedInlinedAllocator(newArray, newArray.length) { unit =>
        val primArray = if (arrayType.getArrayElemType.isPrimitive) 1 else 0
        inlinedCall(newArray.inlinedAllocator)(arrayDesc, newArray.length, newArray.src, newArray.srcPos, newArray.count, IConst(primArray), LConst(unit))
      } {
        if (arrayType.getArrayDimnum == 1 && baseType.isPrimitive) {
          val log2size = IConst(baseType.log2Size)
          RTSCall(RTSProc.JR_NEW_PRIMARRAY_COPY)(log2size, arrayDesc, newArray.length, newArray.src, newArray.srcPos, newArray.count)
        } else {
          assert(!baseType.isDeferred)
          RTSCall(RTSProc.JR_NEW_REFARRAY_COPY)(/* dummy */ IConst(0), arrayDesc, newArray.length, newArray.src, newArray.srcPos, newArray.count)
        }
      }
    }
  }

  private[lowering] def lowerNewArrayCopyRT(arrayCopy: NewArrayCopyRT) = {
    // distinguish allocators that take a range in the original array or a length from the start only,
    // because they throw different exceptions when bad range is passed
    val args = if (arrayCopy.isCopyOfRange) {
      Seq(arrayCopy.src, arrayCopy.from, arrayCopy.to)
    } else {
      assert(arrayCopy.from match { case IConst(0) => true; case _ => false })
      Seq(arrayCopy.src, arrayCopy.to)
    }
    guardedInlinedAllocator(arrayCopy, Sub(arrayCopy.to, arrayCopy.from)) { unit =>
      val desc = if (!arrayCopy.allocType.getArrayElemType.isPrimitive) {
        // JET-12887: allocType of Arrays.copyOf is erased to Object[], so we must use descriptor from src array instead
        InstanceDescriptorBy(arrayCopy.src)
      } else {
        // Note however that prim arrays are not erased, so we can just use constant descriptor itself.
        RawInstanceDescriptor(arrayCopy.allocType.symType)
      }
      inlinedCall(arrayCopy.inlinedAllocator)(desc +: args :+ LConst(unit): _*)
    } {
      DirectCall(arrayCopy.allocatorProc(inlined = false))(args: _*)
    }
  }

  private def guardedInlinedAllocator(newArray: InlineableAllocatorWithGuard, length: => Node)(inlined: Long => Call)(generic: => Call) = {
    import InlineableAllocatorWithGuard._
    if (newArray.shouldBeInlined) {
      val FAKE_NORMAL = RTConst.AllocUnit.FAKE_NORMAL.addrValue
      val BYTES_IN_UNIT = RTConst.AllocUnit.SIZE.intValue

      def lenBySize(size: Int) = {
        (size - RTConst.JavaArray.ARRAY_BODY_OFFS.intValue) / newArray.allocType.getArrayElemType.symType.size
      }

      newArray.sizeGuard match {
        case NoGuard => inlined(FAKE_NORMAL)

        case PointGuard(size) =>
          assert(size >= BYTES_IN_UNIT)
          val minUnitLen = lenBySize(size - BYTES_IN_UNIT)
          val lenInUnit = BYTES_IN_UNIT / newArray.allocType.getArrayElemType.symType.size
          val check = If(Cmp(IntType, Condition.ULT)(Sub(length, IConst(minUnitLen)), IConst(lenInUnit)))

          continue(check.trueExit)
          val unit = size / BYTES_IN_UNIT
          val inlinedCall = inlined(unit) at Goto()

          continue(check.falseExit)
          ColdCodeMarker()
          val normalCall = generic at Goto()

          join(inlinedCall, normalCall)

        case LevelGuard(size) =>
          assert(size >= BYTES_IN_UNIT)
          val guardLen = lenBySize(size)
          val check = If(Cmp(IntType, Condition.ULE)(length, IConst(guardLen)))

          continue(check.trueExit)
          val inlinedCall = inlined(FAKE_NORMAL) at Goto()

          continue(check.falseExit)
          ColdCodeMarker()
          val normalCall = generic at Goto()

          join(inlinedCall, normalCall)
      }

    } else {
      generic
    }
  }

  private def lowerAnyNewStackAllocated(kind: FrameSlot.Kind, inLoop: Boolean, typeDesc: Node, tsWord: Int) = {

    val compactMode = RTConst.CompactHeader.COMPACT_HEADER_ENABLED.boolValue
    val tswordSize = if (compactMode && !RTConst.CompactHeader.DEBUG_TSWORD_FIELD_PRESENT.boolValue) {
      0
    } else {
      RTConst.HeapObj.TSWord.SIZE.intValue
    }

    val zeroingOffset = Env.addressSize +    // td (union with tsword for compact mode)
                        tswordSize +         // tsWord (separate field in non-compact mode)
                        0                    // lockWord

    val mem = StackAlloc(kind)

    // Note that LoopsRecognizer cannot be used during lowering, so `inLoop` is precalculated earlier.
    if ((inLoop || !mem.zeroed) && (mem.size > zeroingOffset)) {
      StackZeroing.Single(zeroingOffset, mem.size - zeroingOffset)(mem)
    }

    if (compactMode) {
      PutField(RT.HeapObj.td)(mem,
        Or(
          typeDesc,
          addrConst(tsWord.toLong << 32))
        )
      if (RTConst.CompactHeader.DEBUG_TSWORD_FIELD_PRESENT.boolValue) {
        PutField(RT.HeapObj.tsWordDebug)(mem, IConst(tsWord))
      }
    } else {
      PutField(RT.HeapObj.td)(mem, typeDesc)
      PutField(RT.HeapObj.tsWord)(mem, IConst(tsWord))
    }

    // already zeroed
    assert(RTConst.LockWord.LOCK_WORD_FREE_OBJECT.intValue == 0)

    mem
  }

  private[lowering] def lowerNewStackAllocated(newOp: NewStackAllocated) = {
    val klass = asClassType(newOp.allocType)
    assert(klass.isClass)

    val tsWord = nonLargeClassTSWord(klass, inHeap = false)

    // TODO: use klass.getRawObjectSize for all stack allocations
    val rawEop = lowerAnyNewStackAllocated(FrameSlot.NewOnStack(newOp.allocType), newOp.inLoop, RawInstanceDescriptor(klass), tsWord)

    PublishRef(rawEop)
  }

  private[lowering] def lowerNewArrayStackAllocated(newArray: NewArrayStackAllocated) = {
    val arrayType = newArray.allocType.symType
    assert(arrayType.isArray)

    val length = newArray.lengths match {
      case Seq(IntegralConst(x)) => x
      case _ => shouldNotReachHere()
    }

    val desc = RawInstanceDescriptor(arrayType)
    val tsWord = nonLargeArrayTSWord(arrayType, length, inHeap = false)

    val rawEop = lowerAnyNewStackAllocated(FrameSlot.NewArrayOnStack(newArray.allocType, Math.toIntExact(length)), newArray.inLoop, desc, tsWord)

    val arrayLength = if (arrayType.isAJArray) RT.RawAJArray.length
    else if (arrayType.isCangjieArray) RT.RawCangjieArray.length
    else if (arrayType.isXScalaArray) RT.RawScalaArray.length
    else RT.RawJavaArray.length

    PutField(arrayLength)(rawEop, IConst(Math.toIntExact(length))) // TODO: support AddrUInt length

    PublishRef(rawEop)
  }


  private def hasInstanceMember[M <: Member](klass: SymClassType,
                                             getMembers: SymClassType => Iterator[M],
                                             memberPredicate: M => Boolean) = {
    @inline
    @tailrec
    def iter(klass: SignatureType): Boolean = {
      klass != null &&
        ((getMembers(asClassType(klass)) exists { m => !m.isStatic && memberPredicate(m) }) || iter(asClassType(klass).getSuperClassSig))
    }

    iter(SignatureType.fromSymType(klass))
  }

  private def isSpecial(klass: SymClassType): Boolean =
    env.getTypeProvider.getAJWeakRefType.isAssignableFrom(klass) ||
      env.getTypeProvider.getReferenceType.isAssignableFrom(klass) ||
      env.getTypeProvider.getBacktraceType.isAssignableFrom(klass) ||
      env.getTypeProvider.isCangjieWeakRef(klass)

  private def nonLargeClassTSWord(klass: SymClassType, inHeap: Boolean): Int =
    klass.computeTSWordForClass(
      isStackAlloc = !inHeap, hasCHA = true, special = isSpecial(klass))

  private def nonLargeArrayTSWord(arrayType: SymType, length: Long, inHeap: Boolean): Int =
    arrayType.computeTSWordForArray(length, isStackAlloc = !inHeap)

}
