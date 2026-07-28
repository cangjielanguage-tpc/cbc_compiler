/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.lowering

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.common.LanguagePack
import com.huawei.excelsior.common.LanguagePack.SCALA
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.common.XString.ascii
import com.huawei.excelsior.jet.compiler.Env.{isStandalone, languagePack, targetArch}
import com.huawei.excelsior.jet.compiler.bytecode.{ArithOp, BytecodeTypeKind}
import com.huawei.excelsior.jet.compiler.delayed.DelayedIntrinsicsUsageTracker
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.symlevel.{CangjieFieldReference, Field, Method, SignatureType, TypeKind, Type as SymType}
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.*
import com.huawei.excelsior.jet.compiler.types.References.*
import com.huawei.excelsior.jet.compiler.*
import com.huawei.excelsior.jet.compiler.driver.ProjectLogic
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.AddrUInt
import com.huawei.excelsior.jet.util.ScalaCollections
import xscala.util.MathUtils.*

import scala.annotation.nowarn
import scala.collection.mutable

/**
 * Lowering of uncategorized operations.
 *
 * @author alexm
 * @author paul
 */
private[lowering] trait MiscOps extends Toolbox { self: Universe =>

  private def initializedTest(klass: SymType): Node = {
    assert(klass.hasRunTimeTypeInfo)

    val rtti = TypeHandle(klass)
    val initializedFlag = GetField(RT.HostingTypeHandle.initialized)(rtti)
    Cmp(AddrType, Condition.NE)(initializedFlag, addrNull)
  }

  private[lowering] def lowerInitializedTest(test: InitializedTest): Node = {
    initializedTest(test.klass)
  }

  private[lowering] def lowerInitializationCheck(check: AbstractInitializationCheck): Unit = {
    val klass = check.klass
    assert(klass.hasRunTimeTypeInfo)

    // Note that this check is just a fast-path for the most common case.
    // Also note that it would be optimized in case of dominating initialized test, don't try to overengineer here.
    val initializedCheck = If(initializedTest(check.klass))

    // 1. Preclinited clinits must be eliminated by context types.
    // 2. Generation of asserts about preclinited classes dramatically slows down compilation time, see JET-10481.
    assert(klass.isCangjiePackage || !klass.isPreClinited || isO1Compiled)

    val (slowPathProc, arg) = check match {
      case _: Clinit =>            (RTSProc.JR_Clinit,            ImportedIndex(klass))
      case _: ClinitedAssert =>    (RTSProc.JR_ClinitedAssert,    TypeHandle(klass))
      case _: InitializedAssert => (RTSProc.JR_InitializedAssert, TypeHandle(klass))
      case _: PackageInit =>       (RTSProc.JR_Clinit,            ImportedIndex(klass))
      case _: PackageInitCheck =>  (RTSProc.JR_ClinitedAssert,    TypeHandle(klass))
    }
    coldBlockWithRTSCall(initializedCheck.falseExit)(slowPathProc, arg)

    continue(Goto(), initializedCheck.trueExit)
  }

  private[lowering] def lowerPreparationCheck(check: PreparationCheck): Unit = {
    def canAssertTypePreparation: Boolean = check.inlineContext.method.canAssertTypePreparation(check.klass)

    def genPreparationCheck(failCase: BlockExit => Option[BlockExit]): Unit = {
      val rawTypeHandle = TypeHandle(check.klass)
      val flags = GetField(RT.TypeHandle.flags)(rawTypeHandle)
      val isPrepared = If(Cmp(IntType, Condition.NE)(And(flags, IConst(RTConst.TypeHandle.Flags.PREPARED.intValue)), IConst(0)))
      val continueExit = failCase(isPrepared.falseExit).toSeq
      continue(continueExit :+ isPrepared.trueExit: _*)
    }

    PreparationCheck.markForPreparation(check)

    val kind = check.kind
    if (kind.`lazy` && !kind.assertionOnly) {
      genPreparationCheck { failCase =>
        coldBlockWithRTSCall(failCase)(RTSProc.JR_PrepareType, TypeHandle(check.klass))
        Some(Goto())
      }
    } else if ((kind.assertionOnly || env.enabled(PreparationAsserts)) && canAssertTypePreparation) {
      genPreparationCheck { failCase =>
        coldBlockWithErrorRTSCallAndHalt(failCase)(check,
          RTSProc.JR_FatalError, AJString.bstr(ascii(s"type ${check.klass.getName} should be prepared")))
        None
      }
    }

    assert(kind.`lazy` || ProjectLogic.useLazyPreparation) // otherwise it should have been eliminated by EagerPreparationChecksElimination
  }

  private[lowering] def lowerClassObject(classObject: XClassObject): Node = {
    val symType = classObject.symType
    assert(!symType.isDeferred)

    val getClassObjectRoutine = if (symType.isJavaReference) {
      RTSProc.JR_GetClassObject
    } else if (symType.isXScalaType) {
      RTSProc.JR_GetScalaClassObject
    } else {
      shouldNotReachHere(symType)
    }

    // TODO: inline RT method instead of following hand-written code.
    if (symType.isJBCArray) {
      RTSCall(getClassObjectRoutine)(TypeHandle(symType.getArrayBase), IConst(symType.getArrayDimnum))

    } else {
      val typeHandle = TypeHandle(symType)
      val classObject = GetField(RT.TypeHandle.classObject)(typeHandle)
      val flags = GetField(RT.TypeHandle.flags)(typeHandle)

      val hasClassObjectFlag = if (symType.isJavaReference) {
        RTConst.JavaTypeHandle.Flags.HAS_CLASS_OBJECT
      } else if (symType.isXScalaType) {
        RTConst.ScalaTypeHandle.Flags.HAS_CLASS_OBJECT
      } else {
        shouldNotReachHere(symType)
      }

      val isClassObjectCreated = If(Cmp(IntType, Condition.NE)(And(flags, IConst(hasClassObjectFlag.intValue)), IConst(0)))

      val slowPathCall = coldBlockWithRTSCall(isClassObjectCreated.falseExit)(getClassObjectRoutine, TypeHandle(symType), IConst(0))
      join(slowPathCall at Goto(), classObject at isClassObjectCreated.trueExit)
    }
  }

  private[lowering] def lowerGetClass(getClass: GetClass): Node = {
    if (languagePack == LanguagePack.SCALA) {
      RTSCall(RTSProc.AnyRefNatives_getClass)(getClass.obj)
    } else {
      RTSCall(RTSProc.ObjectNatives_getClass)(getClass.obj)
    }
  }

  private[lowering] def lowerNullCheck(nullCheck: NullCheck): Unit = {
    assert(nullCheck.trusted)

    // TODO: support rt-calls in fusion and remove `CangjieFusionMode` check
    if (Env.isWorkMode && !env.enabled(BoolOption.CangjieFusionMode)) {
      val obj = nullCheck.obj
      val check = If(Cmp(obj.tpe, Condition.EQ)(nullCheck.obj, AnyNull(obj.tpe)))
      coldBlockWithErrorRTSCallAndHalt(check.trueExit)(nullCheck, RTSProc.JR_FatalError, AJString.bstr("NullPointerException"))
      continue(check.falseExit)
    }
  }

  private[lowering] def lowerGetFlatThinCheck(getFlatThinCheck: GetFlatThinCheck): Unit = {
    assert(getFlatThinCheck.trusted)

    if (Env.isWorkMode) {
      val check = If(Cmp(AddrType, Condition.EQ)(getFlatThinCheck.base, addrNull))
      coldBlockWithErrorRTSCallAndHalt(check.trueExit)(getFlatThinCheck, RTSProc.JR_FatalError, AJString.bstr("flat thin read from null address"))
      continue(check.falseExit)
    }
  }

  private[lowering] def lowerThinNullCheck(nullCheck: ThinNullCheck): Unit = {
    assert(nullCheck.trusted)

    if (Env.isWorkMode) {
      val check = If(Cmp(ThinType, Condition.EQ)(nullCheck.obj, ThinNull()))
      coldBlockWithErrorRTSCallAndHalt(check.trueExit)(nullCheck, RTSProc.JR_FatalError, AJString.bstr("ThinNull dereference"))
      continue(check.falseExit)
    }
  }

  private[lowering] def lowerTrustedDivisorCheck(divisorCheck: DivisorCheck): Unit = {
    assert(divisorCheck.trusted)

    if (Env.isWorkMode) {
      divisorCheckBody(divisorCheck, RTSProc.JR_FatalError, AJString.bstr("div/rem by 0"))
    }
  }

  private[lowering] def divisorCheckBody(divisorCheck: DivisorCheck, errProc: RTSProc, args: Node*): Unit = {
    val tpe = divisorCheck.divisor.tpe
    val check = If(Cmp(tpe, Condition.EQ)(divisorCheck.divisor, IntegralConst(tpe)(0)))
    coldBlockWithErrorRTSCallAndHalt(check.trueExit)(divisorCheck, errProc, args: _*)
    continue(check.falseExit)
  }

  /** Lowering node to predicate for overflow check and result of operation.
    * Source: Hacker's Delight, chapter 2, paragraph 13 "Overflow Detection".
    */
  private[lowering] def lowerCheckedOp(n: CheckedOp): Node = {
    val tpe = n.tpe
    val signed = n.signed

    val (l, r) = (n.l, n.r)

    def lowerCheckedAdd(n: CheckedOp): Node = {
      val res = Add(l, r)
      // Signed overflow occurs iff [ ((x + y) ^ x) & ((x + y) ^ y) ] has 1 in sign.
      // For unsigned overflow it's easier to express check in comparison: y > MAX - x.
      val check = if (signed) {
        If(NonZero(And(And(Xor(res, l), Xor(res, r)), IntegralConst(tpe)(nthBit64(n.width.nbits - 1)))))
      } else {
        If(Cmp(tpe, Condition.ULT)(Sub(IntegralConst(tpe)(rightNBits64(n.width.nbits)), l), r))
      }
      coldBlockWithErrorRTSCallAndHalt(check.trueExit)(n, n.throwProc)
      continue(check.falseExit)
      res
    }

    def lowerCheckedSub(n: CheckedOp): Node = {
      val res = Sub(l, r)
      // Signed overflow occurs iff [ (x ^ y) & ((x - y) ^ x) ] has 1 in sign.
      // For unsigned overflow it's easier to express check in comparasion: x < y.
      val check = if (signed) {
        If(NonZero(And(And(Xor(l, r), Xor(l, res)), IntegralConst(tpe)(nthBit64(n.width.nbits - 1)))))
      } else {
        If(Cmp(tpe, Condition.ULT)(l, r))
      }
      coldBlockWithErrorRTSCallAndHalt(check.trueExit)(n, n.throwProc)
      continue(check.falseExit)
      res
    }

    def lowerCheckedMul(n: CheckedOp): Node = {
      val res = Mul(l, r)
      // Multiplication of 2 n-bits values results in 2n-bits value.
      // Signed multiplication overflows iff high(x, y) != (low(x, y) SSR (n - 1)), where SSR is signed shift right.
      // Unsigned multiplication overflows iff high(x, y) != 0.
      val check = if (signed) {
        If(Cmp(tpe, Condition.NE)(MulH(l, r), ShiftByConst(tpe, ArithOp.ASR, n.width.nbits - 1, res)))
      } else {
        If(Cmp(tpe, Condition.NE)(UMulH(l, r), IntegralConst(tpe)(0)))
      }
      coldBlockWithErrorRTSCallAndHalt(check.trueExit)(n, n.throwProc)
      continue(check.falseExit)
      res
    }

    def lowerCheckedDiv(n: CheckedOp): Node = {
      DivisorCheck()(r)

      if (signed) {
        val check0 = If(Cmp(tpe, Condition.EQ)(l, IntegralConst(tpe)(minExtended(n.width.nbits))))
        continue(check0.trueExit)

        val check1 = If(Cmp(tpe, Condition.EQ)(r, IntegralConst(tpe)(-1L)))
        coldBlockWithErrorRTSCallAndHalt(check1.trueExit)(n, n.throwProc)
        continue(check0.falseExit, check1.falseExit)
      }

      IDivRemOp(tpe, !signed, isDiv = true)(l, r)
    }

    import CheckedOp.Kind._
    n.kind match {
      case ADD => lowerCheckedAdd(n)
      case SUB => lowerCheckedSub(n)
      case MUL => lowerCheckedMul(n)
      case DIV => lowerCheckedDiv(n)
    }
  }

  private[lowering] def lowerArrayIndexCheck(indexCheck: ArrayIndexCheck): Unit = {
    if (!indexCheck.trusted) {
      val check = If(Cmp(indexCheck.idx.tpe, Condition.UGE)(indexCheck.idx, indexCheck.length))
      coldBlockWithErrorRTSCallAndHalt(check.trueExit)(indexCheck, indexCheck.throwProc)
      continue(check.falseExit)
    }

    if (!isO1Compiled) {
      val range = indexCheck.filteredValueRange
      if (range.isEmpty) {
        // TODO: replace by throw before lowering
      } else {
        val (from, to) = ValueRange.bounds(range)
        RawValueRangeFilter(indexCheck.idx, from, to)
      }
    }

    // TODO: generate check with fatal in work mode
  }

  private def getPossiblyRichNode(astoreCheck: ArrayStoreCheck): Node = {
    assert(astoreCheck.valueHasRelaxedType)
    val symType = astoreCheck.valueRelaxedType.symType
    astoreCheck.value match {
      case Deprive(`symType`, x) => x
      case _ => null
    }
  }

  private def genCheckElemType(array: Node, formalArrayBase: ReferenceType) = {
    val desc = genInstanceDescriptorAddr(array)
    val baseTypeField = if (formalArrayBase.symType.isXScalaType) {
      RT.ScalaInstanceDescriptor.arrayBaseType
    } else {
      RT.JavaInstanceDescriptor.arrayBaseType
    }
    val baseType = GetField(baseTypeField)(desc)
    If(Cmp(AddrType, Condition.EQ)(baseType, TypeHandle(formalArrayBase.symType)))
  }

  private def genCheckElemTypeAndCheckRich(astoreCheck: ArrayStoreCheck, arrayTypeIsPoint: Boolean, formalArrayBase: ReferenceType) = {
    if (astoreCheck.valueHasRelaxedType) {
      val possiblyRichNode = getPossiblyRichNode(astoreCheck)
      val cannotGenerateFastPath = possiblyRichNode == null || !useEnrichedPointers
      assert(cannotGenerateFastPath || !JavaArrayType.isSupertype(formalArrayBase))

      if (cannotGenerateFastPath) {
        (Seq.empty, Seq.empty)

      } else if (!arrayTypeIsPoint) {
        val testArrType = genCheckElemType(astoreCheck.array, formalArrayBase)
        continue(testArrType.trueExit)

        val (checkRich, _, _) = genCheckRich(possiblyRichNode)
        (Seq(checkRich.trueExit), Seq(testArrType.falseExit, checkRich.falseExit))

      } else {
        val (checkRich, _, _) = genCheckRich(possiblyRichNode)
        (Seq(checkRich.trueExit), Seq(checkRich.falseExit))
      }

    } else if (!arrayTypeIsPoint) {
      val testArrType = genCheckElemType(astoreCheck.array, formalArrayBase)
      (Seq(testArrType.trueExit), Seq(testArrType.falseExit))

    } else {
      shouldNotReachHere() // array store checks for array type points and strict value types should be striked out
    }
  }

  private[lowering] def lowerArrayStoreCheck(astoreCheck: ArrayStoreCheck): Unit = {
    if (astoreCheck.trusted) {
      // TODO: generate check with fatal in work mode
      return
    }

    val array = astoreCheck.array
    val value = astoreCheck.value

    val testNull = If(Cmp(value.tpe, Condition.EQ)(value, Null()))
    val joinedEdges = mutable.Buffer(testNull.trueExit)

    val rtCheck = if (astoreCheck.hasFastPathInfo) {
      continue(testNull.falseExit)

      val (arrType, arrayTypeIsPoint) = (astoreCheck.arrayTypeForFastPath match {
        case ProbableType(probableArrType) => probableArrType match {
          case c: UpperBounded => (c.root, false)
        }
        case c: Cone => (c.root, false)
        case p: Point => (p.root, true)
      }): @nowarn("msg=match may not be exhaustive")

      val (testArrTypeSuccessEdges, testArrTypeFailedEdges) = arrType match {
        case JavaReferenceArrayType(elemType) if elemType.symType.isJavaLangObject || elemType.symType.isXScalaAnyRef =>
          val td = genInstanceDescriptorAddr(array)
          val objectArrayDesc = RawInstanceDescriptor(arrType.symType)
          val testArrType = ifAddrEq(td, objectArrayDesc)
          (Seq(testArrType.trueExit), Seq(testArrType.falseExit))

        case arrayType: JavaArrayType =>
          genCheckElemTypeAndCheckRich(astoreCheck, arrayTypeIsPoint, arrayType.base.asInstanceOf[ReferenceType]) // Note: we compare array bases, not array elements

        case _ => shouldNotReachHere()
      }
      joinedEdges ++= testArrTypeSuccessEdges

      if (testArrTypeFailedEdges.nonEmpty) {
        continue(testArrTypeFailedEdges: _*)
      }
      ColdCodeMarker()
      RTSCall(astoreCheck.checkArrayStoreOpt)(array, value)
    } else {
      coldBlockWithRTSCall(testNull.falseExit)(astoreCheck.checkArrayStoreNotNull, array, value)
    }

    val testRTCheck = If(Cmp(IntType, Condition.NE)(rtCheck, IConst(0)))
    joinedEdges += testRTCheck.trueExit

    coldBlockWithErrorRTSCallAndHalt(testRTCheck.falseExit)(astoreCheck, astoreCheck.throwProc)

    continue(joinedEdges.toSeq: _*)
  }

  /** Splits ArrayFill to a series of ArrayPut operations. */
  private[lowering] def lowerArrayFill(arrayFill: ArrayFill): Unit = {
    val array = arrayFill.array
    val arrayType = arrayFill.arrayType
    val idxType = TypedArrayOperation.idxType(arrayType)
    val elemType = ValueType(arrayFill.elemType)

    for ((value, index) <- arrayFill.storedValues.zipWithIndex) {
      ArrayPut(arrayType)(array, IntegralConst(idxType)(index), IntegralConst(elemType)(value))
    }
  }

  protected def copyRecord(t: SignatureType, dst: Node, src: Node): Unit = {
    if (env.enabled(SmartRecordZeroing) && t.hasRefFields) {
      copyRecordByFields(t, dst, src)
    } else {
      copyMemory(t.getRawObjectSize, t.symType.getObjectAlignment, dst, src)
    }
  }

  protected def copyRecordByFields(t: SignatureType, dst: Node, src: Node): Unit = {
    assert(t.isRecord, t)
    if (isStandalone) {
      val (dstObj, dstFields) = dst match {
        case dst: GetFieldSeqRef => (Some(dst.obj), dst.fields)
        case dst: GetStaticFieldSeqRef => (None, dst.fields)
        case dst => (Some(dst), Seq.empty)
      }
      val (srcObj, srcFields) = src match {
        case src: GetFieldSeqRef => (Some(src.obj), src.fields)
        case src: GetStaticFieldSeqRef => (None, src.fields)
        case src => (Some(src), Seq.empty)
      }
      copyRecordByFieldSeq(t, dstObj, dstFields, srcObj, srcFields)

    } else {
      for (f <- asClassType(t).getFields if !f.isStatic && !f.getType.isZST) {
        val srcValueOrAddr = t match {
          case t: SignatureType.InstantiatedType =>
            UniversalGeneric.GetField(f, t, f.getType.instantiate(t.instantiatedTypeParameters, Seq.empty))(src)
          case _ => GetField(f)(src)
        }

        if (f.isAJFlat) {
          val fieldType = f.getType ensuring(_.isRecord, f)
          // TODO: generic
          val dstAddr = GetField(f)(dst)
          copyRecord(fieldType, dstAddr, srcValueOrAddr)

        } else {
          t match {
            case t: SignatureType.InstantiatedType =>
              UniversalGeneric.PutField(f, t, f.getType.instantiate(t.instantiatedTypeParameters, Seq.empty))(dst, srcValueOrAddr)
            case _ => PutField(f)(dst, srcValueOrAddr)
          }
        }
      }
    }
  }

  private def copyRecordByFieldSeq(refType: SignatureType,
                                     dstObj: Option[Node], dstFields: Seq[CangjieFieldReference],
                                     srcObj: Option[Node], srcFields: Seq[CangjieFieldReference]): Unit = {
    assert(refType.isRecord, refType)
    assert(isStandalone)

    val fields = (refType: @unchecked) match {
      case refType: SignatureType.Tuple =>
        for ((t, i) <- refType.params.iterator.zipWithIndex if !t.isZST)
          yield CangjieFieldReference(i, None, refType, t)
      case refType: SignatureType.InstantiatedType =>
        for (f <- asClassType(refType).getFields if !f.isStatic && !f.getType.isZST)
          yield CangjieFieldReference(f.getFieldIndex, Some(f), refType, f.getType.instantiate(refType.instantiatedTypeParameters, Seq.empty))
      case refType =>
        for (f <- asClassType(refType).getFields if !f.isStatic && !f.getType.isZST)
          yield CangjieFieldReference(f.getFieldIndex, Some(f), refType, f.getType)
    }
    for (fr <- fields) {
      if (fr.fieldType.isRecord) {
        copyRecordByFieldSeq(fr.fieldType, dstObj, dstFields :+ fr, srcObj, srcFields :+ fr)

      } else {
        val srcValue = srcObj match {
          case Some(src) => LoadFieldSeq(srcFields :+ fr)(src)
          case None => LoadStaticFieldSeq(srcFields :+ fr)
        }
        dstObj match {
          case Some(dst) => StoreFieldSeq(dstFields :+ fr)(dst, srcValue)
          case None => StoreStaticFieldSeq(dstFields :+ fr)(srcValue)
        }
      }
    }
  }

  /** Splits ArrayFill to a series of ArrayPut operations. */
  private[lowering] def lowerAJArrayFill(arrayFill: AJArrayFill): Unit = {
    val array = arrayFill.array
    val value = arrayFill.value

    // Start new block to guarantee valid creation of loop index in header with exactly one forward edge.
    // Backward edge will be added later, when Proxy is replaced by Phi function.
    val header = continue(Goto())
    val index = Proxy(LongType)(header)

    // TODO: JET-17408
    val whileCheck = If(Cmp(index.tpe, Condition.ULT)(index, CangjieArrayLength(array)))

    continue(whileCheck.trueExit)

    {
      val arrayType = arrayFill.arrayType
      assert(!arrayType.getArrayElemType.isZST, "there should be no array filling for ZST array")
      if (arrayType.isRecordArray) {
        val addr = ArrayGet(arrayType)(array, index)
        copyRecord(arrayType.getArrayElemType, addr, value)
      } else {
        ArrayPut(arrayType, arrayFill.enrichedElemType)(array, index, value)
      }
    }

    // Add backward edge and replace Proxy with proper Phi function.
    val backEdge = Goto()
    header.addArg(backEdge)
    assert(header.args.size == 2)
    index.replaceBy(Phi(index.tpe)(header, LConst(0), Add(index, LConst(1))))

    continue(whileCheck.falseExit)
  }

  private[lowering] def lowerThreeCmp(threeCmp: ThreeCmp): Node = {
    // TODO: use conditional moves
    val op = threeCmp.op
    val l = threeCmp.l
    val r = threeCmp.r
    val argsType = l.tpe

    val testEQ = If(Cmp(argsType, Condition.EQ)(l, r))

    continue(testEQ.falseExit)
    val testLT = If(Cmp(argsType, if (op == ArithOp.CMPL) Condition.LT_OR_UNORDERED else Condition.LT)(l, r))

    join(IConst(0)  at testEQ.trueExit,
         IConst(-1) at testLT.trueExit,
         IConst(1)  at testLT.falseExit)
  }

  private[lowering] def lowerStrConcat(concat: StrConcat): Node = {
    //assert(concat.argTypes forall (!_.isJavaLangObject))

    val format = concat.formatString
    val specMethod = if (!concat.isAJ && env.enabled(SpecializeStrConcat)) env.getSpecStrConcatMethod(s"StrConcat_concat_$format") else null
    if (specMethod != null) {
      stats.count(StatsKind.StringOpt, s"strconcat lowered[spec]: $format", concat)
      // Replace StrConcat node with specialized concatenation method
      DirectCall(specMethod)(concat.concatenatedArgs: _*)

    } else {
      stats.count(StatsKind.StringOpt, s"strconcat lowered[gen]: $format", concat)
      // Replace StrConcat node with general concatenation method

      def argSize(tpe: SymType) =
        alignUp(tpe.size, RTConst.AJStrConcatGeneric.Args.SLOT_SIZE.intValue)

      val (primitiveArgsArray, hasPrimitiveArgs) = {
        val size = ScalaCollections.sumBy(concat.argTypes filter (_.isPrimitive))(argSize)
        if (size > 0) {
          (StackAlloc.raw(size, RTConst.AJStrConcatGeneric.Args.alignment), true)
        } else {
          (addrNull, false)
        }
      }

      val stringArrayType = SignatureType.fromSymType(
        if (concat.isAJ) {
          typeProvider.getAJArrayType(BytecodeTypeKind.CLASS)
        } else {
          typeProvider.getArrayType(typeProvider.getStringType, 1)
        }
      )

      import TypeKind._
      val (stringArgsArray: Node, hasStringArgs) = {
        val size = concat.argTypes.count(_.getKind match {
          // These arguments are converted into strings at runtime.
          // See com.huawei.excelsior.jet.runtime.javalib.java.lang.StrConcatGeneric#strConcat
          case LONG | FLOAT | DOUBLE | CLASS => true
          case _ => false
        })
        if (size > 0) {
          val proto =
            if (!isO1Compiled && env.enabled(GenStackAlloc) && !concat.cold) {
              NewArrayStackAllocated(stringArrayType)
            } else {
              NewArray(stringArrayType)
            }
          (proto(IntegralConst(TypedArrayOperation.lenType(stringArrayType))(size)), true)
        } else {
          (Null(), false)
        }
      }

      // Distribute arguments between unmanaged chunk of memory and managed Java array.
      var primitiveArgsOffset = 0
      var stringArgsPos = 0
      for ((arg, symType) <- concat.concatenatedArgs zip concat.argTypes) {
        assert(!symType.isThinClass)
        if (symType.isPrimitive) {
          // Put primitives into unmanaged memory area.
          assert(hasPrimitiveArgs)
          val addr = addAddrInt(primitiveArgsArray, IConst(primitiveArgsOffset))
          val sig = SignatureType.fromSymType(symType)
          StoreMemory(sig.toAsm, sig, atomic = false)(addr, arg)
          primitiveArgsOffset += argSize(symType)

          if (!concat.isAJ) {
            // currently no placeholders are required for JR_AJStrConcat
            symType.getKind match {
              case LONG | FLOAT | DOUBLE =>
                // There is no need to put Null values in array because it's already nullified.
                stringArgsPos += 1
              case _ =>
            }
          }
        } else {
          // Put referenced objects into Java array.
          assert(hasStringArgs)

          if (env.enabled(GenerateWriteBarriers)) {
            VerificationInstanceWriteBarrier(stringArgsArray, arg)
          }

          ArrayPut(stringArrayType)(stringArgsArray, IntegralConst(TypedArrayOperation.idxType(stringArrayType))(stringArgsPos), arg)
          stringArgsPos += 1
        }
      }

      val rtStrConcatProc = if (concat.isAJ) RTSProc.JR_AJStrConcat else RTSProc.JR_StrConcat
      RTSCall(rtStrConcatProc)(AJString.bstr(ascii(format)), primitiveArgsArray, stringArgsArray)
    }
  }

  private[lowering] def lowerAJCallerClass(ajCallerClass: AJCallerClass): Node = ajCallerClass.depth match {
    case IConst(depth) =>
      assert(depth >= 0)

      val callStack = ajCallerClass.ic
        .toRoot
        .filterNot(ic => ic.method.isSkippedByCallStackIterator || ic.klass.isJetRuntimeClass)
        .take(depth + 1) // trim to avoid unnecessary iteration
        .toSeq

      if (depth == callStack.size - 1) {
        TypeHandle(callStack.last.klass)
      } else {
        RTSCall(RTSProc.getCallerClassHandleForFrame)(FrameHeader(), IConst(depth - callStack.size))
      }

    case depth => RTSCall(RTSProc.getCallerClassHandleForFrame)(IntegralConst(AddrType)(0), depth)
  }

  private[lowering] def lowerErrorRTSCall(errorRTSCall: ErrorRTSCall): Unit = {
    coldBlockWithErrorRTSCallAndHalt(Goto())(errorRTSCall, errorRTSCall.proc, errorRTSCall.invokeArgs: _*)
  }

  private def genGetNormalStaticField(op: FieldOperation): Node = {
    val field = op.field
    assert(!field.getDeclaringClass.isDeferred && field.isStatic && !field.isAJFlat)
    LoadMemory(op.accessType, field.getType, atomic = field.isVolatile)(SymbolAddress(field.getStaticFieldSymbol))
  }

  private[lowering] def lowerGetStatic(getStatic: GetStatic): Node = {
    val tpe = getStatic.tpe
    val field = getStatic.field
    if (field.isAJFlat) {
      assert(tpe == ThinType || tpe == AddrType || tpe.isInstanceOf[RecordAddrType])
      ReinterpretCast(AddrType, tpe)(getStaticFieldAddr(field, getStatic.inCtrl))
    } else {
      genGetNormalStaticField(getStatic)
    }
  }

  private[lowering] def lowerPutStatic(putStatic: PutStatic): Unit = {
    val field = putStatic.field
    assert(!field.isAJFlat)
    val addr = SymbolAddress(field.getStaticFieldSymbol)
    val value = putStatic.inValue0
    StoreMemory(putStatic.accessType, field.getType, atomic = field.isVolatile)(addr, value)
  }

  private def arrayBaseOffset(n: ArrayElementOperation): Int = n match {
    case n: TypedArrayOperation =>
      if (n.arrayType.isAJArray) RTConst.AJArray.BODY_OFFS.intValue
      else if (n.arrayType.isCangjieArray) RTConst.CangjieArray.BODY_OFFS.intValue
      else if (n.arrayType.isXScalaArray) RTConst.ScalaArray.ARRAY_BODY_OFFS.intValue
      else RTConst.JavaArray.ARRAY_BODY_OFFS.intValue

    case _: UArrayGet | _: UArrayPut => 0
  }

  private[lowering] def lowerInstanceFieldOperation(op: InstanceFieldOperation): Node = {
    val tpe = op.tpe
    val field = op.field
    val fieldType = field.getType
    val atomic = field.isVolatile
    val obj = op.obj
    val fieldAddr = Lea.Base(obj, field.getInstanceFieldOffset)
    val accessType = op.accessType

    op match {
      case _: GetField if field.isAJFlat =>
        ReinterpretCast(fieldAddr.tpe, tpe)(fieldAddr)

      case _: GetConstField =>
        LoadMemory.memoryIndependent(accessType, fieldType, atomic)(fieldAddr)

      case _: GetField =>
        LoadMemory(accessType, fieldType, atomic)(fieldAddr)

      case pf: PutField =>
        assert(!field.isAJFlat)
        StoreMemory(accessType, fieldType, atomic)(fieldAddr, pf.inValue0)
    }
  }

  private[lowering] def lowerArrayElementOperation(op: ArrayElementOperation): Node = {
    val array = op.array
    val accessType = op.accessType
    val elemSize = accessType.sizeInBytes
    val elemAddr = Lea.Scaled(array, op.idx, elemSize, arrayBaseOffset(op))

    val sig = op match {
      case op: ArrayGet => op.enrichedElemType
      case op: ArrayPut => op.enrichedElemType
      case _: UArrayPut | _: UArrayGet =>
        // symlevel type in load/store operations used only for enrichment support
        // TODO: refactor this hack
        SignatureType.Primitive(op.accessType)
      case _ => shouldNotReachHere(op)
    }

    op match {
      case op: ArrayGet if op.arrayType.isRecordArray =>
        val arrayType = op.arrayType
        val realElemSize = arrayType.getArrayElemType.symType.getRawObjectSize
        ReinterpretCast(AddrType, RecordAddrType(arrayType.getArrayElemType))(
          Add(ConcealRef(array),
            Add(IntegralConst(AddrType)(arrayBaseOffset(op)),
              Mul(op.idx, IntegralConst(AddrType)(realElemSize))))
        )

      case _: ArrayGetOperation =>
        LoadMemory(accessType, sig, atomic = false)(elemAddr)
      case put: ArrayPutOperation =>
        StoreMemory(accessType, sig, atomic = false)(elemAddr, put.inValue0)
        null
    }
  }

  /** @see [[AggressiveClinitAnalysisAssert]] for description of this check */
  private[lowering] def lowerAggressiveClinitAnalysisCheck(check: AggressiveClinitAnalysisAssert): Unit = {
    val field = check.field

    def actualValue() = depriveIfNeeded(genGetNormalStaticField(check))

    val okCmpAndMsg = if (field.getType.isPrimitive) {
      // In case of primitive field we could check that actual value is "strictly" equal to expected.
      //
      // "Strictly" for floats means that -0.0 isn't equal to 0.0 and NaN is equal to NaN.
      // To do this we reinterpret floats as integers.
      //
      // This check is quite useless if expected value is equal to default zero value
      // but it's easier to generate it rather than not.

      globallyAnalyzeClinitForPrimitiveFieldValue(field) collect {
        case (true, expectedValue) =>
          val tpe = check.accessType
          val from = ValueType(tpe)
          val to = if (tpe.width.nbytes <= 4) IntType else LongType

          (
            Cmp(to, Condition.EQ)(
              ReinterpretCast(from, to)(actualValue()),
              ReinterpretCast(from, to)(NumericalConst(expectedValue))
            ),
            s"it's not yet initialized with expected value '$expectedValue'"
          )
      }

    } else {
      // In case of reference field we check that expected non-nullable value is actually non-null.
      //
      // It's hard to check that actual value corresponds to expected type approximation and
      // this is not needed at the current implementation of clinit analysis.
      //
      // This check won't work for nullable fields.
      // There is no easy way to check that field is initialized in such a case. :(
      // So we prohibit nullable approximations during aggressive clinit analysis (see analysis).

      globallyAnalyzeFieldType(field) match {
        case Some((true, expectedType)) =>
          assert(!expectedType.mayBeNull)
          val value = actualValue()
          Some(
            Cmp(value.tpe, Condition.NE)(value, AnyNull(value.tpe)),
            s"it's not yet initialized with expected non-null value")

        case _ =>
          globallyAnalyzeClinitForArrayFieldLength(field) match {
            case Some((true, _)) =>
              shouldNotReachHere("currently length could be computed only if type is computed")
            case _ =>
          }
          None
      }
    }

    for ((okCmp, msg) <- okCmpAndMsg) {
      val ok = If(okCmp)

      val context = check.inlineContext.method.getFullName
      val fieldName = field.getFullName
      val fullMsg = s"Clinit analysis check failed at '$context' for field '$fieldName': $msg"
      val fullMsgAscii = ascii(fullMsg.replaceAll("[^\\p{ASCII}]", "?"))
      coldBlockWithErrorRTSCallAndHalt(ok.falseExit)(check, RTSProc.JR_FatalError,
        AJString.bstr(fullMsgAscii))

      continue(ok.trueExit)
    }
  }

  private[lowering] def lowerConstString(cs: ConstString): Node = {
    // Constant strings in unmanaged context allowed only for AJ intrinsics implementation
    assert(cs.inlineContext.method.isManaged)

    val str = cs.str

    assert(!str.getHost.isInfectedAJClass,
      s"Constant Java string `${str.value}` from ${cs.pos} isn't allowed in infected AJ class ${str.getHost}")

    val stringTable = str.getStringTable
    val tableAddress = SymbolAddress.controlled(stringTable, cs.inCtrl)
    val stringAddress = Add(tableAddress, addrConst(stringTable.dataOffset + str.getStringNumber * AddrType.size))
    val sig = SignatureType.fromSymType(cs.strType)
    LoadMemory.independent(sig.toAsm, sig, atomic = false)(stringAddress)
  }

  private[lowering] def lowerAJString(ajStr: AJString) =
    AJString(ajStr.str, ajStr.bstr) ensuring {!_.isInstanceOf[AJString]}

  private[lowering] def lowerSymbolAddress(symbolAddress: SymbolAddress) =
    SymbolAddress(symbolAddress.symbol) ensuring {!_.isInstanceOf[SymbolAddress]}

  private[lowering] def lowerThinNew(thinNew: ThinNew): Unit = {
    val ThinNew(initType, addr) = thinNew
    if (initType.isPolyThinClass) {
      PutField(RT.ThinObj.td)(ReinterpretCast(ThinType, AddrType)(addr), ThinTypeHandle(asClassType(initType)))
    }
  }

  private[lowering] def lowerRunTimeTypeInfo(n: RunTimeTypeInfo): Node =
    genRunTimeTypeInfoAddr(n)

  private[lowering] def lowerThisTypeInfo(n: ThisTypeInfo): Node =
    genRunTimeTypeInfoAddr(n)

  private[lowering] def lowerInstanceDescriptor(n: InstanceDescriptor): Node =
    SymbolAddress.controlled(n.targetType.getInstanceDescriptor, n.inCtrl)

  private[lowering] def lowerInstanceDescriptorBy(n: InstanceDescriptorBy): Node = genInstanceDescriptorAddr(n.obj)

  private[lowering] def lowerThisTypeInfoBy(n: ThisTypeInfoBy): Node = genThisTypeInfoAddr(n.obj)

  private def getStaticFieldAddr(field: Field, inCtrl: ControlNode) =
    SymbolAddress.controlled(field.getStaticFieldSymbol, inCtrl)

  private[lowering] def lowerFieldAddr(n: FieldAddr): Node =
    getStaticFieldAddr(n.field, n.inCtrl)

  private[lowering] def lowerExportedToCWrapperAddr(n: CFuncWrapperAddr): Node = {
    val declClass = n.target.getDeclaringClass

    val rtti = GetField(RT.TypeHandle.td)(TypeHandle(declClass))
    val wrappers = GetField(RT.HostingRunTimeTypeInfo.cFuncWrappers)(rtti)
    UArrayGet(I64)(wrappers, IConst(n.target.getCFuncWrapperIndex))
  }

  private[lowering] def lowerVirtualMethodAddr(n: VirtualMethodAddr): Node =
    getVirtualMethodAddr(n.originalRef, n.obj)

  private[lowering] def lowerGetElementPtr(n: GetElementPtr): Node =
    Lea.Base(ReinterpretCast(n.base.tpe, AddrType)(n.base), n.field.getInstanceFieldOffset)

  private[lowering] def lowerGetFlatThin(get: GetFlatThin): Node =
    ReinterpretCast(AddrType, ThinType)(addAddrInt(get.base, get.offset))

  private[lowering] def lowerWriteBarrier(wb: WriteBarrier): Node = {
    val (target, args) = wb match {
      case wb: EscapeWriteBarrier.Instance => (RT.EscapeWriteBarriers.instance, Seq(wb.receiver, wb.value))
      case wb: EscapeWriteBarrier.Static   => (RT.EscapeWriteBarriers.static,   Seq(wb.value))
    }
    val barrierCall = inlinedCall(target)(args: _*)
    WriteBarrierMarker() // TODO: think about where it should be inserted - before, after or in the middle of inlined call
    ReinterpretCast(barrierCall.tpe, wb.tpe)(barrierCall)
  }

  private[lowering] def lowerVerificationWriteBarrier(wb: VerificationWriteBarrier): Unit = {
    if (Env.isWorkMode) {
      val (rtsProc, args) = wb match {
        case wb: VerificationInstanceWriteBarrier => (RTSProc.WriteBarriers_writeBarrier_instance_verification, Seq(wb.receiver, wb.value))
        case wb: VerificationStaticWriteBarrier   => (RTSProc.WriteBarriers_writeBarrier_static_verification,   Seq(wb.value))
      }
      RTSCall(rtsProc)(args: _*)
    }
  }

  private[lowering] def lowerAcquireRawData(n: AcquireRawData) = {
    RTSCall(RTSProc.CJ_AcquireRawData)(n.array)
  }

  private[lowering] def lowerReleaseRawData(n: ReleaseRawData) = {
    RTSCall(RTSProc.CJ_ReleaseRawData)(n.array, n.pointer)
  }

  private[lowering] def lowerArrayLength(al: ArrayLength): Node = {
    // TODO: replace ArrayLength with GetConstField in the whole compiler.
    //       Note that currently GetConstField is very backend node, and
    //       RTStructs are not properly handled by TypeAnalysis and the other middle optimizations.

    import BitFieldExtract._

    val obj = al.array
    al match {
      case _: JavaArrayLength =>
        GetField(RT.JavaArray.length)(obj)
      case _: ScalaArrayLength =>
        GetField(RT.ScalaArray.length)(obj)
      case _: AJArrayLength | _: CangjieArrayLength =>
        val cangjie = al.isInstanceOf[CangjieArrayLength]
        val lenField = if (cangjie) RT.CangjieArray.length else RT.AJArray.length
        val len = GetField(lenField)(obj)
        val check = If(Cmp(len.tpe, Condition.GE)(len, IConst(0)))
        // Fast-path: 32-bit length
        val normalLen = Extend(al.tpe, I32, signExtension = false, len)
        // Slow-path: obtain 64-bit array length
        continue(check.falseExit)
        ColdCodeMarker()
        val largeLenField = if (cangjie) RT.CangjieArray.largeLength else RT.AJArray.largeLength
        val largeLen = GetField(largeLenField)(obj)
        val slowGoto = Goto()
        join(normalLen at check.trueExit, largeLen at slowGoto)
    }
  }

  private [lowering] def lowerBoxing(boxing: BoxedValue): Node = {
    if (boxing.isHot) {
      inlinedCall(boxing.target)(boxing.inValue0)
    } else {
      DirectCall(boxing.target)(boxing.inValue0)
    }
  }

  private [lowering] def procForMathIntrinsic(node: MathIntrinsic): Option[Method] =
    Some(env.getRTSProc(node.kind.rtProc))

  private [lowering] final def lowerMathIntrinsic(node: MathIntrinsic): Node =
    DirectCall(procForMathIntrinsic(node).get)(node.argsSeq: _*)

  private [lowering] def procForMemAtomic(node: MemAtomic): Option[Method] =
    Some(node.rtMethod())

  private [lowering] final def lowerMemAtomic(node: MemAtomic): Node =
    inlinedCall(procForMemAtomic(node).get)(node.valueArgs.toSeq: _*)

  private [lowering] def lowerNewArrayMimic(n: NewArrayMimic) = {
    import AnyNewArray._
    if (n.shouldCheckLengths) {
      n.lengths foreach { l =>

        def genIf(cond: Node)(trueAction: => Unit)(falseAction: => Unit): Unit = {
          val check = If(cond)
          continue(check.trueExit)
          trueAction
          continue(check.falseExit)
          falseAction
        }

        def posLength() = Cmp(l.tpe, Condition.GE)(l, IntegralConst(l.tpe)(0))

        def genError(proc: RTSProc): Unit = coldBlockWithErrorRTSCallAndHalt(Goto())(n, proc)
        def genNegError(): Unit = genError(n.negativeArraySizeErrorProc)

        var passEdge: Goto = null
        def pass(): Unit = passEdge = Goto()

        if (shouldCheckNegativeLength(n.allocType.symType, n.inlineContext)) {
          genIf(posLength()) {
            pass()
          } {
            genNegError()
          }
        } else {
          pass()
        }
        continue(passEdge)
      }
    } else {
      n match {
        case AnyNewArray.Erroneous(_) => shouldNotReachHere()
        case _ =>
      }
    }
    // It's ok to return NoValue here, unless it's CBC, because mimics can only be used as array arguments of ArrayIndexChecks,
    // which don't access the array during or after lowering.
    // For not CBC, ArrayIndexCheck is lowered and doesn't use Array (which is NoValue in our case), but in case of
    // CBC AIC is bytecode instruction and will not be lowered, thus keeping NoValue 'alive' up to Preparation, where
    // NoValue will be replaced with Null, but optimizations between these steps require us not to have NoValue uses.
    if (targetArch != CBC) NoValue() else Null()
  }

  private[lowering] def lowerCompileTimeOp(n: CompileTimeOp): Node = n match {
    case _: IsComputableAtCompileTime => False()
    case n: ComputeAtCompileTime => NoValue()
  }

  private[lowering] def lowerMonitorEnter(monitorEnter: MonitorEnter): Node = {
    val obj = monitorEnter.obj
    if (isUnstructuredLocking) {
      RTSCall(RTSProc.JR_MonitorEnter)(obj)
      // There could be some exits linked by this value but they won't use it, so DCE will kill it.
      NoValue()
    } else {
      inlinedCall(RT.Synchronization.monitorEnterInlined)(obj)
    }
  }

  private[lowering] def lowerMonitorExit(monitorExit: MonitorExit): Unit = {
    val obj = monitorExit.obj
    if (isUnstructuredLocking) {
      RTSCall(RTSProc.JR_MonitorExit)(obj)
    } else {
      val lockingContext = monitorExit.lockingContext
      inlinedCall(RT.Synchronization.monitorExitInlined)(obj, lockingContext)
    }
  }

  private[lowering] def lowerValueConvert(cast: ValueConvert): Node = (cast.fromAsm, cast.toAsm) match {
    case (F16, I32 | I64) =>
      ValueConvert(F32, cast.toAsm)(ValueConvert(F16, F32)(cast.arg))
    case (I32 | I64, F16) =>
      ValueConvert(F32, F16)(ValueConvert(cast.fromAsm, F32)(cast.arg))

    case (F16, F32) =>
      DirectCall(Com.Huawei.Excelsior.Aj.Lang.Half.h2f)(cast.arg)
    case (F32, F16) =>
      DirectCall(Com.Huawei.Excelsior.Aj.Lang.Half.f2h)(cast.arg)

    case _ => shouldNotReachHere("lowering of unexpected cast " + cast)
  }

  private[lowering] def lowerCopyStructure(n: CopyStructure): Unit = {
    assert(n.structureType.isRecord)
    assert(!n.structureType.isDeferred)
    copyRecord(n.structureType, n.dst, n.src)
  }

  private[lowering] def lowerLockWrapper(n: LockWrapper): Node = (n, rootMethod.hasManagedExecEnv) match {
    case (_: IncHeldLocks, true) =>
      inlinedCall(RT.StarvationPrevention.incHeldLocks)()
    case (_: DecHeldLocks, true) =>
      inlinedCall(RT.StarvationPrevention.decHeldLocks)()
    case _ => null
  }

  private[lowering] def lowerGetStackDescriptor(): Node = {
    GetField(RT.ExecEnv.stackDescriptor)(ExecEnv())
  }

  private [lowering] final def lowerDelayedOp(op: DelayedOp): Node = {
    assert(env.enabled(IgnoreDelayedIntrinsics) || rootMethod.getDeclaringClass.isJetRuntimeClass, s"${op.name} should be replaced earlier in non-runtime code")
    DelayedIntrinsicsUsageTracker.registerDelayedIntrinsicsUsage(rootMethod)
    coldBlockWithErrorRTSCallAndHalt(Goto())(op, RTSProc.JR_FatalError, AJString.bstr(ascii(op.name)))
    NoValue()
  }

  private[lowering] def lowerExtractEnrichment(n: ExtractEnrichment): Node = {
    inlinedCall(RT.IFaceOps.getEnrichment)(ConcealRef(n.obj))
  }

  private[lowering] def lowerRawEnrich(n: RawEnrich): Node = {
    PublishRef(inlinedCall(RT.IFaceOps.enrich)(n.obj, n.enrichment))
  }

  private[lowering] def lowerRawDeprive(n: RawDeprive): Node = {
    val (richObj, _) = genRichDecompositionActions(n.obj, genExtractObject, () => NoValue())
    val plainObj = ReinterpretCast(EopType.Any, TRefType)(n.obj)

    makePhi(richObj, plainObj)
  }

  private[lowering] def lowerEvacuate(n: Evacuate): Node = {
    val objSrc = n.obj

    val nullBranch = makeNullTest(objSrc)

    val nonStackAllocatedBranch = makeLocationsTagTest(objSrc, RTConst.ObjTags.LOCATION_TYPE_OF_STACK_ALLOC_OBJECT.intValue)
    val call = RTSCall(RTSProc.JR_OBJECT_EVACUATE)(objSrc)
    ColdCodeMarker()
    val evacuated = ReinterpretCast(call.tpe, objSrc.tpe)(call)

    join(objSrc at nullBranch, objSrc at nonStackAllocatedBranch, evacuated at Goto())
  }

  private[lowering] def lowerSingletonObject(n: SingletonObject): Node =
    PublishRef(SymbolAddress(n.allocType.getSingletonObject))

  private[lowering] def lowerZeroRefs(n: ZeroRefs): Unit = {
    assert(n.recordType.isRecord)

    val stackAlloc = n.sa

    val classType = asClassType(n.recordType)
    val sig = SignatureType.fromSymType(typeProvider.getAJObjectType)
    for (offset <- classType.getRefFieldOffsets) {
      val fieldAddr = Lea.Base(stackAlloc, offset)
      StoreMemory(PTR, sig, atomic = false)(fieldAddr, Null())
    }
  }

  private[lowering] def lowerMutFuncHost(host: MutFuncArgNode, isGlobal: Boolean): Node = {
    assert(targetArch != CBC)
    assert(env.enabled(GenerateWriteBarriers), s"$host, ${host.valueUses.toList}")
    if (isGlobal) {
      inlinedCall(RT.EscapeWriteBarriers.alwaysGlobalObject)()
    } else {
      inlinedCall(RT.EscapeWriteBarriers.alwaysLocalObject)()
    }
  }
}
