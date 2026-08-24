/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.cbc.codegen

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.assembler.Location.IReg
import com.huawei.excelsior.jet.assembler.Width.{W32, W64}
import com.huawei.excelsior.jet.assembler.cbc.*
import com.huawei.excelsior.jet.assembler.cbc.CbcFileEncoder.Offset
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.MultiFieldReference
import com.huawei.excelsior.jet.assembler.cbc.Local.*
import com.huawei.excelsior.jet.assembler.cbc.Register.*
import com.huawei.excelsior.jet.assembler.cbc.Register.IR.{IR1, IR2}
import com.huawei.excelsior.jet.assembler.cbc.isa12.{NewIsaParts, Assembler as ISA12Assembler}
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessInfoCollector
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessInfoCollector.LiveState
import com.huawei.excelsior.jet.assembler.cbc.isa12.MemoryAccess.{LoadAccessKind, StoreAccessKind}
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.{MemSpace, SymbolAdapter, Assembler as ForkedISA12Assembler}
import com.huawei.excelsior.jet.assembler.{AsmType, Label, Location, Segment, Symbol, Width}
import com.huawei.excelsior.jet.compiler.Env.{isStandalone, tailRegister}
import com.huawei.excelsior.jet.compiler.NotImplementedFeature.CBC
import com.huawei.excelsior.jet.compiler.abi.ABI
import com.huawei.excelsior.jet.compiler.abi.cbc.FrameCBC
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp.*
import com.huawei.excelsior.jet.compiler.cbc.CbcSignatureAdapter.toCbc
import com.huawei.excelsior.jet.compiler.cbc.{CbcSymbolAdapter, CodeSigSymbol, InstantiatedGenericMethod}
import com.huawei.excelsior.jet.compiler.ir.XInfo
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.FrameComponentCBC.*
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.codegen.LocalLivenessAnalyzerCBC.LocalType
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.{BackEndCBC, FrameComponentCBC}
import com.huawei.excelsior.jet.compiler.opt.backend.codegen.{Code, CodeCBC, CodeGenerator}
import com.huawei.excelsior.jet.compiler.opt.ir.{Resources, Universe}
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{OptionLikeEnum, TypeVariable}
import com.huawei.excelsior.jet.compiler.types.Guards.*
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ReferenceType
import xscala.io.ByteBuffer
import xscala.util.MathUtils
import xscala.util.MathUtils.*

import scala.PartialFunction.condOpt
import scala.annotation.{nowarn, tailrec}

@nowarn("msg=match may not be exhaustive")
trait CodeGeneratorCBC extends CodeGenerator with XSitesToolboxCBC with DebugGeneratorCBC with LocalLivenessAnalyzerCBC { self: Universe with BackEndCBC =>

  type ASM = Assembler

  override protected lazy val asm = if (isStandalone)
    new ForkedISA12Assembler with CbcSymbolAdapter
  else if (Isa12Mode)
    new ISA12Assembler
  else new Assembler

  lazy val cbc: CbcAssembler = asm.asInstanceOf[CbcAssembler]

  override protected lazy val emit = null // TODO-CBC: CodeEmitterCBC

  class CodeGeneratorImplCBC extends CodeGeneratorImpl with DebugGeneratorImplCBC with XSitesGeneratorCBC {

    private def asm = cbc

    private lazy val livenessCollector = LivenessInfoCollector()

    private lazy val fasm = asm.asInstanceOf[ForkedISA12Assembler]
    private lazy val isa12Asm = asm.asInstanceOf[NewIsaParts]
    private lazy val oldIsa = asm.asInstanceOf[OldIsaParts]

    var mayHaveNativeCalls = false

    private object Reg  { def unapply(n: Node): Option[Register] = condOpt(n) { case RegNode(r) => r.asInstanceOf[Register] } }
    private object IReg { def unapply(n: Node): Option[IR]       = condOpt(n) { case Reg(r) if r.isIReg => r.asInstanceOf[IR] } }
    private object FReg { def unapply(n: Node): Option[FR]       = condOpt(n) { case Reg(r) if r.isFReg => r.asInstanceOf[FR] } }

    private object UntypedSlot {
      def unapply(n: Node): Option[(StackSlot.Untyped, LocX)] = NodeWithResource.unapply(n).flatMap {
        case fs: FrameSlotCBC if fs.untypedSlot != null => Some(fs.untypedSlot, fs.local)
        case _ => None
      }
    }

    override protected def widthOf(tpe: Type): Width = tpe match {
      case ConditionType => Width.W32
      case _ => super.widthOf(tpe)
    }

    def normalizeRes(node: Node): IREG | StackSlot.Untyped = node.resource match {
      case ireg: IREG => ireg
      case fs: FrameComponentCBC.FrameSlotCBC => fs.untypedSlot
      case _ if node.isInstanceOf[StackAlloc] =>
        node.asInstanceOf[StackAlloc].slot.asInstanceOf[FrameSlotCBC].untypedSlot
    }

    /** Save gc maps state gathered using [[GCMapsGenerator]] */
    def saveGCState(node: Node): Unit = if (needGCMap(node)) {
      asm match {
        case assembler: ForkedISA12Assembler =>
          val aliveResources = gcMaps(node).toSeq.map(normalizeRes)
          val idxPairs = mutPairsData(node).toSeq.map((base, derived) => (normalizeRes(base), normalizeRes(derived)))
          livenessCollector.saveResources(segment, aliveResources, idxPairs)
        case _ =>
      }
    }

    def saveStateForStackChecks(node: Node): Unit = if (needStackPtrsInfo(node)) {
      asm match {
        case assembler: ForkedISA12Assembler =>
          livenessCollector.saveStackPtrs(segment, stackPtrsData(node).toSeq.map(normalizeRes))
      }
    }

    private def genCmp(cmp: Cmp): Unit = {
      val cv = cmp.singleValueUse.asInstanceOf[CondVal]
      val op = branchOp(flagProducerProperties(cv.condition, cv.negated)._1, cmp.keyType)

      val width = widthOf(cmp.l)
      (cv, cmp.l, cmp.r) match {
        case (IReg(d), IReg(l), r: AnyNull) =>
          if (Isa12Mode) {
            asm.scc(op, d, l, IR.IRZ, width)
          } else {
            asm.scc(op, d, l, 0, width)
          }

        case (IReg(d), IReg(l), IntegralConst(r)) => asm.scc(op, d, l, r, width)
        case (IReg(d), IReg(l), IReg(r))          => asm.scc(op, d, l, r, width)
        case (IReg(d), FReg(l), FReg(r))          => asm.scc(op, d, l, r, width)
      }
    }

    private def genSub(sub: Sub): Unit = {
      (widthOf(sub), sub.isFP, sub, sub.l, sub.r) match {
        case (w, false, IReg(d), IReg(l), IReg(r))          => asm.sub(w, d, l, r)
        case (w, false, IReg(d), IReg(l), IntegralConst(r)) => asm.subi(w, d, l, r)
        case (w, true,  FReg(d), FReg(l), FReg(r))          => asm.fsub(w, d, l, r)

        case (w, true,  FReg(d), FReg(l), FloatingPointConst(r)) if Isa12Mode => isa12Asm.fsubi(d, l, r, w)

        case (w, fp, _, l, r) => shouldNotReachHere(s"unexpected Sub: $w, $fp, ${l.resource}, ${r.resource}")
      }
    }

    private def genDivisorCheck(x: DivisorCheck): Unit = {
      val IReg(r) = x.divisor
      asm.divisorCheck(r)
      addXSite(x)
    }

    private def genIDivRemOp(div: IDivRemOp): Unit = {
      ((div, div.l, div.r): @unchecked) match {
        case (IReg(d), IReg(l), IReg(r)) =>
          assert(d == l)
          addXSite(div)
          val w = widthOf(div)
          (div.isDiv, div.isUnsigned) match {
            case (true,  false) => asm.div(w, d, l, r)
            case (false, false) => asm.rem(w, d, l, r)
            case (true,  true)  => asm.udiv(w, d, l, r)
            case (false, true)  => asm.urem(w, d, l, r)
          }
        case (IReg(d), IReg(l), IntegralConst(c)) =>

          assert(d == l)
          addXSite(div)
          val w = widthOf(div)
          (div.isDiv, div.isUnsigned) match {
            case (true,  false) => asm.divi(w, d, l, c)
            case (false, false) => asm.remi(w, d, l, c)
            case (true,  true)  => asm.udivi(w, d, l, c)
            case (false, true)  => asm.uremi(w, d, l, c)
          }
      }
    }

    private def genFDiv(div: FDiv): Unit = {
      val FReg(l) = div.l
      div.r match {
        case FReg(r)                => asm.fdiv(widthOf(div), fReg(div), l, r)
        case FConst(r) if Isa12Mode => isa12Asm.fdivi(fReg(div), l, r, W32)
        case DConst(r) if Isa12Mode => isa12Asm.fdivi(fReg(div), l, r, W64)
      }
    }

    private def genLogical(op: LogicalBinaryOp): Unit = (widthOf(op), op, op.l, op.r) match {
      case (w, IReg(d), IReg(l), IReg(r)) => op match {
        case _: And => asm.and(w, d, l, r)
        case _: Or  => asm.or(w, d, l, r)
        case _: Xor => asm.xor(w, d, l, r)
      }

      case (w, IReg(d), IReg(l), IntegralConst(c)) => op match {
        case _: And => asm.andi(w, d, l, c)
        case _: Or  => asm.ori(w, d, l, c)
        case _: Xor => asm.xori(w, d, l, c)
      }
    }

    private def genArithCommutativeOp(op: ArithCommutativeOp): Unit = (widthOf(op), op.isFP, op, op.l, op.r) match {
      case (w, false, IReg(d), IReg(l), IReg(r)) => op match {
        case _: Add => asm.add(w, d, l, r)
        case _: Mul => asm.mul(w, d, l, r)
        case _: Pow => asm.pow(w, d, l, r)
        case _: MulH => asm.mulh(w, d, l, r)
        case _: UMulH => asm.umulh(w, d, l, r)
      }

      case (w, false, IReg(d), IReg(l), IntegralConst(c)) => op match {
        case _: Add => asm.addi(w, d, l, c)
        case _: Mul => asm.muli(w, d, l, c)
        case _: Pow => asm.powi(w, d, l, c)
        case _: MulH => asm.mulhi(w, d, l, c)
        case _: UMulH => asm.umulhi(w, d, l, c)
      }

      case (w, true, FReg(d), FReg(l), FReg(r)) => op match {
        case _: Add => asm.fadd(w, d, l, r)
        case _: Mul => asm.fmul(w, d, l, r)
      }

      case (w, true,  FReg(d), FReg(l), FloatingPointConst(r)) if Isa12Mode => op match {
        case _: Add => isa12Asm.faddi(d, l, r, w)
        case _: Mul => isa12Asm.fmuli(d, l, r, w)
      }
    }

    private def genShift(n: Shift): Unit = (n.op, n, n.value, n.num) match {
      case (LSL, IReg(d), IReg(l), IReg(r)) => asm.lsl(widthOf(n), d, l, r)
      case (LSL, IReg(d), IReg(l), Imm8(r)) => asm.lsli(widthOf(n), d, l, r)

      case (LSR, IReg(d), IReg(l), IReg(r)) => asm.lsr(widthOf(n), d, l, r)
      case (LSR, IReg(d), IReg(l), Imm8(r)) => asm.lsri(widthOf(n), d, l, r)

      case (ASR, IReg(d), IReg(l), IReg(r)) => asm.asr(widthOf(n), d, l, r)
      case (ASR, IReg(d), IReg(l), Imm8(r)) => asm.asri(widthOf(n), d, l, r)
    }

    private def genCheckedOp(op: CheckedOp): Unit = {
      (op, op.l, op.r) match {
        case (IReg(d), IReg(l), IntegralConst(r)) =>
          (op.kind: @unchecked) match {
            case CheckedOp.Kind.ADD => if (op.signed) asm.caddi(d, l, r, op.width) else asm.cuaddi(d, l, r, op.width)
            case CheckedOp.Kind.SUB => if (op.signed) asm.csubi(d, l, r, op.width) else asm.cusubi(d, l, r, op.width)
            case CheckedOp.Kind.MUL => if (op.signed) asm.cmuli(d, l, r, op.width) else asm.cumuli(d, l, r, op.width)
            case CheckedOp.Kind.POW => assert(op.signed); asm.cpowi(d, l, r, op.width)
          }

        case (IReg(d), IReg(l), IReg(r)) =>
          op.kind match {
            // swap args of commutative ops to simplify instruction semantics to `mov d, l; d op= r`
            case CheckedOp.Kind.ADD if d == r && d != l => if (op.signed) asm.cadd(d, r, l, op.width) else asm.cuadd(d, r, l, op.width)
            case CheckedOp.Kind.MUL if d == r && d != l => if (op.signed) asm.cmul(d, r, l, op.width) else asm.cumul(d, r, l, op.width)
            case CheckedOp.Kind.ADD => if (op.signed) asm.cadd(d, l, r, op.width) else asm.cuadd(d, l, r, op.width)
            case CheckedOp.Kind.MUL => if (op.signed) asm.cmul(d, l, r, op.width) else asm.cumul(d, l, r, op.width)
            case CheckedOp.Kind.SUB => assert(d != r); if (op.signed) asm.csub(d, l, r, op.width) else asm.cusub(d, l, r, op.width)
            case CheckedOp.Kind.DIV => assert(d != r); assert(op.signed); asm.cdiv(d, l, r, op.width)
            case CheckedOp.Kind.POW => assert(d != r); assert(op.signed); asm.cpow(d, l, r, op.width)
          }
      }

      addXSite(op)
    }

    private def genStackZeroing(sz: StackZeroing): Unit = {
      // We don't support StackZeroing with offset, that's used only by StackZeroing.Single,
      // and that is used only by object allocation on stack that we don't support yet.
      assert(sz.extraOffset == 0)
      // We only support zeroing out FrameSlots, fail when we receive IReg, FReg or Acc
      val startSlot = sz.slot.asInstanceOf[FrameSlotCBC]
      asm.blkzero(startSlot.untypedSlot, sz.size)
      for (i <- 0 until sz.size) {
        mark(FrameCBC.Slot(LocX(startSlot.local.encoding + i)), LocalType.CLEARED)
      }
    }

    private def genCast(cast: Cast): Unit = {
      cast match {
        case ReinterpretCast(_, _, arg) =>
          (cast, arg) match {
            case (Reg(to), Reg(from)) =>
              assert(cast.isFP != arg.isFP)
              asm.mov(to, from, reference = cast.tpe.isTraceableRefType.ensuring(!_))

            case _ => shouldNotReachHere(s"source or dest resource wasn't matched to any reg: (${cast.resource}, ${arg.resource})")
          }

        case ValueConvert(fromType, toType, arg) =>
          (cast, arg) match {
            case (Reg(to), Reg(from)) =>
              asm.convert(toType, fromType, to, from)

            case _ => shouldNotReachHere(s"source or dest resource wasn't matched to any reg: (${cast.resource}, ${cast.arg.resource})")
          }
      }
    }

    private def genNew(n: New): Unit = genNewImpl(n, n.allocType)

    private def genBitcodeDeferredNew(n: BitcodeDeferred.New): Unit = genNewImpl(n, n.allocType)

    private def genNewImpl(n: Node, allocType: SignatureType): Unit = {
      assert(iReg(n) == IR1)
      if (isStandalone) {
        val t = allocType match {
          case SignatureType.Box(t) => t
          case t => t
        }
        val fasm = asm.asInstanceOf[ForkedISA12Assembler]
        t match {
          case t: SignatureType.CangjieReference if t.name.startsWith("$Cl") =>
            fasm.newClosure(IR1, t.toCbc)
          case t: SignatureType.InstantiatedReference if t.name.startsWith("$Cl") =>
            fasm.newClosure(IR1, t.toCbc)
          case _ =>
            fasm.newobj(t.toCbc)
        }
      } else {
        val ftcSigIdx = CodeSigSymbol(allocType)
        if (ftcSigIdx.containsTypeVariables) {
          asm.newobjVST(ftcSigIdx)
        } else {
          asm.newobj(ftcSigIdx)
        }
      }
      addXSite(n)
      saveGCState(n)
    }

    private def memExprHead(n: Node): MemExpr.Head = n match {
      case sa: HasFrameSlot => sa.slot match {
        case slot: TypedFrameSlotCBC => slot.typedSlot
        case slot: FrameSlotCBC => slot.untypedSlot
        case _ => shouldNotReachHere(sa)
      }
      case MutFunc.Combine(IReg(obj), IReg(offset)) => if (!isStandalone)
        MemExpr.Head.RegPair(obj, offset) else shouldNotReachHere("wrong head for standalone")

      case IReg(r) => r
      case _: Void => if (!isStandalone)
        MemExpr.Head.StaticField else shouldNotReachHere("wrong head for standalone")

      case n @ RecordArrayGet(IReg(obj), IReg(idx)) =>
        if (!isStandalone)
          assert(Isa12Mode)
          MemExpr.Head.RecordArray(obj, idx, CodeSigSymbol(n.arrayType))
        else
          shouldNotReachHere("wrong head for standalone")
    }

    @deprecated(message = "cjvm specific")
    private def genGetField(n: GetField): Unit = {
      addXSite(n)

      val Reg(dst) = n
      asm.mov(dst, MemExpr(memExprHead(n.obj), Array(createFieldRef(n.field))))
    }

    @deprecated(message = "cjvm specific")
    private def genGetField(n: UniversalGeneric.GetField): Unit = {
      addXSite(n)

      val Reg(dst) = n
      val body = Array[Symbol](createFieldRef(n))
      asm.mov(dst, MemExpr(memExprHead(n.obj), body, isGeneric = true))
    }

    @deprecated(message = "cjvm specific")
    private def genGetFieldOHM(n: UniversalGeneric.GetFieldOHM): Unit = {
      addXSite(n)

      (n, n.ohms) match {
        case (IReg(dst), sa: StackAlloc) =>
          val dstExpr = MemExpr(dst, Array[Symbol](sa.slot.asInstanceOf[OHMSlotCBC].ohmSlot), isGeneric = true)
          val f = n.field.getPermanent
          assert(n.instantiatedFieldType.isVariableSizeType)
          val body = Array[Symbol](createFieldRef(n))
          val srcExpr = MemExpr(memExprHead(n.obj), body, isGeneric = true)
          oldIsa.mov(dstExpr, srcExpr)
      }
    }

    @deprecated(message = "cjvm specific")
    private def genFieldChainRead(n: FieldChainRead): Unit = {
      addXSite(n)

      val Reg(dst) = n
      val isGeneric = n.fields.head.isGeneric
      val body = n.fields.map(_.asInstanceOf[Symbol])
      asm.mov(dst, MemExpr(memExprHead(n.obj), body, isGeneric))
    }

    @deprecated(message = "cjvm specific")
    private def genPutField(n: PutField): Unit = {
      addXSite(n)

      assert(!n.field.getType.isRecord)
      storeToMemExpr(MemExpr(memExprHead(n.obj), Array(createFieldRef(n.field))), n.inValue0)
    }

    @deprecated(message = "cjvm specific")
    private def genPutField(n: UniversalGeneric.PutField): Unit = {
      addXSite(n)

      assert(!n.field.getType.isRecord)
      val Reg(src) = n.value
      val body = Array[Symbol](createFieldRef(n))
      asm.mov(MemExpr(memExprHead(n.obj), body, isGeneric = true), src)
    }

    private def genFieldSeqOperation(n: FieldSeqOperation): Unit = {
      addXSite(n)

      val fasm = asm.asInstanceOf[ForkedISA12Assembler]
      val adapter = fasm.adapter

      val fieldRefs = n.fields
      val builder = MemSpace.Builder()

      def memExprHead(n: Node): Unit = n match {
        case sa: HasFrameSlot => sa.slot match {
          case slot: TypedFrameSlotCBC => builder.typed(slot.typedSlot)
          case _ => shouldNotReachHere(sa)
        }
        case n @ DerivedPtr(IReg(base), IReg(derived)) =>
          builder.handle(base, derived)

        case n @ ArrayGet(_, _, IReg(obj), IReg(idx)) =>
          assert(n.arrayType.isRecordArray)
          builder.obj(obj)
            .index(idx, n.arrayType.getArrayElemType.toCbc, checked = false)

        case IReg(r) =>
          if (fieldRefs.head.refType.isTraceableReference) {
            builder.obj(r)
          } else {
            builder.rec(r)
          }
      }
      
      def getBaseLocation(n: Node) = n match {
        case IReg(r) => r
        case DerivedPtr(IReg(_), IReg(derived)) => derived
      }

      def fields(fields: Seq[CangjieFieldReference], typeInfos: Seq[Node] = Seq.empty): Unit = {
        for ((f, i) <- fields.zipWithIndex) f.field match {
          case Some(field) =>
            if (f.refType.isVariableLayoutType) {
              val IReg(ti) = typeInfos(i)
              builder.fieldGeneric(adapter.field(f), ti)
            } else {
              builder.field(adapter.field(f))
            }
          case None =>
            val refType = f.refType match {
              case t: SignatureType.OptionLikeEnum => SignatureType.Tuple(Seq(SignatureType.Boolean, t.someType))
              case t => t
            }
            if (refType.isVariableLayoutType) {
              val IReg(ti) = typeInfos(i)
              builder.constIndexGeneric(f.idx.toInt, refType.toCbc, ti)
            } else {
              builder.constIndex(f.idx.toInt, refType.toCbc)
            }
        }
      }

      // TODO remove
      def store(value: Node): Unit = (value match {
        case _: AnyNull       => builder.storeImm(0)
        case IntegralConst(c) => builder.storeImm(c)
        case FConst(c)        => builder.storeImm(java.lang.Float.floatToRawIntBits(c))
        case DConst(c)        => builder.storeImm(java.lang.Double.doubleToRawLongBits(c))
        case Reg(r)           => builder.store(r)
      }).gen(fasm)

      def maybeImmValue(value: Node): Option[Long] = value match {
        case _: AnyNull => Some(0L)
        case IntegralConst(c) => Some(c)
        case FConst(c) => Some(java.lang.Float.floatToRawIntBits(c))
        case DConst(c) => Some(java.lang.Double.doubleToRawLongBits(c))
        case Reg(r) => None
      }

      def constrFieldRef(frs: Seq[CangjieFieldReference]): CbcFileFormat.FieldReference = {
        frs.map(adapter.field) match {
          case Seq(field) => field
          case refs => MultiFieldReference(refs.length, refs)
        }
      }

      n match {
        case n: GetFieldSeqRef if !n.isInstanceOf[HasFrameSlot] =>
          val IReg(dst) = n
          val base = getBaseLocation(n.obj)
          fasm.lea(dst, base, constrFieldRef(fieldRefs))
        case n: LoadFieldSeq if !n.isInstanceOf[HasFrameSlot] =>
          val Reg(dst) = n
          val base = getBaseLocation(n.obj)
          fasm.ld(dst, base, constrFieldRef(fieldRefs))
        case n: (GetFieldSeqRef | LoadFieldSeq) =>
          val Reg(dst) = n
          memExprHead(n.obj)
          fields(fieldRefs)
          builder.load(dst).gen(fasm)
        case n: GetFieldSeqRefGeneric =>
          val Reg(dst) = n
          memExprHead(n.obj)
          fields(fieldRefs, n.typeInfos)
          builder.load(dst).gen(fasm)
        case n: LoadFieldSeqGeneric =>
          val Reg(dst) = n
          memExprHead(n.obj)
          fields(fieldRefs, n.typeInfos)
          if (n.resType.isVariableSizeType) {
            val IReg(ti) = n.typeInfos.last
            builder.loadGeneric(dst.asInstanceOf[IR], ti).gen(fasm)
          } else {
            builder.load(dst).gen(fasm)
          }
          addXSite(n)
          saveGCState(n)
        case n: GetStaticFieldSeqRef =>
          val Reg(dst) = n
          assert(fieldRefs.size == 1 || !fieldRefs.head.fieldType.isTraceableReference, fieldRefs)
          builder.static(adapter.field(fieldRefs.head))
          fields(fieldRefs.tail)
          builder.load(dst).gen(fasm)
        case n: LoadStaticFieldSeq =>
          val Reg(dst) = n
          assert(fieldRefs.size == 1 || !fieldRefs.head.fieldType.isTraceableReference, fieldRefs)
          fasm.ld(dst, constrFieldRef(fieldRefs))
        case n: StoreFieldSeq if !n.isInstanceOf[HasFrameSlot] =>
          addXSite(n)
          maybeImmValue(n.inValue) match {
            case Some(_) => shouldNotReachHere("Field seq stores with imm are not supported")
            case None =>
              val Reg(src) = n.inValue
              val base = getBaseLocation(n.obj)
              fasm.st(src, base, constrFieldRef(fieldRefs))
          }
        case n: StoreFieldSeq =>
          memExprHead(n.obj)
          fields(fieldRefs)
          store(n.inValue)
        case n: StoreFieldSeqGeneric =>
          addXSite(n)
          memExprHead(n.obj)
          fields(fieldRefs, n.typeInfos)
          if (n.resType.isVariableSizeType) {
            val IReg(ti) = n.typeInfos.last
            val IReg(src) = n.inValue
            builder.storeGeneric(src, ti).gen(fasm)
          } else {
            store(n.inValue)
          }
        case n: StoreStaticFieldSeq =>
          addXSite(n)
          assert(fieldRefs.size == 1 || !fieldRefs.head.fieldType.isTraceableReference, fieldRefs)
          maybeImmValue(n.inValue) match {
            case Some(_) => shouldNotReachHere("Field seq stores with imm are not supported")
            case None =>
              val Reg(src) = n.inValue
              fasm.st(src, constrFieldRef(fieldRefs))
          }
      }
    }

    private def createFieldRef(f: Field | BitcodeFieldReference): FieldReference = {
      FieldRef.createFieldRef(f, None, None)
    }

    private def createFieldRef(n: UniversalGeneric.FieldOperation): FieldReference = {
      FieldRef.createFieldRef(n.field, Some(n.instantiatedRefType), Some(n.instantiatedFieldType))
    }

    @deprecated(message = "cjvm specific")
    private def genCopyResultVST(n: UniversalGeneric.CopyResultVST): Unit = {
      val (IReg(IR1), IReg(rv), IReg(rr), ftcIdx) = (n, n.value, n.resultPointerAddress, CodeSigSymbol(n.sig) ensuring (_.containsTypeVariables))
      asm.copyResultVST(rv, rr, ftcIdx)
    }

    @deprecated(message = "cjvm specific")
    private def genOHMSPtr(n: UniversalGeneric.OffHeapMemorySlotPointer): Unit = (n, n.ohms) match {
      case (IReg(dst), sa: HasFrameSlot) =>
        asm.ohmsPtr(dst, sa.slot.asInstanceOf[OHMSlotCBC].ohmSlot)
    }

    @deprecated(message = "cjvm specific")
    private def genTypeVarIsRef(n: UniversalGeneric.TypeVarIsRef): Unit = {
      val IReg(dst) = n
      val ftcIdx = CodeSigSymbol(n.typeVar)
      asm.doTypeVarIsRef(dst, ftcIdx)
    }

    @deprecated(message = "cjvm specific")
    private def genCopyUniversalVariable(n: UniversalGeneric.CopyUniversalVariable): Unit = {
      (n, n.dst, n.src) match {
        case (IReg(dst), sa: HasFrameSlot, IReg(src)) =>
          val dstExpr = MemExpr(dst, Array[Symbol](sa.slot.asInstanceOf[OHMSlotCBC].ohmSlot), isGeneric = true)
          val srcExpr = MemExpr(src, Array(CodeSigSymbol(n.variableType)), isGeneric = true)
          oldIsa.mov(dstExpr, srcExpr)
      }
    }

    @deprecated(message = "cjvm specific")
    private def genFieldChainWrite(n: FieldChainWrite): Unit = {
      addXSite(n)
      val isGeneric = n.fields.head.isGeneric
      val body = n.fields.map(_.asInstanceOf[Symbol])
      storeToMemExpr(MemExpr(memExprHead(n.obj), body, isGeneric), n.inValue)
    }

    private def storeToMemExpr(memExpr: MemExpr, value: Node): Unit = value match {
      case _: AnyNull => asm.movi64(memExpr, 0)
      case IConst(c)  => asm.movi32(memExpr, c)
      case LConst(c)  => asm.movi64(memExpr, c)
      case FConst(c)  => asm.movi32(memExpr, java.lang.Float.floatToRawIntBits(c))
      case DConst(c)  => asm.movi64(memExpr, java.lang.Double.doubleToRawLongBits(c))
      case Reg(r)     => asm.mov(memExpr, r)
    }

    @deprecated(message = "cjvm specific")
    private def genGetStatic(n: GetStatic): Unit = {
      val Reg(dst) = n
      asm.mov(dst, MemExpr(MemExpr.Head.StaticField, Array(createFieldRef(n.field))))
    }

    @deprecated(message = "cjvm specific")
    private def genPutStatic(n: PutStatic): Unit = {
      assert(!n.field.getType.isRecord)
      val Reg(src) = n.inValue0
      asm.mov(MemExpr(MemExpr.Head.StaticField, Array(createFieldRef(n.field))), src)
    }

    @deprecated(message = "cjvm specific")
    private def genBitcodeDeferredFieldOp(n: BitcodeDeferred.FieldOp): Unit = {
      val field = n.fieldRef
      val fieldType = field.fieldType

      assert(field.refType.isDeferred || field.refType.hasDeferredSuper)
      addXSite(n)

      (field.isWrite, field.isStatic) match {
        case (true,  true) => n.inValue match {
          case Reg(src) => assert(!fieldType.isRecord); asm.mov(MemExpr(MemExpr.Head.StaticField, Array(createFieldRef(field))), src)
        }

        case (false, true) => n match {
          case Reg(dst) => asm.mov(dst, MemExpr(MemExpr.Head.StaticField, Array(createFieldRef(field))))
        }

        case (false, false) => (n, n.obj) match {
          case (Reg(dst), IReg(receiver)) => asm.mov(dst, MemExpr(receiver, Array(createFieldRef(field))))
        }

        case (true, false) => (n.inValue, n.obj) match {
          case (Reg(src), IReg(receiver)) => asm.mov(MemExpr(receiver, Array(createFieldRef(field))), src)
        }
      }
    }

    private def genBFX(bfx: BitFieldExtract): Unit = if (!Isa12Mode) {
      import BitFieldExtract.*

      val from = iReg(bfx.arg)
      val to = iReg(bfx)

      def regIf(condition: Boolean)(action: (IR, IR) => Unit)(res: IR, arg: IR): IR = {
        if (condition) {
          action(res, arg)
          res
        } else {
          arg
        }
      }

      def intBFX(action: IR => IR): Unit = {
        val arg = regIf(bfx.argType == LongType)((dst: IR, src: IR) => asm.convert(I32, I64, dst, src))(to, from) // L -> I || L -> UI
        val res = action(arg)
        if (bfx.tpe == LongType) { // I/L -> UL || I/L -> L
          if (bfx.signExtension) {
            asm.convert(I64, I32, to, res)
          } else {
            asm.convert(I64, U32, to, res)
          }
        }
      }

      bfx match {
        case BFX(0,  8, false, _) => intBFX { arg => asm.convert(U8, U32, to, arg);  to }
        case BFX(0,  8, true,  _) => intBFX { arg => asm.convert(I8, I32, to, arg);  to }
        case BFX(0, 16, false, _) => intBFX { arg => asm.convert(U16, U32, to, arg); to }
        case BFX(0, 16, true,  _) => intBFX { arg => asm.convert(I16, I32, to, arg); to }
        case BFX(0, 32, _,     _) => intBFX { arg => arg }

        case BFX(offset, size, sx, _) if bfx.argType == IntType => intBFX { arg =>
          val leftBits = W32.nbits - (offset + size)
          assert(leftBits >= 0)
          val shifted = regIf(leftBits > 0)(asm.lsli(W32, _, _, leftBits))(to, arg)
          if (!sx) {
            asm.lsri(W32, to, shifted, leftBits + offset)
          } else {
            asm.asri(W32, to, shifted, leftBits + offset)
          }
          to
        }

        case BFX(offset, size, sx, _) if bfx.argType == LongType =>
          val leftBits = W64.nbits - (offset + size)
          assert(leftBits >= 0)
          val shifted = regIf(leftBits > 0)(asm.lsli(W64, _, _, leftBits))(to, from)
          if (!sx) {
            asm.lsri(W64, to, shifted, leftBits + offset)
          } else {
            asm.asri(W64, to, shifted, leftBits + offset)
          }

          if (bfx.tpe == IntType) {
            asm.convert(I32, I64, to, to)
          }
      }

    } else {
      val BitFieldExtract.BFX(offset, size, signExtend, arg) = bfx
      val IReg(dst) = bfx
      val IReg(src) = arg
      val resW = widthOf(bfx.tpe)
      val argW = widthOf(arg.tpe)
      isa12Asm.bfx(dst, src, resW, argW, bfx.signExtension, bfx.offset, bfx.size)
    }

    private def genNeg(x: Neg): Unit = {
      val width = widthOf(x)
      (x, x.arg) match {
        case (IReg(ird), IReg(irs)) => assert(!x.isFP); asm.neg(ird, irs, width)
        case (FReg(frd), FReg(frs)) => assert(x.isFP); asm.fneg(frd, frs, width)
      }
    }

    private def genMathIntrinsic(x: MathIntrinsic): Unit = {
      import Java.Lang.MathIntrinsic.*
      val width = widthOf(x)
      (x.kind, x, x.arg) match {
        case (F_ABS | D_ABS,   FReg(dst), FReg(src)) => asm.fabs(dst, src, width)
        case (F_SQRT | D_SQRT, FReg(dst), FReg(src)) => asm.fsqrt(dst, src, width)
      }
    }

    private def genArrLen(arrLen: ArrayLength): Unit = {
      addXSite(arrLen)

      val (IReg(len), IReg(arr)) = (arrLen, arrLen.array)
      arrLen match {
        case _: JavaArrayLength => asm.javaLenarr(len, arr)
        case _: CangjieArrayLength => asm.lenarr(len, arr)
      }
    }

    private def genArrayGet(arrGet: ArrayGet): Unit = {
      addXSite(arrGet)

      val arrayType = arrGet.arrayType
      val elemType = arrayType.getArrayElemType
      val asmType = elemType.toAsm
      val dst = if asmType.isFloatingPoint && asmType != AsmType.F16 then fReg(arrGet) else iReg(arrGet)
      (arrGet.array, arrGet.idx) match {
        case (IReg(arr), IReg(idx)) =>
          if (arrayType.isJavaArray) {
            asm.javaLdarr(asmType, dst, arr, idx)
          } else {
            if (elemType.isRecord) {
              val arrayOrElemSig = if (isStandalone) elemType else arrayType
              asm.ldarrRecord(dst.asInstanceOf[IR], arr, idx, CodeSigSymbol(arrayOrElemSig))
            } else if (elemType.isTraceableReference) {
              asm.ldarrObj(dst, arr, idx)
            } else {
              asm.ldarr(asmType, dst, arr, idx)
            }
          }
      }
    }

    private def genArrayPut(arrPut: ArrayPut): Unit = (arrPut.inValue0, arrPut.array, arrPut.idx) match {
      case (Reg(value), IReg(arr), IReg(idx)) =>
        addXSite(arrPut)

        val arrayType = arrPut.arrayType
        val elemType = arrayType.getArrayElemType

        if (arrayType.isJavaArray) {
          asm.javaStarr(elemType.toAsm, arr, idx, value)
        } else {
          assert(!elemType.isRecord)
          if (elemType.isTraceableReference) {
            asm.starrObj(arr, idx, value)
          } else {
            asm.starr(elemType.toAsm, arr, idx, value)
          }
        }
    }

    private def genArrayFill(arrayFill: ArrayFill): Unit = {
      val IReg(arr) = arrayFill.array
      asm.arrFill(arr, getConstBytes(arrayFill.totalBytes, arrayFill.elemType, arrayFill.inValues0))
    }

    private def genNewArr(newArr: NewArray): Unit =
      genNewArrImpl(newArr, newArr.allocType, newArr.lengths, newArr.uninitialized)

    private def genBitcodeDeferredNewArr(newArr: BitcodeDeferred.NewArray): Unit =
      // TODO: pass true if cangjieZeroValue is used and determine a suitable allocator in run-time based on actual field types
      genNewArrImpl(newArr, newArr.allocType, newArr.lengths, false)

    private def genNewArrImpl(newArr: Node, allocType: SignatureType, lengths: Seq[Node], zeroValue: Boolean): Unit = {
      assert(lengths.size == 1)
      assert(iReg(lengths.head) == IR2)
      assert(iReg(newArr) == IR1)

      val ftcSigIdx = CodeSigSymbol(allocType)
      if (allocType.isJavaArray) {
        asm.javaNewarr(ftcSigIdx)
      } else {
        assert(allocType.isCangjieArray)
        if (zeroValue) {
          asm.newarrzv(ftcSigIdx)
        } else if (ftcSigIdx.containsTypeVariables) {
          asm.newarrVST(ftcSigIdx)
        } else {
          asm.newarr(ftcSigIdx)
        }
      }

      addXSite(newArr)
      saveGCState(newArr)
    }

    private def genNewArrFill(newArrFill: NewArrayFill): Unit = {
      newArrFill.length match {
        case IReg(len) =>
          newArrFill.value match {
            case NumericalConst(v) =>
              asm.newarrfillconst(iReg(newArrFill), len, v.longValue, CodeSigSymbol(newArrFill.allocType))
            case IReg(fillValue) =>
              asm.newarrfillnonconst(iReg(newArrFill), len, fillValue, CodeSigSymbol(newArrFill.allocType))
          }
      }
      addXSite(newArrFill)
    }

    private def genArrayIndexCheck(aic: ArrayIndexCheck): Unit = {
      (aic.array, aic.idx, aic.length) match {
        case (Null(), IReg(idx), IReg(len)) =>
          val arrayType = aic.arrayType
          if (arrayType.symType.isJavaArray) {
            asm.javaArrIC(idx, len)
          } else {
            assert(arrayType.isCangjieArray)
            asm.arrIC(idx, len)
          }
      }
      addXSite(aic)
    }

    private def genArrayStoreCheck(asc: ArrayStoreCheck): Unit = {
      (asc.array, asc.value) match {
        case (IReg(arr), IReg(value)) =>
          assert(asc.arrayType.symType.isJavaArray)
          asm.javaArrSC(arr, value)
      }
      addXSite(asc)
    }

    private def genPackageInit(init: PackageInit): Unit = {
      asm.packageInit(CodeSigSymbol(init.klass))
      addXSite(init)
    }

    private def genPackageInitCheck(init: PackageInitCheck): Unit = {
      if (!isStandalone) {
        asm.packageInitCheck(CodeSigSymbol(init.klass))
        addXSite(init)
      }
    }

    private def genClinit(clinit: Clinit): Unit = {
      val klass = clinit.klass
      assert(klass.isJavaReference)
      asm.javaClinit(CodeSigSymbol(klass))
      addXSite(clinit)
    }

    private def genAJString(dst: IR, s: AJString): Unit = {
      val buf = new ByteBuffer()
      val alignment = if (s.bstr) {
        for (ch <- s.str) {
          buf.putW8(ch & 0xff)
        }
        buf.putW8(0)
        1
      } else {
        for (ch <- s.str.unicodeIterator) {
          buf.putW16(ch & 0xffff)
        }
        buf.putW16(0)
        2
      }
      asm.loadConstDataAddr(dst, buf.toByteArray, alignment)
    }

    private def genLoadConstString(dst: IR, cs: ConstString): Unit = {
      val value = new ConstStringSymbol(cs.stringValue)
      require(cs.strType.isJavaReference)
      asm.javaLdaStr(dst, value)
    }

    private def genLoadStackAlloc(dst: IR, sa: HasFrameSlot): Unit = {
      sa.slot match {
        case slot: TypedFrameSlotCBC =>
          slot.tpe match {
            case sig: SignatureType.TypeVariable =>
              val fasm = asm.asInstanceOf[ForkedISA12Assembler]
              val builder = MemSpace.Builder()
              builder.typed(slot.typedSlot)
              builder.constIndex(0, SignatureType.Tuple(Seq(ReferenceType.cangjieStdCoreObject.sigType)).toCbc)
              builder.load(dst).gen(fasm)
            case sig =>
              if (sig.isRecord) {
                asm.ldstackrec(dst, slot.typedSlot)
              } else {
                asm.ldstackobj(dst, slot.typedSlot)
              }
          }
        case slot: FrameSlotCBC =>
          asm.lea_us(dst, slot.untypedSlot)
      }
    }

    private def genCopyStructure(c: CopyStructure): Unit = {
      addXSite(c)

      val fasm = asm.asInstanceOf[ForkedISA12Assembler]
      val adapter = fasm.adapter
      val builder = MemSpace.Builder()

      def head(n: Node, fields: Seq[CangjieFieldReference]): Unit = n match {
        case stack: HasFrameSlot => stack.slot match {
          case slot: TypedFrameSlotCBC => builder.typed(slot.typedSlot)
          case _ => shouldNotReachHere(stack)
        }
        case DerivedPtr(IReg(base), IReg(derived)) =>
          builder.handle(base, derived)
        case n @ ArrayGet(_, _, IReg(obj), IReg(idx)) =>
          assert(n.arrayType.isRecordArray)
          builder.obj(obj)
            .index(idx, n.arrayType.getArrayElemType.toCbc, checked = false)
        case IReg(r) =>
          if (fields.head.refType.isTraceableReference) {
            builder.obj(r)
          } else {
            builder.rec(r)
          }
      }

      def fields(fields: Seq[CangjieFieldReference], typeInfos: Seq[Node] = Seq.empty): Unit = {
        for ((f, i) <- fields.zipWithIndex) f.field match {
          case Some(field) =>
            builder.field(adapter.field(f))
          case None =>
            val refType = f.refType match {
              case t: SignatureType.OptionLikeEnum => SignatureType.Tuple(Seq(SignatureType.Boolean, t.someType))
              case t => t
            }
            builder.constIndex(f.idx.toInt, refType.toCbc)
        }
      }

      (c.dst, c.src) match {

        case (IReg(dst), IReg(src)) =>
          assert(check(src, LocalType.CLEARED))
          builder.rec(src).copyRegTo(dst, adapter.sigType(CodeSigSymbol(c.structureType))).gen(fasm)
          mark(dst, LocalType.CLEARED)
        case (IReg(dst), n) => {
          val obj = n match {
            case g: GetFieldSeqRef =>
              head(g.obj, g.fields)
              fields(g.fields)
            case g: GetStaticFieldSeqRef =>
              builder.static(adapter.field(g.fields.head))
              fields(g.fields.tail)
            case n => head(n, Seq.empty)
          }
          builder.copyRegTo(dst, adapter.sigType(CodeSigSymbol(c.structureType))).gen(fasm)
          mark(dst, LocalType.CLEARED)
        }
        case (n, IReg(src)) => {
          assert(check(src, LocalType.CLEARED))
          val obj: Unit = n match {
            case g: GetFieldSeqRef =>
              head(g.obj, g.fields)
              fields(g.fields)
            case g: GetStaticFieldSeqRef =>
              builder.static(adapter.field(g.fields.head))
              fields(g.fields.tail)
            case n => head(n, Seq.empty)
          }
          builder.copyRegFrom(src, adapter.sigType(CodeSigSymbol(c.structureType))).gen(fasm)
        }
      }
    }

    private def genCopyStructureCBC(c: CopyStructureCBC): Unit = {
      val dstBody: MemExpr.Body = if (c.dstFields.nonEmpty) c.dstFields.asInstanceOf[Array[Symbol]] else CbcTypeKind.REC
      val dst = MemExpr(memExprHead(c.dst), dstBody)
      val srcBody: MemExpr.Body = if (c.srcFields.nonEmpty) c.srcFields.asInstanceOf[Array[Symbol]] else CbcTypeKind.REC
      val src = MemExpr(memExprHead(c.src), srcBody)
      if (Isa12Mode) {
        isa12Asm.copyRec(dst, src, CodeSigSymbol(c.structureType))
      } else {
        oldIsa.mov(dst, src)
      }
    }

    private def genInitObj(obj: InitObj): Unit = obj.slot match {
        case slot: TypedFrameSlotCBC =>
          assert(!slot.tpe.isRecord)
          asm.initobj(slot.typedSlot)
    }

    private def genEvacuate(evacuate: Evacuate): Unit = {
      assert(iReg(evacuate) == IR1)
      assert(iReg(evacuate.obj) == IR1)
      asm.evacuate()
    }

    private def genSingletonObject(singleton: SingletonObject): Unit = {
      val IReg(dst) = singleton
      asm.singleton(dst, CodeSigSymbol(singleton.allocType))
    }

    private def genEndLocalUnmovable(endLocalUnmovable: EndLocalUnmovable): Unit = {
      val IReg(obj) = endLocalUnmovable.obj
      assert(check(obj, LocalType.UNMOVABLE_REFERENCE))
      asm.endLocalUnmovable(obj)
      mark(obj, LocalType.REFERENCE)
    }

    private def genInstanceOf(iof: Node): Unit = {
      val AnyInstanceOf(tpe, obj) = iof
      val IReg(dstReg) = iof
      val IReg(objReg) = obj

      if (isStandalone) {
        val fasm = asm.asInstanceOf[ForkedISA12Assembler]
        val realTpe = tpe match {
          case SignatureType.Box(t) => t
          case t => t
        }
        fasm.isInstanceOf(dstReg, objReg, realTpe.toCbc)
      } else {
        if (tpe.isClass) {
          assert(iof.isInstanceOf[ControlledInstanceOf], s"all class InstanceOf must be lowered to ControlledInstanceOf: $iof")
          asm.isInstanceOfClass(dstReg, objReg, CodeSigSymbol(tpe))
          return
        }

        if (tpe.isArray) {
          assert(tpe.isJavaArray)
          asm.isInstanceOfArray(dstReg, objReg, CodeSigSymbol(tpe))
          return
        }

        require(tpe.isInterface)
        asm.isInstanceOfInterface(dstReg, objReg, CodeSigSymbol(tpe))
      }
    }

    private def genCheckCast(checkCast: CheckCast): Unit = {
      val IReg(dst) = checkCast
      val IReg(src) = checkCast.obj
      genCheckCast(dst, src, checkCast.targetType)
      addXSite(checkCast)
    }

    private def genCheckCast(checkCast: BitcodeDeferred.CheckCast): Unit = {
      val IReg(dst) = checkCast
      val IReg(src) = checkCast.obj
      genCheckCast(dst, src, checkCast.targetType)
      addXSite(checkCast)
    }

    private def genCheckCast(dst: IR, src: IR, tpe: SignatureType): Unit = {
      assert(tpe.isJavaReference)
      asm.javaCheckCast(dst, src, CodeSigSymbol(tpe))
    }

    /** Obtains stack slot accessed by [[LoadStoreMemoryAccess]] operation and its type kind. */
    private def getSlotForStackAllocLoadStore(access: LoadStoreMemoryAccess): (StackSlot.Untyped, CbcTypeKind) = {
      val (slot, slotSigType) = access.addr match {
        case sa @ StackAlloc.Local(t) => (sa.slot, t)
        case sa @ StackAlloc.OffHeapMemory(t) => (sa.slot, t)
        case sa @ StackAlloc.DebugVar(t, _) => (sa.slot, t)
      }
      assert(!slot.isInstanceOf[TypedFrameSlotCBC])

      val asmType = access.accessType
      // We need to refine type kind to `REF` for cases when stack alloc slot holds traceable reference, e.g. `DebugVar`
      val slotTypeKind = if slotSigType.symType.isTraceableReference then CbcTypeKind.REF else CbcTypeKind(asmType)

      if (!env.enabled(StackAllocSlotsAccessTypeMayDifferFromSlotType)) {
        val slotType = slotSigType.toAsm
        // Otherwise we should enable optimization combining LoadMemory & BFX.
        assert(asmType == slotType ||
          (asmType != PTR && slotType != PTR && asmType.width == slotType.width))
      }

      (slot.asInstanceOf[FrameSlotCBC].untypedSlot, slotTypeKind)
    }

    private def genLoadMemory(load: LoadMemory): Unit = {
      val Reg(dst) = load

      load.addr match {
        case IReg(src) =>
          if (isStandalone) {
            fasm.loadRawMemory(dst, src, LoadAccessKind.from(cbcTypeKind(load.tpe)), 0)
          } else {
            assert(!load.signature.isTraceableReference)
            asm.mov(dst, MemExpr(src, CbcTypeKind(load.accessType)))
          }

        case sa @ StackAlloc.Local(t) if isStandalone =>
          sa.slot match {
            case slot: TypedFrameSlotCBC =>
              val fasm = asm.asInstanceOf[ForkedISA12Assembler]
              val builder = MemSpace.Builder()
              builder.typed(slot.typedSlot)
              builder.constIndex(0, SignatureType.Tuple(Seq(ReferenceType.cangjieStdCoreObject.sigType)).toCbc)
              builder.load(dst).gen(fasm)
            case slot: FrameSlotCBC =>
              val fasm = asm.asInstanceOf[ForkedISA12Assembler]
              assert(t.isTraceableReference || t.isPrimitive)
              fasm.loadUntyped(dst, LoadAccessKind.from(cbcTypeKind(load.tpe)), slot.untypedSlot)
          }

        case sa: StackAlloc =>
          assert(!isStandalone)
          val (src, typeKind) = getSlotForStackAllocLoadStore(load)
          asm.loadUntyped(dst, typeKind, src)
      }
    }

    override protected def genLoadTailParam(ltp: LoadTailParam): Unit = (ltp, ltp.tpe) match {
      case (LoadTailParam(IReg(tailReg), offset), tpe) =>
        val Reg(dst) = ltp
        val ldk = LoadAccessKind.from(cbcTypeKind(tpe))
        fasm.loadRawMemory(dst, tailReg, ldk, offset)
    }

    private def genStoreMemory(store: StoreMemory): Unit = {
      (store.addr, store.inValue0) match {
        case (IReg(dst), Reg(src)) if isStandalone =>
          val fasm = asm.asInstanceOf[ForkedISA12Assembler]
          (store.signature, src) match {
            case (_: SignatureType.Box, _) =>
              assert(!src.isIReg || check(src.asIReg, localTypeOf(store.inValue0)))
              val builder = MemSpace.Builder()
              builder.rec(dst)
              builder.constIndex(0, SignatureType.Tuple(Seq(ReferenceType.cangjieStdCoreObject.sigType)).toCbc)
              builder.store(src).gen(fasm)
            case (_, src: (IR | FR)) =>
              fasm.storeRawMemory(src, dst, StoreAccessKind.from(CbcTypeKind(store.accessType)), 0)
          }

        case (IReg(dst), Reg(src)) =>
          assert(!store.signature.isTraceableReference)
          assert(!src.isIReg || check(src.asIReg, localTypeOf(store.inValue0)))
          asm.mov(MemExpr(dst, CbcTypeKind(store.accessType)), src)
          mark(dst, LocalType.CLEARED)

        case (sa: StackAlloc, Reg(src)) =>
          assert(!src.isIReg || check(src.asIReg, localTypeOf(store.inValue0)))
          val (dst, typeKind) = getSlotForStackAllocLoadStore(store)
          asm.storeUntyped(src, typeKind, dst)
          mark(sa.slot.asInstanceOf[FrameSlotCBC].local, if (store.signature.isTraceableReference) LocalType.REFERENCE else LocalType.CLEARED)
      }
    }

    private def genInitStringRecord(x: InitStringRecord): Unit = {
      val stringSymbol = new ConstStringSymbol(x.str.value)
      val sa = valueOf(x.obj).producer.asInstanceOf[StackAlloc]

      if (Isa12Mode) {
        isa12Asm.initConstString(sa.slot.asInstanceOf[TypedFrameSlotCBC].typedSlot, stringSymbol)
      } else {
        oldIsa.initConstString(MemExpr(memExprHead(sa), CbcTypeKind.REC), stringSymbol)
      }
    }

    @deprecated(message = "cjvm specific")
    private def genEnrich(enrich: EnrichOperation): Unit = {
      val IReg(dst) = enrich
      val IReg(obj) = enrich.obj

      enrich.enrichment match {
        case IReg(enrichment) => asm.eopPack(dst, obj, enrichment)
        case IntegralConst(_) => shouldNotReachHere("Const enrichment is not supported in CBC")
      }
    }

    @deprecated(message = "cjvm specific")
    private def genEnrichCBC(enrich: EnrichCBC): Unit = {
      val IReg(dst) = enrich
      val IReg(obj) = enrich.obj

      asm.eopPack(dst, obj, CodeSigSymbol(enrich.rcvType), CodeSigSymbol(enrich.interfaceType))
    }

    @deprecated(message = "cjvm specific")
    private def genDeprive(deprive: DepriveOperation): Unit = {
      val IReg(dst) = deprive
      val IReg(obj) = deprive.obj
      asm.eopPlain(dst, obj)
    }

    override protected def genDeprive(dst: IREG, src: IREG): Unit = notImplemented(CBC)

    override protected def mergeRichPointer(dst: IREG, imt: IREG, ptr: IREG): Unit = notImplemented(CBC)

    @deprecated(message = "cjvm specific")
    private def genExtractEnrichment(extractEnrichment: ExtractEnrichment): Unit = {
      val IReg(dst) = extractEnrichment
      val IReg(obj) = extractEnrichment.obj
      asm.eopEnrichment(dst, obj)
    }

    @deprecated(message = "cjvm specific")
    private def genInterfaceCast(iCast: InterfaceCastCBC): Unit = {
      val IReg(dst) = iCast
      val IReg(obj) = iCast.obj
      asm.weakCast(dst, obj, CodeSigSymbol(iCast.targetType))
      addXSite(iCast)
    }

    override protected def genThrow(throwNode: Throw): Unit = throwNode.inValue match {
      case IReg(ex) =>
        ensureFullFrame()
        asm.throwEx(ex)

      case _ => shouldNotReachHere()
    }

    private def genCatch(catchNode: CatchCBC): Unit =
      val IReg(dst) = catchNode
      asm.catchEx(dst)

    override protected def genNullCheckImpl(nullCheck: AbstractNullCheck): Unit = {
      val IReg(obj) = nullCheck.obj
      asm.nullcheck(obj)
    }

    @deprecated(message = "cjvm specific")
    private def genMutFuncCombine(combine: MutFunc.Combine): Unit = {
      val IReg(dst) = combine
      asm.combineHostAndOffset(dst, MemExpr(memExprHead(combine), CbcTypeKind.REC))
    }

    override protected def beforeCallActions(call: Call): Unit = {
      for (mut <- call.attachedByReason(Group.AttachReason.MUT_FUNC_ARG)) {
        val mutRecordReg = call.abi.paramLocations(call.methodType.getMutRecordArgIdx).asInstanceOf[IR]
        val mutObjectReg = call.abi.paramLocations(call.methodType.getMutObjectArgIdx).asInstanceOf[IR]
        mut match {
          case offset: MutFunc.Offset =>
            offset.record match {
              case sa: StackAlloc if call.targetRef.hasMethod && call.targetRef.method.isRecordConstructor =>
                val slot = sa.slot.asInstanceOf[TypedFrameSlotCBC]
                assert(slot.tpe.isRecord, s"$sa")
                assert(mutRecordReg == IR1)
                asm.prepareRecord(slot.typedSlot)
                asm.offsetFromHost(mutObjectReg, mutRecordReg, IR1)
              case IReg(record) =>
                asm.offsetFromHost(mutObjectReg, mutRecordReg, record)
            }
          case offset: MutFunc.OffsetCBC =>
            val body: MemExpr.Body = if (offset.fields.nonEmpty) offset.fields else CbcTypeKind.REC
            asm.offsetFromHost(mutObjectReg, mutRecordReg, MemExpr(memExprHead(offset.host), body))
        }
        mark(mutObjectReg, LocalType.REFERENCE)
        mark(mutRecordReg, LocalType.CLEARED)
      }
    }

    override protected def initTailRegister(call: Call): Unit = {
      if (!isStandalone) {
        val firstTailParam = call.abi.paramLocations.collectFirst {
          case ABI.TailSlot(offset, _) => slotsForArguments(offset).asInstanceOf[ArgFrameSlotCBC].untypedSlot
        }.get

        asm.lea_us(tailRegister.asInstanceOf[IR], firstTailParam)
        mark(tailRegister, LocalType.CLEARED)
      }
    }

    override protected def genCallImpl(call: Call): Unit = {
      if (call.abi.isVarArgs) {
        notImplemented("calls with varargs in CBC (JET-13417)");
      }

      if (call.methodType.isCJForeign ||
          // Unmanaged methods from CompilerInterface may be CCall on concrete platform where CBC will be JIT-compiled.
          (call.targetRef.hasMethod && call.targetRef.method.getDeclaringClass.isCompilerInterface && !call.methodType.callConv.hasManagedExecEnv)) {
        mayHaveNativeCalls = true
      }

      val targetRef = call.targetRef

      addLivenessHints(call)

      def isa12ResultReg: IR = call match {
        case IReg(r) => r
        case _ => IR.IR1
      }

      call.target match {
        case InvokeInterfaceTarget(IReg(enrichmentReg)) =>
          assert(check(enrichmentReg, LocalType.CLEARED))
          if (Isa12Mode) {
            isa12Asm.callInterfRich(enrichmentReg, targetRef.getPermanent)
          } else {
            oldIsa.iCallPref(enrichmentReg)
          }

        case InvokeInterfaceTarget(LightInterfCastCBC(rcvType)) =>
          assert(Isa12Mode)
          isa12Asm.callInterf(isa12ResultReg, CodeSigSymbol(rcvType), targetRef.getPermanent)

        case InvokeInterfaceTarget(IntegralConst(enrichment)) =>
          assert(enrichment > 0 && isNBits(enrichment, 16))
          if (Isa12Mode) {
            // TODO-ISA12: eliminate `call.interf.const` and replace this branch with `assert(!Isa12Mode)`
            isa12Asm.callInterfConst(isa12ResultReg, low16Bits(enrichment.toInt), targetRef.getPermanent)
          } else {
            oldIsa.iCallPref(low16Bits(enrichment.toInt))
          }

        case BitcodeDeferred.InvokeTarget(targetRef) if targetRef.isInterfCall =>
          assert(targetRef.refClass.isInterface)
          if (Isa12Mode) {
            isa12Asm.callInterfPlain(isa12ResultReg, targetRef)
          } else {
            oldIsa.iCallPref(targetRef)
          }

        case _ =>
          assert(!targetRef.isInterfCall)
      }

      val realICallGenerated = Isa12Mode && targetRef.isInterfCall

      def genericCall(mr: InstantiatedMethodReference): Unit = {
        assert(mr.method.hasUniversalGenericContext)
        val m = mr.getPermanent
        if (mr.method.isUniversalGeneric) {
          val instantiatedMethod = InstantiatedGenericMethod(m, mr.instantiatedTypeParameters)
          if (instantiatedMethod.containsTypeVariables) {
            asm.callGFDFTC(instantiatedMethod, m) // FIXME-UG method reference should be enough
          } else {
            asm.callGFDSig(instantiatedMethod, m) // FIXME-UG method reference should be enough
          }
        } else if (mr.methodType.hasReceiverParameter) {
          if (Isa12Mode) {
            isa12Asm.callDirect(isa12ResultReg, m)
          } else {
            oldIsa.call(m)
          }
        } else {
          val refType = CodeSigSymbol(mr.refType.sigType)
          if (refType.containsTypeVariables) {
            asm.callGTDFTC(refType, m)
          } else {
            asm.callGTDSig(refType, m)
          }
        }
      }

      def directCall(): Unit = {
        val permanent = targetRef.getPermanent
        if (Isa12Mode) {
          isa12Asm.callDirect(isa12ResultReg, permanent)
        } else {
          oldIsa.call(permanent)
        }
      }

      def virtualStaticCall(): Unit = {
        val permanent = targetRef.getPermanent
        if (Isa12Mode) {
          shouldNotReachHere("unsupported")
        } else {
          oldIsa.callVirtStatic(permanent)
        }
      }

      def directCCall(method: Method): Unit = {
        val permanent = new MethodReference(method, targetRef.accessKind).getPermanent
        if (Isa12Mode) {
          isa12Asm.callDirect(isa12ResultReg, permanent)
        } else {
          oldIsa.call(permanent)
        }
      }

      def virtualCall(): Unit = {
        assert(check(call.abi.paramLocations(0).asIReg, LocalType.REFERENCE))
        val permanent = targetRef.getPermanent
        if (isStandalone) {
          val fasm = asm.asInstanceOf[ForkedISA12Assembler]
          if (targetRef.refType.symType.getName.startsWith("$C")) {
            fasm.callClosure(isa12ResultReg, targetRef.refType.sigType.toCbc)
          } else {
            val outerTI = call.invokeArgs(targetRef.methodType.getOuterTypeInfoArgIdx)
            val loc = outerTI match {
              case IReg(reg) => reg
              case UntypedSlot(slot, _) => slot
            }
            fasm.callInterfGeneric(loc, fasm.adapter.method(permanent))
          }
        } else {
          if (Isa12Mode) {
            isa12Asm.callVirt(isa12ResultReg, permanent)
          } else {
            oldIsa.callVirt(permanent)
          }
        }
      }

      if (!realICallGenerated) {
        call match {
          case UniversalGeneric.InvokeConstraintMethod(targetRef) =>
            asm.callConstraint(CodeSigSymbol(targetRef.receiverType), targetRef.getPermanent)
          case UniversalGeneric.InvokeMethodWithGenericContext(mr) => genericCall(mr)
          case VirtualStaticCall() => virtualStaticCall()
          case AnyDirectCall(_) => directCall()
          case DirectCall(method) if !targetRef.hasMethod => directCCall(method)
          case AnyVirtualCall() => virtualCall()
          case BitcodeDeferred.Invoke(targetRef) => if (targetRef.isDirectCall) directCall() else virtualCall()
          case _ =>
            val IReg(targetReg) = call.target
            asm.callIndirect(targetReg, call.methodType)
        }
      }
    }

    override protected def afterCallActions(call: Call): Unit = {
      if (!call.tpe.isFloatingPointType && call.tpe != VoidType && call.abi.resultLocation.isIReg) {
        mark(call.abi.resultLocation.asIReg, localTypeOf(call))
      }
      saveGCState(call)
      saveStateForStackChecks(call)
    }

    override protected def genTransferImpl(node: Transfer): Unit = {
      val arg = node.transferArg

      (node, arg) match {
        case (dst, src) if dst.resource == src.resource => // nop

        // TODO: remove this ugly workaround
        //       this mov should be done right before call without extra transfer node!
        case (_: SaveCallRefTypeInfo, IReg(src)) =>
          fasm.movAcc(src)
        case (_: SaveCallRefTypeInfo, sa @ StackAlloc.Local(t)) =>
          fasm.loadUntypedAcc(LoadAccessKind.SPECIAL, sa.slot.asInstanceOf[FrameSlotCBC].untypedSlot)

        case (IReg(dst), IReg(src)) if arg.tpe.isHolderType => arg.tpe.asInstanceOf[HolderType].instantiatedSig match {
          case _: TypeVariable if isStandalone =>
            asm.mov(dst, src, reference = true)
          case _: TypeVariable if Isa12Mode =>
            assert(rootMethod.hasUniversalGenericContext, "mov.vst can be generated only in universal generic context")
            isa12Asm.movVST(dst, src)
          case sig =>
            asm.mov(dst, src, reference = sig.isTraceableReference)
        }

        case (Reg(dst), Reg(src)) =>
          asm.mov(dst, src, reference = node.tpe.isTraceableRefType)

        case (IReg(dst), IConst(c)) =>
          asm.movi32(dst, c)

        case (IReg(dst), LConst(c)) =>
          asm.movi64(dst, c)

        case (IReg(dst), AddrConst(_, m: Method, 0)) if m.isCangjieForeign =>
          asm.lea_cforeign(dst, new MethodReference(m, MethodReferenceAccessKind.STATIC).getPermanent)

        case (FReg(dst), FConst(c)) =>
          asm.fmovi(dst, c, W32)

        case (FReg(dst), DConst(c)) =>
          asm.fmovi(dst, c, W64)

        case (IReg(dst), _: AnyNull) =>
          asm.mov(dst, IR.IRZ, reference = true)

        case (IReg(dst), as: AJString) =>
          genAJString(dst, as)

        case (IReg(dst), cs: ConstString) =>
          genLoadConstString(dst, cs)

        case (IReg(dst), sa @ StackAlloc.Local(t)) =>
          val fasm = asm.asInstanceOf[ForkedISA12Assembler]
          sa.slot match {
            case slot: TypedFrameSlotCBC =>
              slot.tpe match {
                case sig: SignatureType.TypeVariable =>
                  val fasm = asm.asInstanceOf[ForkedISA12Assembler]
                  val builder = MemSpace.Builder()
                  builder.typed(slot.typedSlot)
                  builder.constIndex(0, SignatureType.Tuple(Seq(ReferenceType.cangjieStdCoreObject.sigType)).toCbc)
                  builder.load(dst).gen(fasm)
                case sig =>
                  assert(sig.isRecord)
                  asm.ldstackrec(dst, slot.typedSlot)
              }
            case slot: FrameSlotCBC =>
              assert(t.isTraceableReference || t.isPrimitive)
              fasm.loadUntyped(dst, LoadAccessKind.SPECIAL, slot.untypedSlot)
          }

        case (IReg(dst), sa: HasFrameSlot) =>
          genLoadStackAlloc(dst, sa)

        case (IReg(dst), x: DerivedPtr.Local) =>
          asm.movbp(dst, local = true)

        case (IReg(dst), x: DerivedPtr.Global) =>
          asm.movbp(dst, local = false)

        case (Reg(dst), UntypedSlot(src, _)) => arg.tpe match {
          case HolderType(tv: TypeVariable) =>
            asm.mov(dst, MemExpr(src, Array[Symbol](CodeSigSymbol(tv)), isGeneric = true))
          case tpe =>
            asm.loadUntyped(dst, cbcTypeKind(tpe), src)
        }
        case (UntypedSlot(dst, _), src) => (node.tpe, src) match {
          case (HolderType(tv: TypeVariable), Reg(src)) =>
            asm.mov(MemExpr(dst, Array[Symbol](CodeSigSymbol(tv)), isGeneric = true), src)
          case (tpe, Reg(src)) =>
            asm.storeUntyped(src, cbcTypeKind(tpe), dst)
          case (_, _: AnyNull) =>
            asm.storeUntypedImm(0, dst)
          case (_, IConst(c)) =>
            asm.storeUntypedImm(c, dst)
          case (_, LConst(c)) =>
            asm.storeUntypedImm(c, dst)
          case (_, FConst(c)) =>
            asm.storeUntypedImm(java.lang.Float.floatToRawIntBits(c), dst)
          case (_, DConst(c)) =>
            asm.storeUntypedImm(java.lang.Double.doubleToRawLongBits(c), dst)
        }
      }
    }

    private def addLivenessHints(node: Node): Unit = {
      if (needGCMap(node)) {
        gatherGCMapCBC(node)
      }
    }

    override protected def genNop(): Unit = asm.nop()

    private def genGCPoint(gcPoint: GCPoint): Unit = {
      addXSite(gcPoint)
      asm.gcpoint()
      saveGCState(gcPoint)
    }

    @deprecated(message = "cjvm specific")
    private def genConvertHolder(convert: UniversalGeneric.ConvertHolder): Unit = {
      if (isStandalone) {
        notImplemented("convert holder")
      } else {
        assert(!convert.concreteType.isReference || convert.arg.resource.isIReg)
        (convert, convert.arg) match {
          case (IReg(dst), _) if convert.concreteType.isZST =>
            assert(convert.isInstanceOf[UniversalGeneric.ToHolder], convert)
            // Target method ABI has Holder instead of Unit as param type.
            // So, value left on target register must be primitive.
            asm.movi64(dst, 0xDEAD1FEA)

          case (dst, src) if dst.resource == src.resource => // nop

          case (IReg(dst), IReg(src)) if convert.concreteType.isReference =>
            asm.mov(dst, src, reference = true)

          case (Reg(dst), Reg(src)) =>
            asm.mov(dst, src, reference = false)
        }
      }
    }

    private def genBox(n: Box): Unit = {
      val fasm = asm.asInstanceOf[ForkedISA12Assembler]
      val IReg(dst) = n
      n.value match {
        case sa: HasFrameSlot => fasm.box(sa.slot.asInstanceOf[TypedFrameSlotCBC].typedSlot, dst)
        case Reg(src)         => fasm.box(src, dst, n.base.toCbc)
        case _: Void          => fasm.box(IR.IRZ, dst, n.base.toCbc)
      }
      addXSite(n)
      saveGCState(n)
    }

    private def genUnbox(n: Unbox): Unit = {
      val fasm = asm.asInstanceOf[ForkedISA12Assembler]
      val IReg(src) = n.value
      n match {
        case Reg(dst) => fasm.unbox(dst, src, n.base.toCbc)
      }
    }

    private def genUnboxRec(n: UnboxRec): Unit = {
      val fasm = asm.asInstanceOf[ForkedISA12Assembler]
      val IReg(src) = n.value
      fasm.unbox(n.slot.asInstanceOf[TypedFrameSlotCBC].typedSlot, src)
    }

    private def genSpawnFuture(n: SpawnFuture): Unit = {
      val fasm = asm.asInstanceOf[ForkedISA12Assembler]
      val IReg(futureReg) = n.future
      fasm.spawnFuture(futureReg, n.retType.toCbc)
      addXSite(n)
      saveGCState(n)
    }

    private def genSpawnClosure(n: SpawnClosure): Unit = {
      val fasm = asm.asInstanceOf[ForkedISA12Assembler]
      val IReg(closureReg) = n.closure
      fasm.spawn(closureReg, n.closureType.toCbc)
      addXSite(n)
      saveGCState(n)
    }

    private def genOptionTagGeneric(n: OptionTagGeneric): Unit = {
      val fasm = asm.asInstanceOf[ForkedISA12Assembler]
      val IReg(dst) = n
      val IReg(src) = n.value
      val IReg(bti) = n.baseTypeInfo
      fasm.tagGeneric(dst, src, bti, n.optionType.toCbc)
    }

    private def genOptionPayloadGeneric(n: OptionPayloadGeneric): Unit = {
      val fasm = asm.asInstanceOf[ForkedISA12Assembler]
      val IReg(dst) = n
      val IReg(src) = n.value
      val IReg(bti) = n.baseTypeInfo
      val IReg(oti) = n.optionTypeInfo
      fasm.payloadGeneric(dst, src, bti, oti, n.optionType.toCbc)

      addXSite(n)
      saveGCState(n)
    }

    private def genNewNoneOptionGeneric(n: NewNoneOptionGeneric): Unit = {
      val fasm = asm.asInstanceOf[ForkedISA12Assembler]
      val IReg(dst) = n
      val IReg(bti) = n.baseTypeInfo
      val IReg(oti) = n.optionTypeInfo
      fasm.newNoneGeneric(dst, bti, oti, n.optionType.toCbc)

      addXSite(n)
      saveGCState(n)
    }

    private def genNewSomeOptionGeneric(n: NewSomeOptionGeneric): Unit = {
      val fasm = asm.asInstanceOf[ForkedISA12Assembler]
      val IReg(dst) = n
      val IReg(src) = n.value
      val IReg(bti) = n.baseTypeInfo
      val IReg(oti) = n.optionTypeInfo
      fasm.newSomeGeneric(dst, src, bti, oti, n.optionType.toCbc)

      addXSite(n)
      saveGCState(n)
    }

    private def genAssignGeneric(n: AssignGeneric): Unit = {
      val fasm = asm.asInstanceOf[ForkedISA12Assembler]
      val IReg(dst) = n.dst
      val IReg(src) = n.src
      val IReg(bti) = n.baseTypeInfo
      fasm.assignGeneric(dst, src, bti)

      addXSite(n)
      saveGCState(n)
    }

    private def genInstanceOfGeneric(n: InstanceOfGeneric): Unit = {
      val fasm = asm.asInstanceOf[ForkedISA12Assembler]
      val IReg(dst) = n
      val IReg(obj) = n.obj
      val IReg(bti) = n.targetTypeInfo
      fasm.instanceOfGeneric(dst, obj, bti)
    }

    private def genAtomic(n: AtomicOps.AtomicNode): Unit = {
      val fasm = asm.asInstanceOf[ForkedISA12Assembler]
      val adapter = fasm.adapter
      n match {
        case x: AtomicOps.Load =>
          val IReg(dst) = x
          val IReg(obj) = x.obj
          val field = adapter.field(x.field)
          fasm.atomicLoad(dst, obj, field)

        case x: AtomicOps.Store =>
          val IReg(src) = x.value
          val IReg(obj) = x.obj
          val field = adapter.field(x.field)
          fasm.atomicStore(src, obj, field)

        case x: AtomicOps.CAS =>
          val IReg(dst) = x
          val IReg(obj) = x.obj
          val IReg(src1) = x.compareValue
          val IReg(src2) = x.swapValue
          val field = adapter.field(x.field)
          fasm.cas(dst, obj, src1, src2, field)

        case x: AtomicOps.Simple =>
          import AtomicOps.Simple.Kind as Kind
          val gen = x.kind match {
            case Kind.SWAP       => fasm.atomicSwap
            case Kind.FETCH_ADD  => fasm.atomicFetchAdd
            case Kind.FETCH_SUB  => fasm.atomicFetchSub
            case Kind.FETCH_AND  => fasm.atomicFetchAnd
            case Kind.FETCH_OR   => fasm.atomicFetchOr
            case Kind.FETCH_XOR  => fasm.atomicFetchXor
          }

          val IReg(dst) = x
          val IReg(src) = x.value
          val IReg(obj) = x.obj
          val field = adapter.field(x.field)

          gen(dst, obj, src, field)
      }
    }

    /** Generates assembler pattern for given `node`. */
    @nowarn("cat=deprecation")
    override protected def genNodeImpl(node: Node): Unit = {
      if (!trackLivenessDuringCodegen(node)) {
        addLivenessHints(node)
      }
      node match {
        case x: Cmp                        => genCmp(x)
        case x: Sub                        => genSub(x)
        case x: IDivRemOp                  => genIDivRemOp(x)
        case x: FDiv                       => genFDiv(x)
        case x: LogicalBinaryOp            => genLogical(x)
        case x: CheckedOp                  => genCheckedOp(x)
        case x: ArithCommutativeOp         => genArithCommutativeOp(x)
        case x: BinaryOp                   => shouldNotReachHere(s"unexpected BinaryOp: $x")

        case x: Shift                      => genShift(x)
        case x: Cast                       => genCast(x)
        case x: New                        => genNew(x)
        case x: BitcodeDeferred.New        => genBitcodeDeferredNew(x)
        case x: GetField                   => genGetField(x)
        case x: FieldChainRead             => genFieldChainRead(x)
        case x: PutField                   => genPutField(x)
        case x: FieldChainWrite            => genFieldChainWrite(x)
        case x: GetStatic                  => genGetStatic(x)
        case x: PutStatic                  => genPutStatic(x)
        case x: BitcodeDeferred.FieldOp    => genBitcodeDeferredFieldOp(x)
        case x: BitFieldExtract            => genBFX(x)
        case x: Neg                        => genNeg(x)
        case x: ArrayLength                => genArrLen(x)
        case x: ArrayGet                   => genArrayGet(x)
        case x: ArrayPut                   => genArrayPut(x)
        case x: ArrayFill                  => genArrayFill(x)
        case x: ArrayIndexCheck            => genArrayIndexCheck(x)
        case x: ArrayStoreCheck            => genArrayStoreCheck(x)
        case x: NewArray                   => genNewArr(x)
        case x: BitcodeDeferred.NewArray   => genBitcodeDeferredNewArr(x)
        case x: NewArrayFill               => genNewArrFill(x)
        case x: PackageInit                => genPackageInit(x)
        case x: PackageInitCheck           => genPackageInitCheck(x)
        case x: InstanceOf                 => genInstanceOf(x)
        case x: ControlledInstanceOf       => genInstanceOf(x)
        case x: BitcodeDeferred.InstanceOf => genInstanceOf(x)
        case x: CheckCast                  => genCheckCast(x)
        case x: BitcodeDeferred.CheckCast  => genCheckCast(x)
        case x: DivisorCheck               => genDivisorCheck(x)
        case x: GCPoint                    => genGCPoint(x)
        case x: MathIntrinsic              => genMathIntrinsic(x)
        case x: LoadMemory                 => genLoadMemory(x)
        case x: StoreMemory                => genStoreMemory(x)
        case x: EnrichOperation            => genEnrich(x)
        case x: EnrichCBC                  => genEnrichCBC(x)
        case x: DepriveOperation           => genDeprive(x)
        case x: ExtractEnrichment          => genExtractEnrichment(x)
        case x: InterfaceCastCBC           => genInterfaceCast(x)
        case x: StackZeroing               => genStackZeroing(x)
        case x: Clinit                     => genClinit(x)
        case x: CopyStructure              => genCopyStructure(x)
        case x: CopyStructureCBC           => genCopyStructureCBC(x)
        case x: InitObj                    => genInitObj(x)
        case x: Evacuate                   => genEvacuate(x)
        case x: SingletonObject            => genSingletonObject(x)
        case x: EndLocalUnmovable          => genEndLocalUnmovable(x)
        case x: CatchCBC                   => genCatch(x)
        case x: MutFunc.Combine            => genMutFuncCombine(x)

        case x: UniversalGeneric.ConvertHolder            => genConvertHolder(x)
        case x: UniversalGeneric.CopyUniversalVariable    => genCopyUniversalVariable(x)
        case x: UniversalGeneric.GetField                 => genGetField(x)
        case x: UniversalGeneric.GetFieldOHM              => genGetFieldOHM(x)
        case x: UniversalGeneric.PutField                 => genPutField(x)
        case x: UniversalGeneric.CopyResultVST            => genCopyResultVST(x)
        case x: UniversalGeneric.OffHeapMemorySlotPointer => genOHMSPtr(x)
        case x: UniversalGeneric.TypeVarIsRef             => genTypeVarIsRef(x)
        case x: UniversalGeneric.HolderConst =>
          val IReg(dst) = node
          asm.movi64(dst, 0)

        case x: Box => genBox(x)
        case x: Unbox => genUnbox(x)
        case x: UnboxRec => genUnboxRec(x)

        case x: SpawnFuture => genSpawnFuture(x)
        case x: SpawnClosure => genSpawnClosure(x)

        case x: OptionTagGeneric => genOptionTagGeneric(x)
        case x: OptionPayloadGeneric => genOptionPayloadGeneric(x)
        case x: NewNoneOptionGeneric => genNewNoneOptionGeneric(x)
        case x: NewSomeOptionGeneric => genNewSomeOptionGeneric(x)
        case x: AssignGeneric => genAssignGeneric(x)
        case x: InstanceOfGeneric => genInstanceOfGeneric(x)

        case x: FieldSeqOperation => genFieldSeqOperation(x)

        case x: InitStringRecord => genInitStringRecord(x)

        case _: MemBarrier => // nop
          // TODO: make proper support for barriers in CBC: JET-14017
          // TODO: overrides common code generator, remove it when CodeEmitterCBC appear

        case FieldAddr(field) =>
          val IReg(dst) = node
          asm.lea_static(dst, createFieldRef(field))

        case CFuncWrapperAddr(method) =>
          val IReg(dst) = node
          if (Isa12Mode) {
            isa12Asm.cFuncWrap(dst, new MethodReference(method, MethodReferenceAccessKind.STATIC).getPermanent)
          } else {
            oldIsa.cFuncWrapOld(dst, new MethodReference(method, MethodReferenceAccessKind.STATIC).getPermanent)
          }

        case x: ZeroRefs =>
          asm.zerorefs(x.sa.slot.asInstanceOf[TypedFrameSlotCBC].typedSlot)

        case x: ThisTypeInfoCBC =>
          val IReg(dst) = x
          val ftcSigIdx = CodeSigSymbol(x.target)
          if (ftcSigIdx.containsTypeVariables) {
            asm.loadTypeInfoFTC(dst, ftcSigIdx)
          } else {
            asm.loadTypeInfoSig(dst, ftcSigIdx)
          }

        case x: LoadTypeInfo =>
          val IReg(dst) = x
          val fasm = asm.asInstanceOf[ForkedISA12Assembler]
          fasm.loadTypeInfoSig(dst, x.target.toCbc)

        case x: LoadTypeInfoGeneric =>
          val IReg(dst) = x
          val fasm = asm.asInstanceOf[ForkedISA12Assembler]
          fasm.loadTypeInfoGeneric(dst, x.target.uninstantiated.toCbc)

        case x: GenericTypeArg =>
          val IReg(dst) = x
          val IReg(ti) = x.typeInfo
          val fasm = asm.asInstanceOf[ForkedISA12Assembler]
          fasm.typeArg(ti, x.idx, dst)

        case x: ThisTypeInfoByCBC =>
          val IReg(dst) = x
          val IReg(obj) = x.obj
          asm.loadTypeInfoObj(dst, obj)

        case x: DerivedPtr =>
          val IReg(dst) = x
          val IReg(src) = x.derived
          asm.mov(dst, src, reference = false)

        case x: AtomicOps.AtomicNode =>
          genAtomic(x)

        case _ => super.genNodeImpl(node)
      }
      updateLiveness(node)
    }

    // TODO: its time to introduce CodeEmitterCBC
    override protected def genJump(target: Label): Unit = asm.jmp(target)

    override protected def genJump(target: Block, isNext: Block => Boolean): Unit =
      if (!isNext(target)) genJump(startOf(target))

    override protected def genReturn(): Unit = {
      assert(!Isa12Mode)
      oldIsa.ret()
    }

    private def genReturnIsa12(ret: Return): Unit = {
      val retType = rootMethod.getReturnType
      if (retType.isZST) {
        isa12Asm.ret(IR.IR1, W64) // TODO: mark as primitive when ret.ref is separated from ret.64
        return
      }

      val width = if (retType.width < W64) W32 else W64

      ret.inValue match {
        case IReg(v) if retType.isReference =>
          isa12Asm.retRef(v)
          transferMark(v, IR.IR1)
          
        case IReg(v) =>
          isa12Asm.ret(v, width)
          transferMark(v, IR.IR1)

        case FReg(v) =>
          isa12Asm.fret(v, width)
      }
    }

    private def conditionCode(condition: Condition): CC = {
      condition match {
        case Condition.GE_OR_UNORDERED => CC.GE
        case Condition.GT_OR_UNORDERED => CC.GT
        case Condition.LT_OR_UNORDERED => CC.LT
        case Condition.LE_OR_UNORDERED => CC.LE
        case Condition.EQ => CC.EQ
        case Condition.NE => CC.NE
        case Condition.LT => CC.LT
        case Condition.LE => CC.LE
        case Condition.GT => CC.GT
        case Condition.GE => CC.GE
        case Condition.UGE => CC.GE
        case Condition.UGT => CC.GT
        case Condition.ULT => CC.LT
        case Condition.ULE => CC.LE
      }
    }

    /** Generates assembler pattern for end of given `block`. */
    override protected def genBlockEnd(block: Block, isNext: Block => Boolean): Unit = {
      genBlockEnd0(block, isNext)
      updateLiveness(block.blockEnd)
    }

    override def genCode0(segment: Segment, layout: Layout, xInfo: XInfo, methodStart: Label, slowPathStubStart: Label): Code = {
      CodeCBC(segment, xInfo, exTable.build, livenessInfo)
    }

    override def doFreeze(): Unit = {}

    private lazy val exTable = ExceptionTable.Builder()

    override def genXHandlerInfo(b: Block): Unit = {
      b.singleXHandlerOption.foreach(xb => exTable.addRegionRef(startOf(b), endOf(b), startOf(xb)));
    }

    private def genBlockEnd0(block: Block, isNext: Block => Boolean): Unit = block.blockEnd match {
      case branch: If =>
        val (condition, _, directJmpBlock, condJmpBlock) = prepareGenBranch(branch, isNext)
        val condJmpLabel = startOf(condJmpBlock)
        val selector = branch.singleAttachedByReason(Group.AttachReason.COND_BRANCH_ARG).get

        selector match {
          case tt: TypeTest =>
            val negated = condition == Condition.EQ
            val l = iReg(tt.obj)
            tt.guard match {
              case CHABitGuard              => asm.bttCHA   (l, negated,                               condJmpLabel)
              case LevelGuard(level)        => asm.bttLevel (l, negated, level,                        condJmpLabel)
              case PointGuard(klass)        => asm.bttPoint (l, negated, CodeSigSymbol(klass),         condJmpLabel)
              case ConeGuard(klass, closed) => asm.bttCone  (l, negated, CodeSigSymbol(klass), closed, condJmpLabel)
            }

          case cmp @ CmpAnyInstanceOf(iof, tpe, obj) if branch.hasAttachedByReason(Group.AttachReason.INSTANCE_OF_BRANCH) =>
            assert(iof.attachedTo(branch), s"$iof $branch")
            if (tpe.isClass) {
              asm.bttIOFC(iReg(obj), negated = condition == Condition.EQ, CodeSigSymbol(tpe), condJmpLabel)
            } else if (tpe.isInterface) {
              asm.bttIOFI(iReg(obj), negated = condition == Condition.EQ, CodeSigSymbol(tpe), condJmpLabel)
            } else {
              require(tpe.isArray)
              asm.bttIOFA(iReg(obj), negated = condition == Condition.EQ, CodeSigSymbol(tpe), condJmpLabel)
            }

          case Cmp(_, l, r) if l.tpe.isFloatingPointType =>
            asm.bcc(branchOp(condition, l.tpe), fReg(l), fReg(r), widthOf(l), condJmpLabel)

          case CmpOrTest(_, l, n: AnyNull) =>
            assert(l.tpe.isTraceableRefType, s"$l")
            asm.bcc(branchOp(condition, l.tpe), iReg(l), IR.IRZ, W64, condJmpLabel)

          case CmpOrTest(_, l, r @ IntegralConst(c)) =>
            assert(r.tpe.isIntegralType, s"${r.tpe}")
            // Performing normalization later is hard, because before bcc we also probably generate fext.ld.# operation.
            // And we need to normalize imm before this happens.
            asm.bcc(branchOp(condition, l.tpe), iReg(l), c, widthOf(l), condJmpLabel)

          case CmpOrTest(_, l, r) =>
            assert(l.tpe.isIntegralType || l.tpe.isTraceableRefType, s"${l.tpe}")
            asm.bcc(branchOp(condition, l.tpe), iReg(l), iReg(r), widthOf(l), condJmpLabel)
        }

        genJump(directJmpBlock, isNext)

      case Goto(_, target) =>
        genJump(target, isNext)

      case ret: Return if Isa12Mode =>
        genReturnIsa12(ret)

      case _ =>
        super.genBlockEnd(block, isNext)
    }

    def livenessInfo: LivenessInfoCollector.AllStates = asm match {
      case assembler: ForkedISA12Assembler => livenessCollector.collect
      case _ => LivenessInfoCollector.empty
    }
  }

  private def cbcTypeKind(`type`: Type): CbcTypeKind = `type` match {
    case _: EopType | _: FragileReferenceType => CbcTypeKind.REF
    case _: RecordAddrType                    => CbcTypeKind.REC
    case ThinType | _: FragilePointerType     => CbcTypeKind.U64
    case IntType                              => CbcTypeKind.I32
    case LongType                             => CbcTypeKind.I64
    case FloatType                            => CbcTypeKind.F32
    case DoubleType                           => CbcTypeKind.F64
    case HolderType(sig)                      =>
      assert(!sig.isTypeVariable, "unexpected to have holder type instantiated by type variable")
      cbcTypeKind(ValueType.fromSig(sig))
    case _                                    => CbcTypeKind.I32
  }
}
