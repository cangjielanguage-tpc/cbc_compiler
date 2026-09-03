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
import com.huawei.excelsior.jet.assembler.Width.{W32, W64}
import com.huawei.excelsior.jet.assembler.cbc.*
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.{MultiFieldReference, NoneFieldReference}
import com.huawei.excelsior.jet.assembler.cbc.Local.*
import com.huawei.excelsior.jet.assembler.cbc.Register.*
import com.huawei.excelsior.jet.assembler.cbc.Register.IR.{IR1, IR2}
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessInfoCollector
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.{LoadAccessKind, StoreAccessKind}
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.{MemSpace, Assembler as ForkedISA12Assembler}
import com.huawei.excelsior.jet.assembler.{AsmEmitter, AsmType, Label, Location, Segment, Symbol, Width}
import com.huawei.excelsior.jet.compiler.NotImplementedFeature.CBC
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp.*
import com.huawei.excelsior.jet.compiler.cbc.CbcSignatureAdapter.toCbc
import com.huawei.excelsior.jet.compiler.cbc.{CbcSymbolAdapter, CodeSigSymbol}
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
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ReferenceType
import xscala.io.ByteBuffer

import scala.PartialFunction.condOpt
import scala.annotation.nowarn

@nowarn("msg=match may not be exhaustive")
trait CodeGeneratorCBC extends CodeGenerator with XSitesToolboxCBC with DebugGeneratorCBC with LocalLivenessAnalyzerCBC { self: Universe with BackEndCBC =>

  override protected lazy val asm: AsmEmitter = new ForkedISA12Assembler with CbcSymbolAdapter
  lazy val cbc: ForkedISA12Assembler = asm.asInstanceOf[ForkedISA12Assembler]

  override protected lazy val emit = null // TODO-CBC: CodeEmitterCBC

  class CodeGeneratorImplCBC extends CodeGeneratorImpl with DebugGeneratorImplCBC with XSitesGeneratorCBC {

    private def asm = cbc

    private lazy val livenessCollector = LivenessInfoCollector()

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
      val aliveResources = gcMaps(node).toSeq.map(normalizeRes)
      val idxPairs = mutPairsData(node).toSeq.map((base, derived) => (normalizeRes(base), normalizeRes(derived)))
      livenessCollector.saveResources(segment, aliveResources, idxPairs)
    }

    def saveStateForStackChecks(node: Node): Unit = if (needStackPtrsInfo(node)) {
      livenessCollector.saveStackPtrs(segment, stackPtrsData(node).toSeq.map(normalizeRes))
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
        case FReg(r) => asm.fdiv(widthOf(div), fReg(div), l, r)
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

    private def genNewImpl(n: Node, allocType: SignatureType): Unit = {
      assert(iReg(n) == IR1)

      val t = allocType match {
        case SignatureType.Box(t) => t
        case t => t
      }
      if (t.isCangjieLambda) {
        asm.newClosure(IR1, t.toCbc)
      } else {
        asm.newobj(t.toCbc)
      }

      addXSite(n)
      saveGCState(n)
    }

    private def genFieldSeqOperation(n: FieldSeqOperation): Unit = {
      addXSite(n)

      val adapter = asm.adapter

      val fieldRefs = n.fields
      val builder = MemSpace.Builder()

      // TODO remove
      def memExprHead(base: Node, n: Node): Unit = n match {
        case sa: HasFrameSlot => sa.slot match {
          case slot: TypedFrameSlotCBC => builder.typed(slot.typedSlot)
          case _ => shouldNotReachHere(sa)
        }

        case n @ ArrayGet(_, _, IReg(obj), IReg(idx)) =>
          assert(n.arrayType.isRecordArray)
          builder.obj(obj)
            .index(idx, n.arrayType.getArrayElemType.toCbc, checked = false)

        case IReg(r) =>
          if (fieldRefs.head.refType.isTraceableReference) {
            builder.obj(r)
          } else {
            valueOf(base).producer match {
              case base: DerivedPtr.BaseHandle =>
                builder.rec(r)
              case _ =>
                val IReg(b) = base
                builder.handle(b, r)
            }
          }
      }

      // TODO remove
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
      }).gen(asm)

      def getBaseLocation(n: Node) = n match {
        case IReg(r) => r
      }

      def maybeImmValue(value: Node): Option[Long] = value match {
        case _: AnyNull => Some(0L)
        case IntegralConst(c) => Some(c)
        case FConst(c) => Some(java.lang.Float.floatToRawIntBits(c))
        case DConst(c) => Some(java.lang.Double.doubleToRawLongBits(c))
        case Reg(r) => None
      }

      def constrFieldRef(frs: Seq[CangjieFieldReference]): CbcFileFormat.FieldReference = {
        val adaptedRefs = frs.map { fr => 
          fr.field match {
            case Some(name) => fr
            case None =>
              val refType = fr.refType match {
                case t: SignatureType.OptionLikeEnum =>
                  require(!t.isNullableOption)
                  SignatureType.Tuple(Seq(SignatureType.Boolean, t.someType))
                case t => t
              }
              CangjieFieldReference(fr.idx, None, refType, fr.fieldType)
          }
        }.map(adapter.field) 
        
        adaptedRefs match {
          case Seq(field) => field
          case refs => MultiFieldReference(refs)
        }
      }

      n match {
        case n: GetFieldSeqRef if !n.isInstanceOf[HasFrameSlot] =>
          val IReg(dst) = n
          val base = getBaseLocation(n.base)
          asm.lea(dst, base, constrFieldRef(fieldRefs))
        case n: LoadFieldSeq if !n.isInstanceOf[HasFrameSlot] =>
          val Reg(dst) = n
          val base = getBaseLocation(n.base)
          asm.ld(dst, base, constrFieldRef(fieldRefs))
        case n: (GetFieldSeqRef | LoadFieldSeq) =>
          val Reg(dst) = n
          memExprHead(n.baseRef, n.base)
          fields(fieldRefs)
          builder.load(dst).gen(asm)
        case n: GetFieldSeqRefGeneric =>
          val Reg(dst) = n
          memExprHead(n.baseRef, n.base)
          fields(fieldRefs, n.typeInfos)
          builder.load(dst).gen(asm)
        case n: LoadFieldSeqGeneric =>
          val Reg(dst) = n
          memExprHead(n.baseRef, n.base)
          fields(fieldRefs, n.typeInfos)
          if (n.resType.isVariableSizeType) {
            val IReg(ti) = n.typeInfos.last
            builder.loadGeneric(dst.asInstanceOf[IR], ti).gen(asm)
          } else {
            builder.load(dst).gen(asm)
          }
          addXSite(n)
          saveGCState(n)
        case n: GetStaticFieldSeqRef =>
          val Reg(dst) = n
          assert(fieldRefs.size == 1 || !fieldRefs.head.fieldType.isTraceableReference, fieldRefs)
          builder.static(adapter.field(fieldRefs.head))
          fields(fieldRefs.tail)
          builder.load(dst).gen(asm)
        case n: LoadStaticFieldSeq =>
          val Reg(dst) = n
          assert(fieldRefs.size == 1 || !fieldRefs.head.fieldType.isTraceableReference, fieldRefs)
          asm.ld(dst, constrFieldRef(fieldRefs))
        case n: StoreFieldSeq if !n.isInstanceOf[HasFrameSlot] =>
          addXSite(n)
          maybeImmValue(n.inValue) match {
            case Some(_) => shouldNotReachHere("Field seq stores with imm are not supported")
            case None =>
              val Reg(src) = n.inValue
              val base = getBaseLocation(n.base)
              asm.st(src, base, constrFieldRef(fieldRefs))
          }
        case n: StoreFieldSeq =>
          memExprHead(n.baseRef, n.base)
          fields(fieldRefs)
          store(n.inValue)
        case n: StoreFieldSeqGeneric =>
          addXSite(n)
          memExprHead(n.baseRef, n.base)
          fields(fieldRefs, n.typeInfos)
          if (n.resType.isVariableSizeType) {
            val IReg(ti) = n.typeInfos.last
            val IReg(src) = n.inValue
            builder.storeGeneric(src, ti).gen(asm)
          } else {
            store(n.inValue)
          }
        case n: StoreStaticFieldSeq =>
          addXSite(n)
          assert(fieldRefs.size == 1 || !fieldRefs.head.fieldType.isTraceableReference, fieldRefs)
          maybeImmValue(n.inValue) match {
            case Some(x) => shouldNotReachHere(s"Field seq stores with imm are not supported: $x")
            case None =>
              val Reg(src) = n.inValue
              asm.st(src, constrFieldRef(fieldRefs))
          }
      }
    }

    private def createFieldRef(f: Field): FieldReference = {
      FieldRef.createFieldRef(f, None, None)
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
      asm.bfx(dst, src, resW, argW, bfx.signExtension, bfx.offset, bfx.size)
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
        case _: CangjieArrayLength => asm.lenarr(len, arr)
      }
    }

    private def genArrayGet(arrGet: ArrayGet): Unit = {
      addXSite(arrGet)

      val adapter = asm.adapter
      
      val arrayType = arrGet.arrayType
      val elemType = arrayType.getArrayElemType
      val asmType = elemType.toAsm
      val dst = if asmType.isFloatingPoint && asmType != AsmType.F16 then fReg(arrGet) else iReg(arrGet)
      (arrGet.array, arrGet.idx) match {
        case (IReg(arr), IReg(idx)) =>
          if (elemType.isRecord) {
            asm.index(dst.asInstanceOf[IR], arr, idx, adapter.sigType(CodeSigSymbol(elemType)))
          } else if (elemType.isTraceableReference) {
            asm.ldarrObj(dst, arr, idx)
          } else {
            asm.ldarr(asmType, dst, arr, idx)
          }
      }
    }

    private def genArrayPut(arrPut: ArrayPut): Unit = (arrPut.inValue0, arrPut.array, arrPut.idx) match {
      case (Reg(value), IReg(arr), IReg(idx)) =>
        addXSite(arrPut)

        val arrayType = arrPut.arrayType
        val elemType = arrayType.getArrayElemType

        assert(!elemType.isRecord)
        if (elemType.isTraceableReference) {
          asm.starrObj(arr, idx, value)
        } else {
          asm.starr(elemType.toAsm, arr, idx, value)
        }
    }

    private def genArrayFill(arrayFill: ArrayFill): Unit = {
      val IReg(arr) = arrayFill.array
      asm.arrFill(arr, getConstBytes(arrayFill.totalBytes, arrayFill.elemType, arrayFill.inValues0))
    }

    private def genNewArr(newArr: NewArray): Unit =
      genNewArrImpl(newArr, newArr.allocType, newArr.lengths, newArr.uninitialized)

    private def genNewArrImpl(newArr: Node, allocType: SignatureType, lengths: Seq[Node], zeroValue: Boolean): Unit = {
      assert(lengths.size == 1)
      assert(iReg(lengths.head) == IR2)
      assert(iReg(newArr) == IR1)

      val ftcSigIdx = CodeSigSymbol(allocType)
      assert(allocType.isCangjieArray)
      if (zeroValue) {
        asm.newarrzv(ftcSigIdx)
      } else {
        asm.newarr(ftcSigIdx)
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
          assert(arrayType.isCangjieArray)
          asm.arrIC(idx, len)
      }
      addXSite(aic)
    }

    // TODO: use or remove
    private def genPackageInitCheck(init: PackageInitCheck): Unit = ()

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

    private def genLoadStackAlloc(dst: IR, sa: HasFrameSlot): Unit = {
      sa.slot match {
        case slot: TypedFrameSlotCBC =>
          slot.tpe match {
            case sig: SignatureType.TypeVariable =>
              val builder = MemSpace.Builder()
              builder.typed(slot.typedSlot)
              builder.constIndex(0, SignatureType.Tuple(Seq(ReferenceType.cangjieStdCoreObject.sigType)).toCbc)
              builder.load(dst).gen(asm)
            case sig =>
              if (sig.isRecord) {
                asm.ldstackrec(dst, slot.typedSlot)
              } else {
                notImplemented("Stack alloc")
              }
          }
        case slot: FrameSlotCBC =>
          asm.lea_us(dst, slot.untypedSlot)
      }
    }

    private def genCopyStructure(c: CopyStructure): Unit = {
      addXSite(c)

      val adapter = asm.adapter

      (c.dstBase, c.dst, c.srcBase, c.src) match
        case (IReg(dstBase), IReg(dst), IReg(srcBase), IReg(src))  =>
          asm.copy(dstBase, dst, srcBase, src, adapter.sigType(CodeSigSymbol(c.structureType)))
          if(valueOf(c.dstBase).producer.isInstanceOf[DerivedPtr.Local]) {
            mark(dst, LocalType.CLEARED)
          }
        case _ => shouldNotReachHere(c)
    }

    private def genInitObj(obj: InitObj): Unit = obj.slot match {
        case slot: TypedFrameSlotCBC =>
          assert(!slot.tpe.isRecord)
          asm.initobj(slot.typedSlot)
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

      val realTpe = tpe match {
        case SignatureType.Box(t) => t
        case t => t
      }
      asm.isInstanceOf(dstReg, objReg, realTpe.toCbc)
    }

    private def genCheckCast(checkCast: CheckCast): Unit = shouldNotReachHere("Unsupported")

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
            asm.loadRawMemory(dst, src, LoadAccessKind.from(cbcTypeKind(load.tpe)), 0)
        case sa @ StackAlloc.Local(t) =>
          sa.slot match {
            case slot: TypedFrameSlotCBC =>
              val builder = MemSpace.Builder()
              builder.typed(slot.typedSlot)
              builder.constIndex(0, SignatureType.Tuple(Seq(ReferenceType.cangjieStdCoreObject.sigType)).toCbc)
              builder.load(dst).gen(asm)
            case slot: FrameSlotCBC =>
              assert(t.isTraceableReference || t.isPrimitive)
              asm.loadUntyped(dst, LoadAccessKind.from(cbcTypeKind(load.tpe)), slot.untypedSlot)
          }
      }
    }

    override protected def genLoadTailParam(ltp: LoadTailParam): Unit = (ltp, ltp.tpe) match {
      case (LoadTailParam(IReg(tailReg), offset), tpe) =>
        val Reg(dst) = ltp
        val ldk = LoadAccessKind.from(cbcTypeKind(tpe))
        asm.loadRawMemory(dst, tailReg, ldk, offset)
    }

    private def genStoreMemory(store: StoreMemory): Unit = {
      (store.addr, store.inValue0) match {
        case (IReg(dst), Reg(src)) =>
          (store.signature, src) match {
            case (_: SignatureType.Box, _) =>
              assert(!src.isIReg || check(src.asIReg, localTypeOf(store.inValue0)))
              val builder = MemSpace.Builder()
              builder.rec(dst)
              builder.constIndex(0, SignatureType.Tuple(Seq(ReferenceType.cangjieStdCoreObject.sigType)).toCbc)
              builder.store(src).gen(asm)
            case (_, src: (IR | FR)) =>
              asm.storeRawMemory(src, dst, StoreAccessKind.from(CbcTypeKind(store.accessType)), 0)
          }
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

      asm.initConstString(sa.slot.asInstanceOf[TypedFrameSlotCBC].typedSlot, stringSymbol)
    }

    // TODO: fix hierarchy
    override protected def genDeprive(dst: IREG, src: IREG): Unit = notImplemented(CBC)

    // TODO: fix hierarchy
    override protected def mergeRichPointer(dst: IREG, imt: IREG, ptr: IREG): Unit = notImplemented(CBC)

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

    override protected def beforeCallActions(call: Call): Unit = {
      for (mut <- call.attachedByReason(Group.AttachReason.MUT_FUNC_ARG)) {
        val mutRecordReg = call.abi.paramLocations(call.methodType.getMutRecordArgIdx).asInstanceOf[IR]
        val mutObjectReg = call.abi.paramLocations(call.methodType.getMutObjectArgIdx).asInstanceOf[IR]
        mark(mutObjectReg, LocalType.REFERENCE)
        mark(mutRecordReg, LocalType.CLEARED)
      }
    }

    // TODO: fix hierarchy
    override protected def initTailRegister(call: Call): Unit = ()

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

      def resultReg: IR = call match {
        case IReg(r) => r
        case _ => IR.IR1
      }

      call.target match {
        case InvokeInterfaceTarget(LightInterfCastCBC(rcvType)) =>
          assert(Isa12Mode)
          asm.callInterf(resultReg, CodeSigSymbol(rcvType), targetRef.getPermanent)
        case _ =>
          assert(!targetRef.isInterfCall)
      }

      val realICallGenerated = Isa12Mode && targetRef.isInterfCall

      def genericCall(mr: InstantiatedMethodReference): Unit = {
        assert(mr.method.hasUniversalGenericContext)
        val m = mr.getPermanent
        if (mr.methodType.hasReceiverParameter) {
          asm.callDirect(resultReg, m)
        } else {
          shouldNotReachHere(s"Incorrect method type: ${mr.methodType}")
        }
      }

      def directCall(): Unit = {
        val permanent = targetRef.getPermanent
        asm.callDirect(resultReg, permanent)
      }

      def virtualStaticCall(): Unit = {
        val permanent = targetRef.getPermanent
        shouldNotReachHere("unsupported")
      }

      def directCCall(method: Method): Unit = {
        val permanent = new MethodReference(method, targetRef.accessKind).getPermanent
        asm.callDirect(resultReg, permanent)
      }

      def virtualCall(): Unit = {
        assert(check(call.abi.paramLocations(0).asIReg, LocalType.REFERENCE))
        val permanent = targetRef.getPermanent

        if (targetRef.refType.sigType.isCangjieClosure) {
          if (targetRef.method.getName == "$GenericVirtualFunc") {
            asm.callClosureGeneric(resultReg, targetRef.refType.sigType.toCbc)
          } else {
            asm.callClosure(resultReg, targetRef.refType.sigType.toCbc)
          }
        } else {
          val outerTI = call.invokeArgs(targetRef.methodType.getOuterTypeInfoArgIdx)
          val loc = outerTI match {
            case IReg(reg) => reg
            case UntypedSlot(slot, _) => slot
          }
          asm.callInterfGeneric(loc, asm.adapter.method(permanent))
        }
      }

      if (!realICallGenerated) {
        call match {
          case UniversalGeneric.InvokeMethodWithGenericContext(mr) => genericCall(mr)
          case VirtualStaticCall() => virtualStaticCall()
          case AnyDirectCall(_) => directCall()
          case DirectCall(method) if !targetRef.hasMethod => directCCall(method)
          case AnyVirtualCall() => virtualCall()
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
          asm.movAcc(src)
        case (_: SaveCallRefTypeInfo, sa @ StackAlloc.Local(t)) =>
          asm.loadUntypedAcc(LoadAccessKind.SPECIAL, sa.slot.asInstanceOf[FrameSlotCBC].untypedSlot)

        case (IReg(dst), IReg(src)) if arg.tpe.isHolderType => arg.tpe.asInstanceOf[HolderType].instantiatedSig match {
          case _: TypeVariable => asm.mov(dst, src, reference = true)
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

        case (IReg(dst), sa @ StackAlloc.Local(t)) =>
          sa.slot match {
            case slot: TypedFrameSlotCBC =>
              slot.tpe match {
                case sig: SignatureType.TypeVariable =>
                  val builder = MemSpace.Builder()
                  builder.typed(slot.typedSlot)
                  builder.constIndex(0, SignatureType.Tuple(Seq(ReferenceType.cangjieStdCoreObject.sigType)).toCbc)
                  builder.load(dst).gen(asm)
                case sig =>
                  assert(sig.isRecord)
                  asm.ldstackrec(dst, slot.typedSlot)
              }
            case slot: FrameSlotCBC =>
              assert(t.isTraceableReference || t.isPrimitive)
              asm.loadUntyped(dst, LoadAccessKind.SPECIAL, slot.untypedSlot)
          }

        case (IReg(dst), sa: HasFrameSlot) =>
          genLoadStackAlloc(dst, sa)

        case (IReg(dst), x: DerivedPtr.Local) =>
          asm.movbp(dst, local = true)

        case (IReg(dst), x: DerivedPtr.Global) =>
          asm.movbp(dst, local = false)

        case (Reg(dst), UntypedSlot(src, _)) => arg.tpe match {
          case tpe => asm.loadUntyped(dst, cbcTypeKind(tpe), src)
        }
        case (UntypedSlot(dst, _), src) => (node.tpe, src) match {
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

    private def genBox(n: Box): Unit = {
      val IReg(dst) = n
      n.value match {
        case sa: HasFrameSlot => asm.box(sa.slot.asInstanceOf[TypedFrameSlotCBC].typedSlot, dst)
        case Reg(src)         => asm.box(src, dst, n.base.toCbc)
        case _: Void          => asm.box(IR.IRZ, dst, n.base.toCbc)
      }
      addXSite(n)
      saveGCState(n)
    }

    private def genUnbox(n: Unbox): Unit = {
      val IReg(src) = n.value
      n match {
        case Reg(dst) => asm.unbox(dst, src, n.base.toCbc)
      }
    }

    private def genUnboxRec(n: UnboxRec): Unit = {
      val IReg(src) = n.value
      asm.unbox(n.slot.asInstanceOf[TypedFrameSlotCBC].typedSlot, src)
    }

    private def genUnboxLea(n: UnboxLea): Unit = {
      val fasm = asm.asInstanceOf[ForkedISA12Assembler]
      val IReg(dst) = n
      val IReg(src) = n.value
      fasm.leaBox(dst, src)
    }

    private def genSpawnFuture(n: SpawnFuture): Unit = {
      val IReg(futureReg) = n.future
      asm.spawnFuture(futureReg, n.retType.toCbc)
      addXSite(n)
      saveGCState(n)
    }

    private def genSpawnClosure(n: SpawnClosure): Unit = {
      val IReg(closureReg) = n.closure
      asm.spawn(closureReg, n.closureType.toCbc)
      addXSite(n)
      saveGCState(n)
    }

    private def genOptionTagGeneric(n: OptionTagGeneric): Unit = {
      val IReg(dst) = n
      val IReg(src) = n.value
      val IReg(bti) = n.baseTypeInfo
      asm.tagGeneric(dst, src, bti, n.optionType.toCbc)
    }

    private def genOptionPayloadGeneric(n: OptionPayloadGeneric): Unit = {
      val IReg(dst) = n
      val IReg(src) = n.value
      val IReg(bti) = n.baseTypeInfo
      val IReg(oti) = n.optionTypeInfo
      asm.payloadGeneric(dst, src, bti, oti, n.optionType.toCbc)

      addXSite(n)
      saveGCState(n)
    }

    private def genNewNoneOptionGeneric(n: NewNoneOptionGeneric): Unit = {
      val IReg(dst) = n
      val IReg(bti) = n.baseTypeInfo
      val IReg(oti) = n.optionTypeInfo
      asm.newNoneGeneric(dst, bti, oti, n.optionType.toCbc)

      addXSite(n)
      saveGCState(n)
    }

    private def genNewSomeOptionGeneric(n: NewSomeOptionGeneric): Unit = {
      val IReg(dst) = n
      val IReg(src) = n.value
      val IReg(bti) = n.baseTypeInfo
      val IReg(oti) = n.optionTypeInfo
      asm.newSomeGeneric(dst, src, bti, oti, n.optionType.toCbc)

      addXSite(n)
      saveGCState(n)
    }

    private def genAssignGeneric(n: AssignGeneric): Unit = {
      val IReg(dst) = n.dst
      val IReg(src) = n.src
      val IReg(bti) = n.baseTypeInfo
      asm.assignGeneric(dst, src, bti)

      addXSite(n)
      saveGCState(n)
    }

    private def genInstanceOfGeneric(n: InstanceOfGeneric): Unit = {
      val IReg(dst) = n
      val IReg(obj) = n.obj
      val IReg(bti) = n.targetTypeInfo
      asm.instanceOfGeneric(dst, obj, bti)
    }

    private def genNewGeneric(n: NewGeneric): Unit = {
      assert(iReg(n) == IR1)
      val IReg(ti) = n.allocTypeInfo
      // TODO: do we need it here?
      val t = n.allocType match {
        case SignatureType.Box(t) => t
        case t => t
      }
      val fasm = asm.asInstanceOf[ForkedISA12Assembler]
      if (t.isCangjieLambda) {
        fasm.newClosureGeneric(ti, t.toCbc)
      } else {
        fasm.newobjGeneric(ti, t.toCbc)
      }
      addXSite(n)
      saveGCState(n)
    }

    private def genAtomic(n: AtomicOps.AtomicNode): Unit = {
      val adapter = asm.adapter
      n match {
        case x: AtomicOps.Load =>
          val IReg(dst) = x
          val IReg(obj) = x.obj
          val field = adapter.field(x.field)
          asm.atomicLoad(dst, obj, field)

        case x: AtomicOps.Store =>
          val IReg(src) = x.value
          val IReg(obj) = x.obj
          val field = adapter.field(x.field)
          asm.atomicStore(src, obj, field)

        case x: AtomicOps.CAS =>
          val IReg(dst) = x
          val IReg(obj) = x.obj
          val IReg(src1) = x.compareValue
          val IReg(src2) = x.swapValue
          val field = adapter.field(x.field)
          asm.cas(dst, obj, src1, src2, field)

        case x: AtomicOps.Simple =>
          import AtomicOps.Simple.Kind as Kind
          val gen = x.kind match {
            case Kind.SWAP       => asm.atomicSwap
            case Kind.FETCH_ADD  => asm.atomicFetchAdd
            case Kind.FETCH_SUB  => asm.atomicFetchSub
            case Kind.FETCH_AND  => asm.atomicFetchAnd
            case Kind.FETCH_OR   => asm.atomicFetchOr
            case Kind.FETCH_XOR  => asm.atomicFetchXor
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
        case x: BitFieldExtract            => genBFX(x)
        case x: Neg                        => genNeg(x)
        case x: ArrayLength                => genArrLen(x)
        case x: ArrayGet                   => genArrayGet(x)
        case x: ArrayPut                   => genArrayPut(x)
        case x: ArrayFill                  => genArrayFill(x)
        case x: ArrayIndexCheck            => genArrayIndexCheck(x)
        case x: NewArray                   => genNewArr(x)
        case x: NewArrayFill               => genNewArrFill(x)
        case x: PackageInitCheck           => genPackageInitCheck(x)
        case x: InstanceOf                 => genInstanceOf(x)
        case x: ControlledInstanceOf       => genInstanceOf(x)
        case x: CheckCast                  => genCheckCast(x)
        case x: DivisorCheck               => genDivisorCheck(x)
        case x: GCPoint                    => genGCPoint(x)
        case x: MathIntrinsic              => genMathIntrinsic(x)
        case x: LoadMemory                 => genLoadMemory(x)
        case x: StoreMemory                => genStoreMemory(x)
        case x: CopyStructure              => genCopyStructure(x)
        case x: InitObj                    => genInitObj(x)
        case x: EndLocalUnmovable          => genEndLocalUnmovable(x)
        case x: CatchCBC                   => genCatch(x)

        case x: Box => genBox(x)
        case x: Unbox => genUnbox(x)
        case x: UnboxRec => genUnboxRec(x)
        case x: UnboxLea => genUnboxLea(x)

        case x: SpawnFuture => genSpawnFuture(x)
        case x: SpawnClosure => genSpawnClosure(x)

        case x: OptionTagGeneric => genOptionTagGeneric(x)
        case x: OptionPayloadGeneric => genOptionPayloadGeneric(x)
        case x: NewNoneOptionGeneric => genNewNoneOptionGeneric(x)
        case x: NewSomeOptionGeneric => genNewSomeOptionGeneric(x)
        case x: AssignGeneric => genAssignGeneric(x)
        case x: InstanceOfGeneric => genInstanceOfGeneric(x)
        case x: NewGeneric => genNewGeneric(x)

        case x: FieldSeqOperation => genFieldSeqOperation(x)

        case x: InitStringRecord => genInitStringRecord(x)

        case _: MemBarrier => // nop
          // TODO: make proper support for barriers in CBC: JET-14017
          // TODO: overrides common code generator, remove it when CodeEmitterCBC appear

        case FieldAddr(field) =>
          val IReg(dst) = node
          asm.lea_static(dst, createFieldRef(field))

        case x: ZeroRefs =>
          asm.zerorefs(x.sa.slot.asInstanceOf[TypedFrameSlotCBC].typedSlot)

        case x: LoadTypeInfo =>
          val IReg(dst) = x
          asm.loadTypeInfoSig(dst, x.target.toCbc)

        case x: LoadTypeInfoGeneric =>
          val IReg(dst) = x
          asm.loadTypeInfoGeneric(dst, x.target.uninstantiated.toCbc)

        case x: GenericTypeArg =>
          val IReg(dst) = x
          val IReg(ti) = x.typeInfo
          asm.typeArg(ti, x.idx, dst)

        case x: ThisTypeInfoByCBC =>
          val IReg(dst) = x
          val IReg(obj) = x.obj
          asm.loadTypeInfoObj(dst, obj)

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

    // TODO: fix hierarchy
    override protected def genReturn(): Unit = notImplemented(CBC)

    private def genReturnIsa12(ret: Return): Unit = {
      val retType = rootMethod.getReturnType
      if (retType.isZST) {
        asm.ret(IR.IR1, W64) // TODO: mark as primitive when ret.ref is separated from ret.64
        return
      }

      val width = if (retType.width < W64) W32 else W64

      ret.inValue match {
        case IReg(v) if retType.isReference =>
          asm.retRef(v)
          transferMark(v, IR.IR1)
          
        case IReg(v) =>
          asm.ret(v, width)
          transferMark(v, IR.IR1)

        case FReg(v) =>
          asm.fret(v, width)
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

    def livenessInfo: LivenessInfoCollector.AllStates = livenessCollector.collect
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
