/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.direct

import com.huawei.excelsior.jet.assembler.{Label, Location, Segment, Width}
import com.huawei.excelsior.jet.assembler.cbc.{CbcTypeKind, ExceptionTable, StackSlot}
import com.huawei.excelsior.jet.assembler.cbc.Register.FR
import com.huawei.excelsior.jet.assembler.cbc.Register.FR.FR0
import com.huawei.excelsior.jet.assembler.cbc.Register.FR.FR1
import com.huawei.excelsior.jet.assembler.cbc.Register.IR
import com.huawei.excelsior.jet.assembler.cbc.Register.IR.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.{LoadAccessKind, StoreAccessKind}
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessInfoCollector
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.Assembler as ForkedAssembler
import com.huawei.excelsior.jet.codeemitter.BranchOp
import com.huawei.excelsior.jet.common.XString.xstr
import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.abi.Frame
import com.huawei.excelsior.jet.compiler.abi.XTableGenerator
import com.huawei.excelsior.jet.compiler.abi.cbc.PlatformCBC
import com.huawei.excelsior.jet.compiler.cbc.CBCFileGenerator
import com.huawei.excelsior.jet.compiler.cangjie.CHIRVTable
import com.huawei.excelsior.jet.compiler.cbc.CbcSignatureAdapter.toCbc
import com.huawei.excelsior.jet.compiler.chir.{CHIR, CHIRLoader, CHIRResolver}
import com.huawei.excelsior.jet.compiler.ir.XInfo
import com.huawei.excelsior.jet.compiler.o2lib.opt.VZCModule
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule as pcO
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment as LE
import com.huawei.excelsior.jet.compiler.symlevel
import com.huawei.excelsior.jet.compiler.symlevel.{ConstStringSymbol, InstantiatedMethodReference, Method, MethodReference, MethodReferenceAccessKind as MAK, SignatureType}
import com.huawei.excelsior.jet.compiler.types.CompiledType
import com.huawei.excelsior.jet.compiler.{Pass, TypeProvider}

import scala.collection.mutable

/** Direct CHIR-to-CBC transliterator: bypasses the whole per-method optimizing
  * pipeline (CHIRInterpreter -> opt IR -> inline/optimize -> lowering -> regalloc
  * -> genCode) and emits isa12 bytecode straight from the CHIR body.
  *
  * M2 subset: literals, load/store of locals and static fields, arithmetic
  * (wrapping/throwing/saturating-agnostic int & float ops), comparisons, casts,
  * allocations, instance/static field access; anything else falls back to the
  * full pipeline per-method (mixing is legal, both paths feed the same
  * CBCFileGenerator).
  *
  * No register allocator: each CHIR value lives in a dedicated untyped stack
  * slot, registers are scratch only. Conservative GC maps: at every allocation
  * and call site the collector records all reference-typed slots (never
  * registers), which is sound because every value's home is its slot.
  */
class DirectCBCCompiler(fallback: VZCModule.CompilerInterface) extends VZCModule.CompilerInterface {
  private val env: LE = LE.getInstance

  override def enterClass(`class`: pcO.Class, stage: Pass): Unit =
    fallback.enterClass(`class`, stage)
  override def exitClass(`class`: pcO.Class): Unit = fallback.exitClass(`class`)
  override def printFinalStatistics(): Unit = {
    System.err.println(f"DIRECT-CBC: direct=$directCount fallback=$fallbackCount directTime=${directNanos / 1000}%dus fallbackTime=${fallbackNanos / 1000}%dus")
    fallback.printFinalStatistics()
  }

  /** Statistics for the potential measurement. */
  var directCount: Int = 0
  var fallbackCount: Int = 0
  var directNanos: Long = 0L
  var fallbackNanos: Long = 0L

  private val disablePat = sys.env.getOrElse("DIRECT_CBC_DISABLE", "")

  override def compileMethod(m: pcO.Method, versioned: symlevel.impl.light.VersionedMethod): Unit = {
    if (versioned != null) {
      fallback.compileMethod(m, versioned)
      return
    }
    val onlyPat = sys.env.getOrElse("DIRECT_CBC_ONLY", "")
    if (onlyPat.nonEmpty && !env.fromO2(m).toString.contains(onlyPat)) {
      fallbackCount += 1
      val t1 = System.nanoTime()
      fallback.compileMethod(m, versioned)
      fallbackNanos += System.nanoTime() - t1
      return
    }
    if (disablePat.nonEmpty && env.fromO2(m).toString.contains(disablePat)) {
      fallbackCount += 1
      val t1 = System.nanoTime()
      fallback.compileMethod(m, versioned)
      fallbackNanos += System.nanoTime() - t1
      return
    }
    val t0 = System.nanoTime()
    val direct = tryDirect(m)
    val t1 = System.nanoTime()
    if (direct) {
      directCount += 1
      directNanos += t1 - t0
      if (sys.env.contains("DIRECT_CBC_LIST")) System.err.println("DIRECT-CBC ok " + env.fromO2(m).toString)
    } else {
      fallbackCount += 1
      fallback.compileMethod(m, versioned)
      fallbackNanos += System.nanoTime() - t1
    }
  }

  private def tryDirect(m: pcO.Method): Boolean = {
    val method = env.fromO2(m)
    if (method.isAbstract || method.isNative) return false
    val chirDef = m.getCHIRDef.getOrElse(return false)
    implicit val resolver: CHIRResolver = CHIRLoader.getCHIRResolver(chirDef.source.toString)(env)
    val func: CHIR.Func = resolver.pkg.function(chirDef.id)
    if (func.body.isEmpty) return false

    if (sys.env.contains("DIRECT_CBC_DUMP") && method.toString.contains(sys.env("DIRECT_CBC_DUMP"))) {
      System.err.println(s"=== CHIR of $method ===")
      func.params.foreach(p => System.err.println(s"  PARAM ${p.getClass.getSimpleName} tpe=${p.tpe}"))
      func.body.foreach { bg =>
        bg.blocks.foreach { b =>
          System.err.println(s"BLOCK ${b.hashCode()}")
          b.expressions.foreach { e => System.err.println("  " + dumpExpr(e) + " @" + System.identityHashCode(e)) }
        }
      }
    }
    val ctx = new MethodContext(method, func, chirDef.source.toString)
    ctx.translate() && { ctx.send(); true }
  }

  private def dumpExpr(e: CHIR.Expression): String = e match {
    case a: CHIR.Apply => s"Apply(callee=${a.callee.id} thisArg=${try dumpVal(a.thisArg) catch { case _: Throwable => "<none>" }} args=${a.args.map(dumpVal).mkString(",")})"
    case s: CHIR.Store => s"Store(loc=${dumpVal(s.location)} value=${dumpVal(s.value)})"
    case l: CHIR.Load => s"Load(loc=${dumpVal(l.location)})"
    case b: CHIR.Binary => s"Binary(${b.kind} ${dumpVal(b.leftOperand)} ${dumpVal(b.rightOperand)})"
    case a: CHIR.Allocate => s"Allocate"
    case f: CHIR.Field => s"Field(base=${dumpVal(f.base)} path=${f.path})"
    case g: CHIR.GetElementRef => s"GetElementRef(base=${dumpVal(g.base)} path=${g.path})"
    case s: CHIR.StoreElementRef => s"StoreElementRef(loc=${dumpVal(s.location)} path=${s.path} value=${dumpVal(s.value)})"
    case t: CHIR.Terminator => s"Terminator(${t.getClass.getSimpleName})"
    case other => other.getClass.getSimpleName
  }
  private def dumpVal(v: CHIR.Value): String = v match {
    case l: CHIR.LocalVar => s"Local#${l.hashCode}" + (if (l.associatedExpr != null) "->" + l.associatedExpr.getClass.getSimpleName + "@" + System.identityHashCode(l.associatedExpr) else "")
    case p: CHIR.Parameter => s"Param#${p.hashCode}"
    case g: CHIR.GlobalVar => s"Global(${g.identifier})"
    case l: CHIR.Literal => s"Lit(${l.getClass.getSimpleName})"
    case other => other.getClass.getSimpleName
  }

  private class MethodContext(val method: Method, val func: CHIR.Func, val source: String)(using resolver: CHIRResolver) {
    private implicit val typeProvider: TypeProvider = env.getTypeProvider

    private var bail_ : Boolean = false
    /** Own-method ABI param registers (set during entry spill) — used to read
      * the method's own generic func params and outer TI. */
    private var ownParamLocs: Seq[Location] = Seq.empty
    private var ownOuterTiLoc: Option[IR] = None
    private var ownThisTiLoc: Option[IR] = None
    private def fail(msg: String): Unit = {
      if (sys.env.contains("DIRECT_CBC_BAIL")) System.err.println(s"DIRECT-CBC bail $method: $msg")
      bail_ = true
    }

    /** Signature type of a value (literals carry their own type). */
    private def sigOf(v: CHIR.Value): Option[SignatureType] = v match {
      case lv: CHIR.LocalVar => Some(resolver.typeSig(lv.tpe))
      case p: CHIR.Parameter => Some(resolver.typeSig(p.tpe))
      case g: CHIR.GlobalVar => Some(resolver.typeSig(g.tpe))
      case l: CHIR.Literal => Some(resolver.typeSig(l.tpe))
      case _ => None
    }

    // -- value model: one untyped stack slot per CHIR value.
    // LocalVars that have an associatedExpr are SSA aliases of the defining
    // expression (the interpreter reads/writes through to it), so canonicalize
    // the slot key: every expression owns a slot; plain locals own their own.
    private def keyOf(v: CHIR.Value): AnyRef = v match {
      case lv: CHIR.LocalVar if lv.associatedExpr != null => lv.associatedExpr
      case x => x
    }

    private val slotOf = mutable.HashMap.empty[AnyRef, Int]
    private val slotTypes = mutable.ArrayBuffer.empty[SignatureType]
    private val slotStringDone = mutable.HashSet.empty[AnyRef]
    // typed (stack-allocated record) frame slots: declared to the engine via
    // stackAllocatedTypeSigs so the runtime frame layout places them after the
    // untyped region and traces their ref fields for GC
    private val typedSigs = mutable.ArrayBuffer.empty[SignatureType]

    private def newTypedSlot(sig: SignatureType): StackSlot.Typed = {
      typedSigs += sig
      StackSlot.Typed(typedSigs.length - 1)
    }

    private def slot(v: CHIR.Value, sig: SignatureType): Int = slotOfAny(keyOf(v), sig)

    private val ZSCRATCH = IR7 // dedicated scratch for slot initialization

    private def slotOfAny(key0: AnyRef, sig: SignatureType): Int = {
      val key = key0
      slotOf.getOrElseUpdate(key, {
        val idx = slotTypes.length
        slotTypes += sig
        // every reference-typed slot starts as null so that the conservative
        // GC maps (which report all ref slots at alloc/call sites) can never
        // observe stack garbage; safe here because slot creation only happens
        // at expression boundaries where ZSCRATCH holds no live value
        if (sig.isTraceableReference) {
          asm.mov(ZSCRATCH, IRZ, reference = true)
          asm.storeUntyped(ZSCRATCH, StoreAccessKind.from(CbcTypeKind.REF), StackSlot.Untyped(idx))
        }
        idx
      })
    }

    /** Pre-creates (and null-initializes) slots for values used by the
      * upcoming expression; call at expression start while registers are dead. */
    private def touch(vals: CHIR.Value*): Unit = vals.foreach { v =>
      sigOf(v) match {
        case Some(s) if !s.isZST => slot(v, s)
        case _ =>
      }
    }

    // -- emission
    private val segment = new Segment(method)
    private val asm = new ForkedAssembler with com.huawei.excelsior.jet.compiler.cbc.CbcSymbolAdapter

    private val liveness = new LivenessInfoCollector
    private val exRegions = mutable.ArrayBuffer.empty[(Label, Label, Label)]
    private val handlerBlocks = mutable.HashSet.empty[CHIR.Block]
    private val handlerGetEx = mutable.HashMap.empty[CHIR.Block, CHIR.GetException.type]
    private val blockEndLabels = mutable.HashMap.empty[CHIR.Block, Label]
    private lazy val exceptionSig: SignatureType =
      SignatureType.fromSymType(resolver.findClass("std.core:Exception").get)

    private val blocks: Seq[CHIR.Block] = func.body.get.blocks

    // Scratch registers for load-op-store (all volatile: IR8+ are non-volatile).
    private val SCRATCH = IR1 // primary op result; newobj result is hardcoded IR1
    private val SCRATCH2 = IR2
    private val SCRATCH3 = IR3
    private val FSCRATCH0 = FR0
    private val FSCRATCH1 = FR1

    private def widthOf(sig: SignatureType): Width = sig.toAsm match {
      case com.huawei.excelsior.jet.assembler.AsmType.I64 | com.huawei.excelsior.jet.assembler.AsmType.U64 |
           com.huawei.excelsior.jet.assembler.AsmType.F64 | com.huawei.excelsior.jet.assembler.AsmType.PTR => Width.W64
      case _ => Width.W32
    }

    private def isFloat(sig: SignatureType): Boolean = sig match {
      case _: SignatureType.FloatingPoint => true
      case _ => false
    }

    private def cbcKind(sig: SignatureType): CbcTypeKind = sig match {
      case s if s.isTraceableReference => CbcTypeKind.REF
      case s: SignatureType.Record => CbcTypeKind.REC
      case s => CbcTypeKind(s.toAsm)
    }

    /** Registers the conservative GC state for the current position: all
      * reference-typed slots are live (sound: every value's home is a slot;
      * registers hold only transient scratch). Must be called right after an
      * op that can trigger GC (allocation, call). */
    private def saveGCMapHere(): Unit = {
      val refSlots = slotTypes.iterator.zipWithIndex.collect {
        case (s, idx) if s.isTraceableReference => StackSlot.Untyped(idx)
      }.toSeq
      liveness.saveResources(segment, refSlots, Seq.empty)
    }

    /** Loads a value into reg. LocalVars aliasing a defining expression read
      * that expression's slot; locals aliasing a literal materialize it. */
    private def loadToReg(v: CHIR.Value, reg: IR): Unit = {
      if (v == CHIR.GetException) {
        // the caught exception slot is created by the handler prologue
        asm.loadUntyped(reg, LoadAccessKind.from(CbcTypeKind.REF), StackSlot.Untyped(slotOfAny(CHIR.GetException, exceptionSig)))
        return
      }
      val s = sigOf(v).getOrElse(return fail(s"load of unknown value ${v.getClass.getSimpleName}"))
      keyOf(v) match {
        case l: CHIR.Literal => literalToReg(l, reg)
        case c: CHIR.Constant => loadToReg(c.literal, reg)
        case lv: CHIR.LocalVar =>
          if (s.isZST) () // nothing to load
          else asm.loadUntyped(reg, LoadAccessKind.from(cbcKind(s)), StackSlot.Untyped(slotOfAny(lv, s)))
        case p: CHIR.Parameter =>
          if (s.isZST) ()
          else asm.loadUntyped(reg, LoadAccessKind.from(cbcKind(s)), StackSlot.Untyped(slotOfAny(p, s)))
        case e: CHIR.Expression =>
          // defining expression of an aliased local: its slot was filled when
          // the expression was emitted
          if (s.isZST) ()
          else asm.loadUntyped(reg, LoadAccessKind.from(cbcKind(s)), StackSlot.Untyped(slotOfAny(e, s)))
        case other => fail(s"load of ${other.getClass.getSimpleName}")
      }
    }

    private def literalToReg(l: CHIR.Literal, reg: IR): Unit = l match {
      case CHIR.UnitLiteral => ()
      case l: CHIR.IntLiteral =>
        widthOf(sigOf(l).get) match {
          case Width.W32 => asm.movi32(reg, l.value.toInt)
          case _ => asm.movi64(reg, l.value)
        }
      case l: CHIR.BoolLiteral => asm.movi32(reg, if (l.value) 1 else 0)
      case l: CHIR.RuneLiteral => asm.movi64(reg, l.value)
      case _: CHIR.NullLiteral => asm.mov(reg, IRZ, reference = true)
      case l: CHIR.StringLiteral =>
        // String used as an operand: materialize once into a typed slot
        // (cached per literal), then load the record address from it.
        val sig = sigOf(l).getOrElse(stringSig)
        val idx = materializeStringSlot(l, sig)
        asm.loadUntyped(reg, LoadAccessKind.from(cbcKind(sig)), StackSlot.Untyped(idx))
      case other => fail(s"literal ${other.getClass.getSimpleName}")
    }

    /** Emits (once per literal) initConstString into a fresh typed slot and
      * stores the record address into an untyped slot keyed by the literal. */
    private def materializeStringSlot(lit: CHIR.StringLiteral, sig: SignatureType): Int = {
      val idx = slotOfAny(lit, sig)
      if (!slotStringDone.contains(lit)) {
        val ts = newTypedSlot(sig)
        asm.initConstString(ts, new ConstStringSymbol(lit.value))
        asm.ldstackrec(SCRATCH, ts)
        asm.storeUntyped(SCRATCH, StoreAccessKind.from(cbcKind(sig)), StackSlot.Untyped(idx))
        slotStringDone += lit
      }
      idx
    }

    /** Loads a float value into an FR register (from slot only; literals bail). */
    private def loadFloatToReg(v: CHIR.Value, reg: FR): Unit = {
      val sig = sigOf(v).getOrElse(return fail("float load of unknown value"))
      val stk = StackSlot.Untyped(slot(v, sig))
      sig match {
        case SignatureType.Float64 => asm.loadUntyped(reg, LoadAccessKind.LD_F64, stk)
        case SignatureType.Float32 => asm.loadUntyped(reg, LoadAccessKind.LD_F32, stk)
        case s => fail(s"float load of $s")
      }
    }

/** Stores the value in `reg` into the result slot of expression `e`: keyed by
  * the result var if present, else by the expression itself (aliased-local
  * reads). */
    private def storeResult(e: CHIR.Expression, reg: IR, sig: SignatureType): Unit = {
      if (sig.isZST) ()
      else resultOf(e) match {
        case Some(res) => storeFromReg(res, reg, sig)
        case None => asm.storeUntyped(reg, StoreAccessKind.from(cbcKind(sig)), StackSlot.Untyped(slotOfAny(e, sig)))
      }
    }

    private def storeFromReg(v: CHIR.Value, reg: IR, sig: SignatureType): Unit = v match {
      case _: CHIR.Literal => () // literals are re-materialized at each use
      case lv: (CHIR.LocalVar | CHIR.Parameter) =>
        if (sig.isZST) ()
        else asm.storeUntyped(reg, StoreAccessKind.from(cbcKind(sig)), StackSlot.Untyped(slot(lv, sig)))
      case g: CHIR.GlobalVar =>
        if (sig.isZST) ()
        else asm.storeUntyped(reg, StoreAccessKind.from(cbcKind(sig)), StackSlot.Untyped(slot(g, sig)))
      case other => fail(s"store to ${other.getClass.getSimpleName}")
    }

    private def storeFloatFromReg(v: CHIR.Value, reg: FR, sig: SignatureType): Unit = v match {
      case _: CHIR.Literal => ()
      case lv: (CHIR.LocalVar | CHIR.Parameter | CHIR.GlobalVar) =>
        val stk = StackSlot.Untyped(slot(lv, sig))
        sig match {
          case SignatureType.Float64 => asm.storeUntyped(reg, StoreAccessKind.ST_F64, stk)
          case SignatureType.Float32 => asm.storeUntyped(reg, StoreAccessKind.ST_F32, stk)
          case s => fail(s"float store of $s")
        }
      case other => fail(s"float store to ${other.getClass.getSimpleName}")
    }

    private def resultOf(e: CHIR.Expression): Option[CHIR.LocalVar] = e match {
      case hr: CHIR.HasResultVar => Some(hr.resultVar)
      case _ => None
    }

    def translate(): Boolean = {
      try {
        // Fast pre-scan: reject methods containing patterns we can never
        // translate BEFORE doing any slot allocation / emission work. This
        // makes hopeless methods cheap (large fallback populations otherwise
        // burn mirror-side analysis before failing).
        def scanFatal(t: CHIR.Terminator): Unit = ()
        def scanFatalExpr(e: CHIR.Expression): Unit = e match {
          case _: CHIR.Terminator | _: CHIR.Constant | _: CHIR.Apply | _: CHIR.Store |
               _: CHIR.Load | _: CHIR.Binary | _: CHIR.Unary | _: CHIR.NumericCast |
               _: CHIR.StaticCast | _: CHIR.Allocate | _: CHIR.Field |
               _: CHIR.GetElementRef | _: CHIR.StoreElementRef | _: CHIR.Intrinsic |
               _: CHIR.RawArrayAllocate | _: CHIR.RawArrayLiteralInit |
               _: CHIR.Invoke | CHIR.GetException | _: CHIR.Debug |
               _: CHIR.Tuple | _: CHIR.StringLiteral |
               _: CHIR.Box | _: CHIR.UnboxToValue |
               _: CHIR.RawArrayInitByValue | _: CHIR.InstanceOf |
               _: CHIR.GetRTTIStatic =>
            // individually translatable (or no-op); per-expr checks happen in
            // the translators
            ()
          case other => fail(s"expr ${other.getClass.getSimpleName}")
        }
        if (!bail_) {
          func.body.get.blocks.foreach { b =>
            b.expressions.foreach {
              case t: CHIR.Terminator => scanFatal(t)
              case e => scanFatalExpr(e)
            }
            scanFatal(b.terminator)
          }
        }
        asm.withSegment(segment) {
          // spill incoming params (they arrive in ABI registers; calls clobber volatiles)
          val abi = DirectCBCCompiler.platform(env).abi(method.getMethodType)
          val params = func.params
          val regLocs = abi.paramLocations.filter(l => l.isReg && l.asReg.isInstanceOf[IR])
          val zstParams = params.filter(p => sigOf(p).exists(_.isZST))
          if (zstParams.nonEmpty) fail("ZST param")
          else if (params.size > regLocs.size) fail("too many params")
          else {
            // Mirrors CHIRParser.parseEntryBlock: the receiver is the CHIR
            // params head and maps to the ABI's receiver slot; the remaining
            // params map from startSpecialParamsCount onward. End special
            // params (OuterTI) and Custom params (GenericFuncParams) occupy
            // trailing/middle locs and are ignored here (but remembered: the
            // generic ones are read as own TI sources).
            val mt = method.getMethodType
            val hasRcv = mt.hasReceiverParameter || mt.hasMutRecordParameter
            val rcvIdx = if (mt.hasReceiverParameter) mt.getReceiverArgIdx else mt.getMutRecordArgIdx
            val (rcv, rest) = if (hasRcv) (Some(params.head), params.tail) else (None, params)
            val start = mt.startSpecialParamsCount
            val genCount = if (mt.hasGenericFuncParams) method.getGenericInfo.constraints.size else 0
            // trailing end-special locs (OuterTI) exist but carry no CHIR param
            if (start + rest.size + genCount + mt.endSpecialParamsCount != regLocs.size)
              fail(s"special params: start=$start rest=${rest.size} gen=$genCount end=${mt.endSpecialParamsCount} regLocs=${regLocs.size}")
            else {
              ownParamLocs = regLocs.toSeq
              if (mt.hasOuterTypeInfoParameter)
                ownOuterTiLoc = Some(regLocs(mt.getOuterTypeInfoArgIdx).asReg.asInstanceOf[IR])
              if (mt.hasThisTypeInfoParameter)
                ownThisTiLoc = Some(regLocs(mt.getThisTypeInfoArgIdx).asReg.asInstanceOf[IR])
              touch(params: _*)
              rcv.foreach(p => storeFromReg(p, regLocs(rcvIdx).asReg.asInstanceOf[IR], sigOf(p).get))
              rest.zipWithIndex.foreach { (p, i) =>
                storeFromReg(p, regLocs(i + start).asReg.asInstanceOf[IR], sigOf(p).get)
              }
            }
          }
          if (!bail_) {
            blocks.foreach(translateBlock)
          }
        }
      } catch {
        case e: Throwable =>
          bail_ = true
          if (sys.env.contains("DIRECT_CBC_DEBUG")) {
            System.err.println(s"DIRECT-CBC error in $method: $e")
          }
      }
      !bail_
    }

    private def translateBlock(b: CHIR.Block): Unit = {
      // exception-handler bookkeeping: a block containing Try* exprs (or a
      // RaiseException with a local handler) gets an xtable region covering
      // the whole block, targeting the handler block
      val tryExprs = b.expressions.collect {
        case t: CHIR.Terminator with CHIR.HasSuccessors if t.successors.length == 2 => t
      }
      tryExprs.foreach { t =>
        val handler = t.successors(1)
        handlerBlocks += handler
        val endL = blockEndLabels.getOrElseUpdate(b, asm.newLabel)
        exRegions += ((blockLabel(b), endL, blockLabel(handler)))
      }
      if (handlerBlocks.contains(b) && !handlerGetEx.contains(b)) {
        handlerGetEx(b) = CHIR.GetException
        slotOfAny(CHIR.GetException, exceptionSig)
        asm.catchEx(SCRATCH)
        asm.storeUntyped(SCRATCH, StoreAccessKind.from(CbcTypeKind.REF), StackSlot.Untyped(slotOfAny(CHIR.GetException, exceptionSig)))
      }
      asm.bind(blockLabel(b))
      // NOTE: the serialized CHIR keeps the terminator as the LAST element of
      // `expressions` (BlockImpl.terminator = expressions.last), so handle a
      // terminator inside the loop and stop — the terminator field mirrors it.
      val it = b.expressions.iterator
      while (it.hasNext) {
        it.next() match {
          case t: CHIR.Terminator =>
            translateTerminator(t, b)
            bindBlockEnd(b)
            return
          case _: CHIR.Constant => () // literals materialized at use
          case e: CHIR.Apply => translateApply(e)
          case e: CHIR.Store => translateStore(e)
          case e: CHIR.Load => translateLoad(e)
          case e: CHIR.Binary => translateBinary(e)
          case e: CHIR.Unary => translateUnary(e)
          case e: CHIR.NumericCast => translateNumericCast(e)
          case e: CHIR.StaticCast => translateStaticCast(e)
          case e: CHIR.Allocate => translateAllocate(e)
          case e: CHIR.Field => translateField(e)
          case e: CHIR.GetElementRef => translateGetElementRef(e)
          case e: CHIR.StoreElementRef => translateStoreElementRef(e)
          case e: CHIR.Intrinsic => translateIntrinsic(e)
          case e: CHIR.Invoke => translateInvoke(e)
          case CHIR.GetException => () // catchEx emitted at handler start
          case e: CHIR.RawArrayAllocate => translateRawArrayAllocate(e)
          case e: CHIR.RawArrayLiteralInit => translateRawArrayLiteralInit(e)
          case e: CHIR.Tuple => translateTuple(e)
          case e: CHIR.Box => translateBox(e)
          case e: CHIR.UnboxToValue => translateUnbox(e)
          case e: CHIR.RawArrayInitByValue => translateRawArrayInitByValue(e)
          case e: CHIR.InstanceOf => translateInstanceOf(e)
          case e: CHIR.GetRTTIStatic =>
            // production: this-method's TI param (asserts hasThisTypeInfoParameter)
            if (!method.getMethodType.hasThisTypeInfoParameter)
              return fail("GetRTTIStatic: method has no ThisTypeInfo param")
            ownThisTiLoc match {
              case Some(loc) => storeResult(e, loc, SignatureType.ThisTypeInfo)
              case None => return fail("GetRTTIStatic: no own TI loc")
            }
          case e: CHIR.StringLiteral => translateStringLiteral(e, e)
          case _: CHIR.Debug => () // debug bookkeeping: no-op
          case e => return fail(s"expr ${e.getClass.getSimpleName}")
        }
      }
      translateTerminator(b.terminator, b)
      bindBlockEnd(b)
    }

    private def bindBlockEnd(b: CHIR.Block): Unit =
      blockEndLabels.remove(b).foreach(asm.bind)

    private val labels = mutable.HashMap.empty[CHIR.Block, Label]
    private def blockLabel(b: CHIR.Block): Label = labels.getOrElseUpdate(b, asm.newLabel)

    // ------------------------------------------------------------------
    // Load / Store
    // ------------------------------------------------------------------

    private def staticFieldOf(v: CHIR.GlobalVar): symlevel.Field = {
      val symRefType = v.declaringDef
        .map(d => symlevel.Type.asClassType(resolver.symType(d).get))
        .getOrElse(resolver.findClass(v.packageName).get)
      val name = resolver.symName(v)
      val sig = resolver.typeSig(v.tpe)
      val f = symRefType.findDeclaredFieldOrNull(xstr(name), sig)
      if (f == null) fail(s"static field not found: $name")
      f
    }

    private def translateStore(e: CHIR.Store): Unit = e.location match {
      case lv: (CHIR.LocalVar | CHIR.Parameter) =>
        val s = sigOf(lv).get
        if (s.isZST) return
        if (isFloat(s)) {
          loadFloatToReg(e.value, FSCRATCH0)
          storeFloatFromReg(lv, FSCRATCH0, s)
        } else {
          loadToReg(e.value, SCRATCH)
          storeFromReg(lv, SCRATCH, s)
        }
      case g: CHIR.GlobalVar =>
        val s = sigOf(g).get
        if (s.isZST) return
        if (isFloat(s)) return fail("static float store")
        val f = staticFieldOf(g)
        val fr = asm.adapter.field(symlevel.CangjieFieldReference(f, SignatureType.fromSymType(f.getDeclaringClass), f.getType))
        loadToReg(e.value, SCRATCH)
        asm.st(SCRATCH, fr)
      case _ => fail("store to non-local")
    }

    private def translateLoad(e: CHIR.Load): Unit = {
      // A Load's result type is the type of its location; readers reach it
      // through LocalVars aliased to this expression.
      val s = sigOf(e.location).getOrElse(return fail("load: unknown sig"))
      if (s.isZST) return
      if (isFloat(s)) return fail("float load")
      // The load's value flows to readers via LocalVars whose associatedExpr
      // is this Load (canonicalized to this expression's slot).
      slotOfAny(e, s) // null-init the result slot before any GC point
      val dst = StackSlot.Untyped(slotOfAny(e, s))
      e.location match {
        case lv: (CHIR.LocalVar | CHIR.Parameter) =>
          asm.loadUntyped(SCRATCH, LoadAccessKind.from(cbcKind(s)), StackSlot.Untyped(slot(lv, s)))
          asm.storeUntyped(SCRATCH, StoreAccessKind.from(cbcKind(s)), dst)
        case g: CHIR.GlobalVar =>
          val f = staticFieldOf(g)
          val fr = asm.adapter.field(symlevel.CangjieFieldReference(f, SignatureType.fromSymType(f.getDeclaringClass), f.getType))
          asm.ld(SCRATCH, fr)
          asm.storeUntyped(SCRATCH, StoreAccessKind.from(cbcKind(s)), dst)
        case _ => fail("load from non-local")
      }
    }

    // ------------------------------------------------------------------
    // Binary / Unary / Casts
    // ------------------------------------------------------------------

    private def translateBinary(e: CHIR.Binary): Unit = {
      val lsig = sigOf(e.leftOperand).getOrElse(return fail("binary: unknown left sig"))
      val resSig = resolver.typeSig(e.resultTpe)
      if (lsig.isZST) {
        // comparisons of ZST values are always true (see interpreter); other ZST ops are no-ops
        if (resSig == SignatureType.Boolean) {
          asm.movi32(SCRATCH, 1)
          resultOf(e) match {
            case Some(res) => storeFromReg(res, SCRATCH, resSig)
            case None => fail("comparison without result")
          }
        } else fail(s"binary over ZST $lsig")
        return
      }
      val w = widthOf(lsig)
      touch(e.leftOperand, e.rightOperand)
      resultOf(e).foreach(res => touch(res))

      def emitIntOp(): Unit = {
        val throwing = e.overflowStrategy == CHIR.OverflowStrategy.Throwing
        loadToReg(e.leftOperand, SCRATCH2)
        loadToReg(e.rightOperand, SCRATCH3)
        val (l, r) = (SCRATCH2, SCRATCH3)
        e.kind match {
          case CHIR.Binary.Kind.Add => if (throwing) asm.cadd(SCRATCH, l, r, w) else asm.add(w, SCRATCH, l, r)
          case CHIR.Binary.Kind.Sub => if (throwing) asm.csub(SCRATCH, l, r, w) else asm.sub(w, SCRATCH, l, r)
          case CHIR.Binary.Kind.Mul => if (throwing) asm.cmul(SCRATCH, l, r, w) else asm.mul(w, SCRATCH, l, r)
          case CHIR.Binary.Kind.Div =>
            asm.divisorCheck(r)
            if (throwing) asm.cdiv(SCRATCH, l, r, w)
            else if (lsig.isInstanceOf[SignatureType.Integral] && !lsig.asInstanceOf[SignatureType.Integral].signed) asm.udiv(w, SCRATCH, l, r)
            else asm.div(w, SCRATCH, l, r)
          case CHIR.Binary.Kind.Mod =>
            asm.divisorCheck(r)
            if (lsig.isInstanceOf[SignatureType.Integral] && !lsig.asInstanceOf[SignatureType.Integral].signed) asm.urem(w, SCRATCH, l, r)
            else asm.rem(w, SCRATCH, l, r)
          case CHIR.Binary.Kind.LShift => maskShift(w); asm.lsl(w, SCRATCH, l, r)
          case CHIR.Binary.Kind.RShift =>
            val signed = lsig match {
              case i: SignatureType.Integral => i.signed
              case _ => true
            }
            maskShift(w)
            if (signed) asm.asr(w, SCRATCH, l, r) else asm.lsr(w, SCRATCH, l, r)
          case CHIR.Binary.Kind.And => asm.and(w, SCRATCH, l, r)
          case CHIR.Binary.Kind.Or => asm.or(w, SCRATCH, l, r)
          case CHIR.Binary.Kind.Xor => asm.xor(w, SCRATCH, l, r)
          case other => fail(s"binary int ${other}")
        }
      }

      def maskShift(w: Width): Unit = {
        val mask = if (w == Width.W32) 31 else 63
        asm.andi(w, SCRATCH3, SCRATCH3, mask)
      }

      def emitFloatOp(): Unit = {
        loadFloatToReg(e.leftOperand, FSCRATCH0)
        loadFloatToReg(e.rightOperand, FSCRATCH1)
        e.kind match {
          case CHIR.Binary.Kind.Add => asm.fadd(w, FSCRATCH0, FSCRATCH0, FSCRATCH1)
          case CHIR.Binary.Kind.Sub => asm.fsub(w, FSCRATCH0, FSCRATCH0, FSCRATCH1)
          case CHIR.Binary.Kind.Mul => asm.fmul(w, FSCRATCH0, FSCRATCH0, FSCRATCH1)
          case CHIR.Binary.Kind.Div => asm.fdiv(w, FSCRATCH0, FSCRATCH0, FSCRATCH1)
          case other => return fail(s"binary float $other")
        }
        // result now in FSCRATCH0; caller stores it
        resultOf(e) match {
          case Some(res) => storeFloatFromReg(res, FSCRATCH0, resSig)
          case None => fail("float binary without result")
        }
      }

      if (resSig == SignatureType.Boolean) {
        // comparison
        val cc = e.kind match {
          case CHIR.Binary.Kind.Lt => BranchOp.LT
          case CHIR.Binary.Kind.Gt => BranchOp.GT
          case CHIR.Binary.Kind.Le => BranchOp.LE
          case CHIR.Binary.Kind.Ge => BranchOp.GE
          case CHIR.Binary.Kind.Eq => BranchOp.EQ
          case CHIR.Binary.Kind.NotEq => BranchOp.NE
          case other => return fail(s"comparison $other")
        }
        def unsigned: Boolean = lsig match {
          case i: SignatureType.Integral => !i.signed
          case _ => false
        }
        val realCc = cc match {
          case BranchOp.LT if unsigned => BranchOp.ULT
          case BranchOp.GT if unsigned => BranchOp.UGT
          case BranchOp.LE if unsigned => BranchOp.ULE
          case BranchOp.GE if unsigned => BranchOp.UGE
          case op => op
        }
        if (isFloat(lsig)) {
          loadFloatToReg(e.leftOperand, FSCRATCH0)
          loadFloatToReg(e.rightOperand, FSCRATCH1)
          asm.scc(realCc, SCRATCH, FSCRATCH0, FSCRATCH1, w)
        } else {
          loadToReg(e.leftOperand, SCRATCH2)
          loadToReg(e.rightOperand, SCRATCH3)
          asm.scc(realCc, SCRATCH, SCRATCH2, SCRATCH3, w)
        }
        resultOf(e) match {
          case Some(res) => storeFromReg(res, SCRATCH, resSig)
          case None => fail("comparison without result")
        }
      } else if (isFloat(resSig)) {
        emitFloatOp()
      } else {
        emitIntOp()
        resultOf(e) match {
          case Some(res) => storeFromReg(res, SCRATCH, resSig)
          case None => fail("binary without result")
        }
      }
    }

    private def translateUnary(e: CHIR.Unary): Unit = {
      val sig = resolver.typeSig(e.resultTpe)
      val w = widthOf(sig)
      touch(e.operand)
      resultOf(e).foreach(res => touch(res))
      e.kind match {
        case CHIR.Unary.Kind.Neg =>
          if (isFloat(sig)) {
            loadFloatToReg(e.operand, FSCRATCH0)
            asm.fneg(FSCRATCH0, FSCRATCH0, w)
            resultOf(e) match {
              case Some(res) => storeFloatFromReg(res, FSCRATCH0, sig)
              case None => fail("unary without result")
            }
          } else {
            val throwing = e.overflowStrategy == CHIR.OverflowStrategy.Throwing
            loadToReg(e.operand, SCRATCH2)
            if (throwing) asm.cneg(SCRATCH, SCRATCH2, w) else asm.neg(SCRATCH, SCRATCH2, w)
            resultOf(e) match {
              case Some(res) => storeFromReg(res, SCRATCH, sig)
              case None => fail("unary without result")
            }
          }
        case CHIR.Unary.Kind.BitNot =>
          val mask = if (w == Width.W32) -1 else -1
          loadToReg(e.operand, SCRATCH2)
          asm.xori(w, SCRATCH, SCRATCH2, mask)
          resultOf(e) match {
            case Some(res) => storeFromReg(res, SCRATCH, sig)
            case None => fail("unary without result")
          }
        case CHIR.Unary.Kind.Not =>
          // boolean negation
          loadToReg(e.operand, SCRATCH2)
          asm.xori(Width.W32, SCRATCH, SCRATCH2, 1)
          storeResult(e, SCRATCH, sig)
        case null => fail("unary null")
      }
    }

    private def translateNumericCast(e: CHIR.NumericCast): Unit = {
      val from = sigOf(e.value).getOrElse(return fail("cast: unknown source sig"))
      val to = resolver.typeSig(e.targetTpe)
      if (e.overflowStrategy == CHIR.OverflowStrategy.Saturating) return fail("saturating cast")
      touch(e.value)
      resultOf(e).foreach(res => touch(res))

      def emitCast(): Unit = (from, to) match {
        case (f: SignatureType.Integral, t: SignatureType.Integral) =>
          loadToReg(e.value, SCRATCH2)
          val argW = if (f.bits <= 32) Width.W32 else Width.W64
          val resW = if (t.bits <= 32) Width.W32 else Width.W64
          asm.bfx(SCRATCH, SCRATCH2, resW, argW, f.signed, 0, f.bits min t.bits)
        // rune casts: UnicodeChar32 is a 32-bit unsigned reinterpret
        case (f: SignatureType.Integral, SignatureType.UnicodeChar32) =>
          loadToReg(e.value, SCRATCH2)
          asm.bfx(SCRATCH, SCRATCH2, Width.W32, Width.W32, f.signed, 0, f.bits min 32)
        case (SignatureType.UnicodeChar32, t: SignatureType.Integral) =>
          loadToReg(e.value, SCRATCH2)
          val resW = if (t.bits <= 32) Width.W32 else Width.W64
          asm.bfx(SCRATCH, SCRATCH2, resW, Width.W32, false, 0, 32 min t.bits)
        case (SignatureType.UnicodeChar32, SignatureType.UnicodeChar32) =>
          loadToReg(e.value, SCRATCH)
        // float <-> integral conversions (production: ValueConvert -> asm.convert)
        case (f: SignatureType.Integral, t: SignatureType.FloatingPoint) =>
          loadToReg(e.value, SCRATCH2)
          asm.convert(com.huawei.excelsior.jet.assembler.AsmType.F32, if (f.bits <= 32) com.huawei.excelsior.jet.assembler.AsmType.I32 else com.huawei.excelsior.jet.assembler.AsmType.I64, SCRATCH, SCRATCH2)
        case (f: SignatureType.FloatingPoint, t: SignatureType.Integral) =>
          loadToReg(e.value, SCRATCH2)
          asm.convert(if (t.bits <= 32) com.huawei.excelsior.jet.assembler.AsmType.I32 else com.huawei.excelsior.jet.assembler.AsmType.I64, com.huawei.excelsior.jet.assembler.AsmType.F32, SCRATCH, SCRATCH2)
        case _ => fail(s"numeric cast $from -> $to")
      }
      emitCast()
      // A result-less cast can still be read later through an aliased
      // LocalVar (keyOf maps the local to this expr) — materialize its slot.
      storeResult(e, SCRATCH, to)
    }

    private def translateStaticCast(e: CHIR.StaticCast): Unit = {
      val from = sigOf(e.value)
      val to = resolver.typeSig(e.targetTpe)
      if (from.exists(_.isZST) || to.isZST) return
      touch(e.value)
      resultOf(e).foreach(res => touch(res))
      if (from.contains(to)) {
        loadToReg(e.value, SCRATCH)
        // A result-less cast can still be read later through an aliased
        // LocalVar (keyOf maps the local to this expr) — materialize its slot.
        storeResult(e, SCRATCH, to)
      } else {
        val f = from.getOrElse(to)
        (f, to) match {
        // Production CHIRParser casts Reference->Reference via CheckCast(trusted)
        // and Record->Record / Record->Tuple via ReinterpretCast: both are
        // identity ops at CBC level (register keeps the same address/word).
        case (_: SignatureType.Reference | _: SignatureType.InstantiatedReference,
              _: SignatureType.Reference | _: SignatureType.InstantiatedReference) =>
          loadToReg(e.value, SCRATCH); storeResult(e, SCRATCH, to)
        case (_: SignatureType.Record | _: SignatureType.InstantiatedRecord,
              _: SignatureType.Record | _: SignatureType.InstantiatedRecord | _: SignatureType.Tuple) =>
          loadToReg(e.value, SCRATCH); storeResult(e, SCRATCH, to)
        case (fe @ SignatureType.OptionLikeEnum(_, _, x), SignatureType.Tuple(Seq(SignatureType.Boolean, y))) =>
          // Erasure: recursive options widen payload to std.core:Object.
          assert(x == y || (x == SignatureType.Reference("std.core:Object", jbc = false) && y.isTraceableReference), s"cast from $fe to $to")
          loadToReg(e.value, SCRATCH); storeResult(e, SCRATCH, to)
        case (_: SignatureType.ClassBasedEnum, _: SignatureType.Tuple) =>
          // EnumCast: payload address unchanged
          loadToReg(e.value, SCRATCH); storeResult(e, SCRATCH, to)
        case (_: SignatureType.UnionBasedEnum, _: SignatureType.Tuple) =>
          // ReinterpretCast: payload address unchanged
          loadToReg(e.value, SCRATCH); storeResult(e, SCRATCH, to)
        case _ =>
          fail(s"static cast $from -> $to")
        }
      }
    }

    // ------------------------------------------------------------------
    // Allocation
    // ------------------------------------------------------------------

    private def translateAllocate(e: CHIR.Allocate): Unit = {
      val sig = resolver.typeSig(e.allocatedType)
      if (sig.isZST) return
      if (sig.isTraceableReference) {
        slotOfAny(e, sig) // null-init before newobj's GC point
        val allocType = sig match { case SignatureType.Box(t) => t; case t => t }
        asm.newobj(allocType.toCbc)
        saveGCMapHere()
        asm.storeUntyped(SCRATCH, StoreAccessKind.from(cbcKind(sig)), StackSlot.Untyped(slotOfAny(e, sig)))
      } else if (sig.isVariableSizeType) {
        fail("variable size allocation")
      } else if (sig.isPrimitive) {
        // zero value
        asm.movi32(SCRATCH, 0)
        asm.storeUntyped(SCRATCH, StoreAccessKind.from(cbcKind(sig)), StackSlot.Untyped(slotOfAny(e, sig)))
      } else if (sig.isRecord) {
        // Production CHIRParser.Allocate for records: StackAlloc.Local(sig,
        // zeroed) — a zeroed typed frame slot. Record value = its address.
        val ts = newTypedSlot(sig)
        if (sig.hasRefFields) asm.zerorefs(ts)
        asm.ldstackrec(SCRATCH, ts)
        storeResult(e, SCRATCH, sig)
      } else {
        fail(s"allocate $sig")
      }
    }

    // ------------------------------------------------------------------
    // Field access
    // ------------------------------------------------------------------

    /** Replicates CHIRParser.fieldChain: walk `path` over field indices,
      * with the interpreter's special cases for lambdas, tuples and
      * Option-like enums. Each step: (pathIdx, field-or-null, refType, fieldType). */
    private def fieldChain(host: SignatureType, path: Seq[Long]): Seq[(Long, symlevel.Field, SignatureType, SignatureType)] = {
      val out = mutable.ArrayBuffer.empty[(Long, symlevel.Field, SignatureType, SignatureType)]
      var refType = host
      path.foreach { idx =>
        val fieldType: SignatureType = refType match {
          case refType if refType.isCangjieLambda =>
            // first two fields are synthesized lambda function pointers
            val i = (idx + 2).toInt
            val refClass = symlevel.Type.asClassType(refType)
            val allClassFields = (refClass +: refClass.getSuperClasses.toArray).reverse.flatMap(_.getDeclaredFields).filterNot(_.isStatic).toArray
            if (i >= allClassFields.length) { fail(s"lambda field index $i out of range"); return Seq.empty }
            allClassFields(i).getType.instantiate(genericParamsOf(refType), Seq.empty)
          case t: SignatureType.Tuple =>
            if (idx >= t.params.length) { fail(s"tuple index $idx out of range"); return Seq.empty }
            t.params(idx.toInt)
          case t: SignatureType.OptionLikeEnum =>
            idx match {
              case 0 => SignatureType.Boolean
              case 1 => t.someType
              case _ => { fail(s"option index $idx"); return Seq.empty }
            }
          case _ =>
            val refClass = symlevel.Type.asClassType(refType)
            val allClassFields = (refClass +: refClass.getSuperClasses.toArray).reverse.flatMap(_.getDeclaredFields).filterNot(_.isStatic).toArray
            if (idx >= allClassFields.length) { fail(s"field index $idx out of range"); return Seq.empty }
            val f = allClassFields(idx.toInt)
            f.getType.instantiate(genericParamsOf(refType), Seq.empty)
        }
        // named field only for class-like hosts; tuples/options use index refs
        val field: symlevel.Field = refType match {
          case _: (SignatureType.Tuple | SignatureType.OptionLikeEnum) => null
          case _ if refType.isCangjieLambda =>
            val refClass = symlevel.Type.asClassType(refType)
            val allClassFields = (refClass +: refClass.getSuperClasses.toArray).reverse.flatMap(_.getDeclaredFields).filterNot(_.isStatic).toArray
            allClassFields((idx + 2).toInt)
          case _ =>
            val refClass = symlevel.Type.asClassType(refType)
            val allClassFields = (refClass +: refClass.getSuperClasses.toArray).reverse.flatMap(_.getDeclaredFields).filterNot(_.isStatic).toArray
            allClassFields(idx.toInt)
        }
        out += ((idx, field, refType, fieldType))
        refType = fieldType
      }
      out.toSeq
    }

    private def genericParamsOf(refType: SignatureType): Seq[SignatureType] = refType match {
      case x: SignatureType.InstantiatedType => x.instantiatedTypeParameters
      case x: SignatureType.CangjieEnum => x.params
      case _ => Seq.empty
    }

    private def fieldRef(pathIdx: Long, f: symlevel.Field, refType: SignatureType, fieldType: SignatureType) = {
      // Mirrors CodeGeneratorCBC.indexReference: a ConstIndex ref on an
      // OptionLikeEnum host must declare the tuple (Boolean, payload) layout.
      val refT = refType match {
        case t: SignatureType.OptionLikeEnum => SignatureType.Tuple(Seq(SignatureType.Boolean, t.someType))
        case t => t
      }
      asm.adapter.field {
        if (f != null) symlevel.CangjieFieldReference(pathIdx, f, refT, fieldType)
        else symlevel.CangjieIndexReference(pathIdx, refT, fieldType)
      }
    }

    /** Loads the field chain value into SCRATCH: walks each field ref. */
    /** Field hosts: traceable references (class instances), boxed records
      * (e.g. std.core:Array<T> objects), and stack-allocated records
      * (Tuple/Option/NamedRecord) whose chain contains only primitive or
      * reference fields — nested records would need copy semantics. */
    private def canFieldHost(sig: SignatureType, chain: => Seq[(Long, symlevel.Field, SignatureType, SignatureType)]): Boolean =
      sig.isTraceableReference ||
        (sig.isRecord && !sig.isVariableSizeType && !sig.isZST &&
          chain.forall { case (_, _, _, ft) => ft.isPrimitive || ft.isTraceableReference || (ft.isRecord && !ft.isVariableSizeType) })

    private def translateField(e: CHIR.Field): Unit = {
      val hostSig = sigOf(e.base).getOrElse(return fail("field: unknown host sig"))
      lazy val preChain = fieldChain(hostSig, e.path)
      if (!canFieldHost(hostSig, preChain)) return fail(s"field on non-ref host $hostSig")
      touch(e.base)
      resultOf(e).foreach(res => touch(res))
      val chain = fieldChain(hostSig, e.path)
      if (chain.isEmpty) return
      val lastType = chain.last._3
      if (lastType.isZST) return

      // load base ref
      loadToReg(e.base, SCRATCH2)
      asm.nullcheck(SCRATCH2)
      emitFieldChainLoad(chain, StackSlot.Untyped(slotOfAny(e, lastType)))
    }

    /** Walks the chain from the base ref in SCRATCH2, leaving the last field
      * value in SCRATCH and storing it into `dst`. */
    private def emitFieldChainLoad(chain: Seq[(Long, symlevel.Field, SignatureType, SignatureType)], dst: StackSlot.Untyped): Unit = {
      var i = 0
      while (i < chain.length) {
        val (pidx, f, rt, ft) = chain(i)
        val isLast = i == chain.length - 1
        val fr = fieldRef(pidx, f, rt, ft)
        if (isLast) {
          asm.ld(SCRATCH, SCRATCH2, fr)
          asm.storeUntyped(SCRATCH, StoreAccessKind.from(cbcKind(ft)), dst)
        } else {
          asm.ld(SCRATCH3, SCRATCH2, fr)
          asm.mov(SCRATCH2, SCRATCH3, reference = false)
        }
        i += 1
      }
    }

    private def translateGetElementRef(e: CHIR.GetElementRef): Unit = {
      val hostSig = sigOf(e.base).getOrElse(return fail("getElementRef: unknown host sig"))
      if (hostSig.isArray) return fail("array GetElementRef")
      lazy val preChain = fieldChain(hostSig, e.path)
      if (!canFieldHost(hostSig, preChain)) return fail(s"getElementRef on non-ref host $hostSig")
      touch(e.base)
      resultOf(e).foreach(res => touch(res))
      val chain = fieldChain(hostSig, e.path)
      if (chain.isEmpty) return
      val lastType = chain.last._3
      if (lastType.isZST) return
      loadToReg(e.base, SCRATCH2)
      asm.nullcheck(SCRATCH2)
      emitFieldChainLoad(chain, StackSlot.Untyped(slotOfAny(e, lastType)))
    }

    private def translateStoreElementRef(e: CHIR.StoreElementRef): Unit = {
      val hostSig = sigOf(e.location).getOrElse(return fail("storeElementRef: unknown host sig"))
      lazy val preChain = fieldChain(hostSig, e.path)
      if (!canFieldHost(hostSig, preChain)) return fail(s"storeElementRef on non-ref host $hostSig")
      // array element stores need starr + a bounds-check XSite: not supported here
      if (hostSig.isArray) return fail("array StoreElementRef")
      touch(e.location, e.value)
      val chain = fieldChain(hostSig, e.path)
      if (chain.isEmpty) return
      val lastType = chain.last._3
      if (lastType.isZST) return
      loadToReg(e.location, SCRATCH2)
      asm.nullcheck(SCRATCH2)
      var i = 0
      while (i < chain.length - 1) {
        val (pidx, f, rt, ft) = chain(i)
        val fr = fieldRef(pidx, f, rt, ft)
        asm.ld(SCRATCH3, SCRATCH2, fr)
        asm.mov(SCRATCH2, SCRATCH3, reference = false)
        i += 1
      }
      val (pidx, f, rt, ft) = chain.last
      val fr = fieldRef(pidx, f, rt, ft)
      loadToReg(e.value, SCRATCH)
      asm.st(SCRATCH, SCRATCH2, fr)
    }

    // ------------------------------------------------------------------
    // Intrinsics (raw-array ops)
    // ------------------------------------------------------------------

    private def translateIntrinsic(e: CHIR.Intrinsic): Unit = {
      import CHIR.Intrinsic.Kind
      e.kind match {
        case Kind.BeginCatch =>
          // identity of the exception value (caught at handler start;
          // mirrors CHIRParser: state(e) = state(ex))
          val Seq(ex) = e.args
          touch(ex)
          resultOf(e).foreach(res => touch(res))
          loadToReg(ex, SCRATCH)
          storeResult(e, SCRATCH, exceptionSig)
        case Kind.Abs | Kind.Fabs =>
          // abs: if (arg < 0) -arg else arg (production lowers CHIR Abs to
          // exactly this); float args use the FABS instruction
          val Seq(x) = e.args
          val rsig = resultOf(e).flatMap(sigOf).orElse(sigOf(x)).getOrElse(return fail("abs: unknown sig"))
          touch(x)
          resultOf(e).foreach(res => touch(res))
          if (rsig match { case _: SignatureType.FloatingPoint => true; case _ => false }) {
            loadFloatToReg(x, FSCRATCH1)
            asm.fabs(FSCRATCH0, FSCRATCH1, Width.W64)
            resultOf(e).foreach(res => storeFloatFromReg(res, FSCRATCH0, rsig))
          } else {
            loadToReg(x, SCRATCH2)
            val negLab = asm.newLabel
            val done = asm.newLabel
            asm.bcc(BranchOp.LT, SCRATCH2, IRZ, Width.W64, negLab)
            resultOf(e).foreach(res => storeFromReg(res, SCRATCH2, rsig))
            asm.jmp(done)
            asm.bind(negLab)
            asm.neg(SCRATCH, SCRATCH2, Width.W64)
            resultOf(e).foreach(res => storeFromReg(res, SCRATCH, rsig))
            asm.bind(done)
          }
        case Kind.ArrayGet | Kind.ArrayGetUnchecked | Kind.ArrayGetRefUnchecked =>
          val Seq(arrV, idxV) = e.args
          val arrType = sigOf(arrV).getOrElse(return fail("arrayGet: unknown array sig"))
          if (!arrType.isArray) return fail(s"arrayGet on non-array $arrType")
          val elem = arrType.getArrayElemType
          if (elem.isZST) return
          if (elem.isVariableSizeType) return fail("arrayGet of variable-size elem")
          touch(arrV, idxV)
          resultOf(e).foreach(res => touch(res))
          loadToReg(arrV, SCRATCH2)
          asm.nullcheck(SCRATCH2)
          loadToReg(idxV, SCRATCH3)
          if (elem.isRecord) {
            // Record elements: `index` computes the element address (gc unsafe
            // — mirrors production genArrayGet); the value is the address.
            asm.index(SCRATCH, SCRATCH2, SCRATCH3, arrType.toCbc)
            saveGCMapHere()
          } else {
            loadArrayElem(elem)
          }
          if (isFloat(elem)) {
            storeFloatFromReg(resultOf(e).getOrElse(e.asInstanceOf[CHIR.Value]), FSCRATCH0, elem)
          } else {
            storeResult(e, SCRATCH, elem)
          }
        case Kind.ArraySet | Kind.ArraySetUnchecked =>
          val Seq(arrV, idxV, valV) = e.args
          val arrType = sigOf(arrV).getOrElse(return fail("arraySet: unknown array sig"))
          if (!arrType.isArray) return fail(s"arraySet on non-array $arrType")
          val elem = arrType.getArrayElemType
          if (elem.isZST) return
          if (elem.isRecord) return fail("arraySet of record elem")
          if (elem.isVariableSizeType) return fail("arraySet of variable-size elem")
          touch(arrV, idxV, valV)
          loadToReg(arrV, SCRATCH2)
          asm.nullcheck(SCRATCH2)
          loadToReg(idxV, SCRATCH3)
          if (elem.isTraceableReference) {
            loadToReg(valV, SCRATCH)
            asm.starrObj(SCRATCH2, SCRATCH3, SCRATCH)
          } else if (isFloat(elem)) {
            loadFloatToReg(valV, FSCRATCH0)
            asm.starr(elem.toAsm, SCRATCH2, SCRATCH3, FSCRATCH0)
          } else {
            loadToReg(valV, SCRATCH)
            asm.starr(elem.toAsm, SCRATCH2, SCRATCH3, SCRATCH)
          }
        case Kind.ArraySize =>
          val arrV = e.args.head
          val arrType = sigOf(arrV).getOrElse(return fail("arraySize: unknown array sig"))
          if (!arrType.isArray) return fail(s"arraySize on non-array $arrType")
          touch(arrV)
          resultOf(e).foreach(res => touch(res))
          loadToReg(arrV, SCRATCH2)
          asm.nullcheck(SCRATCH2)
          asm.lenarr(SCRATCH, SCRATCH2)
          storeResult(e, SCRATCH, resolver.typeSig(e.resultTpe))
        case Kind.ObjectZeroValue =>
          // zero-initialized value of the result type (mirrors CHIRParser arm:
          // StackAlloc.Local for records, null for refs, zero register else)
          val sig = resolver.typeSig(e.resultTpe)
          resultOf(e).foreach(res => touch(res))
          if (sig.isZST) return
          if (sig.isRecord && !sig.isVariableSizeType) {
            val ts = newTypedSlot(sig)
            if (sig.hasRefFields) asm.zerorefs(ts)
            asm.ldstackrec(SCRATCH, ts)
            storeResult(e, SCRATCH, sig)
          } else if (sig.isVariableSizeType || sig.isTraceableReference) {
            storeResult(e, IRZ, sig)
          } else if (isFloat(sig)) {
            resultOf(e) match {
              case Some(res) => storeFloatFromReg(res, FSCRATCH0, sig) // FSCRATCH0 is zero
              case None => ()
            }
          } else {
            asm.movi64(SCRATCH, 0)
            storeResult(e, SCRATCH, sig)
          }
        case other => fail(s"intrinsic $other")
      }
    }

    private def translateRawArrayAllocate(e: CHIR.RawArrayAllocate): Unit = {
      val elem = resolver.typeSig(e.elementType)
      if (elem.isZST) return
      if (elem.isVariableSizeType) return fail("raw array of variable-size elem")
      val arrayType = SignatureType.CangjieArray(elem)
      touch(e.size)
      slotOfAny(e, arrayType) // null-init before newarr's GC point
      loadToReg(e.size, IR2)
      asm.newarr(arrayType.toCbc) // result hardcoded IR1
      saveGCMapHere()
      storeResult(e, IR1, arrayType)
    }

    /** String literal: materialize a stack-allocated String record initialized
      * from the method's constant-string pool; the value is the record's
      * address (CBC record convention). */
    private lazy val stringSig: SignatureType =
      SignatureType.fromSymType(resolver.findClass("std.core:String").get)

    private def translateStringLiteral(e0: CHIR.Expression, lit: CHIR.StringLiteral): Unit = {
      val sig = sigOf(lit).getOrElse(stringSig)
      val ts = newTypedSlot(sig)
      asm.initConstString(ts, new ConstStringSymbol(lit.value))
      asm.ldstackrec(SCRATCH, ts)
      storeResult(e0, SCRATCH, sig)
    }

    /** Tuple construction: stack-allocate a record for the tuple and store
      * each element via a const-index field reference. */
    private def translateTuple(e: CHIR.Tuple): Unit = {
      val resSig = resolver.typeSig(e.resultTpe)
      val tupleSig: SignatureType = resSig match {
        case t: SignatureType.Tuple => t
        case opt: SignatureType.OptionLikeEnum =>
          // CHIR materializes Option construction as a 2-tuple (tag, payload);
          // the storage layout is the option enum itself. Production
          // CHIRParser (CHIR Tuple / EnumType): nullable Some/None collapse to
          // the payload or null; non-nullable stack-allocate the enum record.
          if (opt.isNullableOption) {
            val vals = e.elementValues
            def tagValue(v: CHIR.Value): Option[Long] = keyOf(v) match {
              case c: CHIR.Constant => c.literal match {
                case t: CHIR.IntLiteral => Some(t.value)
                case t: CHIR.BoolLiteral => Some(if (t.value) 1 else 0)
                case _ => None
              }
              case t: CHIR.IntLiteral => Some(t.value)
              case t: CHIR.BoolLiteral => Some(if (t.value) 1 else 0)
              case _ => None
            }
            if (vals.size == 2 && tagValue(vals(0)).nonEmpty) {
              val tag = tagValue(vals(0)).get
              val payload = vals(1)
              assert(tag == 0 || tag == 1, tag)
              touch(e.elementValues: _*)
              loadToReg(payload, SCRATCH)
              storeResult(e, SCRATCH, opt.someType)
              return
            }
            // None: single tag element (production `Seq(IConst(c)) => Null()`)
            if (vals.size == 1 && tagValue(vals(0)).nonEmpty) {
              touch(e.elementValues: _*)
              storeResult(e, IRZ, opt.someType)
              return
            }
            return fail(s"option tuple: unexpected element values n=${vals.size} tag=${keyOf(vals.headOption.getOrElse(null)).getClass.getSimpleName} lit=${vals.map(keyOf).headOption.collect{case c: CHIR.Constant => c.literal.getClass.getSimpleName}.mkString}")
          } else if (opt.someType.isVariableSizeType) {
            return fail("variable-size option")
          } else {
            opt
          }
        case other => return fail(s"tuple with non-tuple sig $other")
      }
      if (tupleSig.isVariableSizeType) return fail("variable-size tuple")
      if (tupleSig match {
            case t: SignatureType.Tuple => t.params.length != e.elementValues.length
            case _: SignatureType.OptionLikeEnum => e.elementValues.isEmpty || e.elementValues.length > 2
            case _ => true
          })
        return fail(s"tuple arity ${e.elementValues.length} vs $tupleSig")
      val ts = newTypedSlot(tupleSig)
      if (tupleSig.hasRefFields) asm.zerorefs(ts)
      touch(e.elementValues: _*)
      val refSig = tupleSig.toCbc
      def storeElem(i: Int, v: CHIR.Value, elemSig: SignatureType): Unit = {
        if (elemSig.isZST) return
        val fr = com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.ConstIndexFieldReference(refSig, i, elemSig.toCbc)
        if (isFloat(elemSig)) {
          loadFloatToReg(v, FSCRATCH0)
          asm.st(FSCRATCH0, ts, fr)
        } else {
          loadToReg(v, SCRATCH)
          asm.st(SCRATCH, ts, fr)
        }
      }
      tupleSig match {
        case t: SignatureType.Tuple =>
          e.elementValues.zipWithIndex.foreach { (v, i) => storeElem(i, v, t.params(i)) }
        case _: SignatureType.OptionLikeEnum =>
          // OptionLikeEnum layout: field 0 = Boolean tag, field 1 = payload.
          // A single element is tag-only (None / Some-with-no-payload form,
          // production `Seq(IConst(c)) => StoreFieldSeq(tag)`).
          e.elementValues.zipWithIndex.foreach { (v, i) =>
            storeElem(i, v, if (i == 0) SignatureType.Boolean else tupleSig.asInstanceOf[SignatureType.OptionLikeEnum].someType)
          }
        case _ => fail(s"tuple with non-tuple sig $tupleSig")
      }
      // the tuple/option value is the address of its buffer (CBC record convention)
      asm.ldstackrec(SCRATCH, ts)
      storeResult(e, SCRATCH, tupleSig)
    }

    /** Box: allocate a heap box holding `value` (mirrors CHIRParser's CHIR.Box
      * arm + genBox; passthrough when the value is already a heap object). */
    private def translateBox(e: CHIR.Box): Unit = {
      val v = e.value
      val baseType = sigOf(v).getOrElse(return fail("box: unknown value sig"))
      val resSig = resolver.typeSig(e.targetTpe)
      touch(v)
      if (baseType.isVariableSizeType || baseType.isTraceableReference ||
        (baseType match {
          case o: SignatureType.OptionLikeEnum => o.someType.isVariableSizeType
          case _ => false
        })) {
        // passthrough: value is already a heap object
        loadToReg(v, SCRATCH)
        storeResult(e, SCRATCH, resSig)
        return
      }
      loadToReg(v, SCRATCH)
      if (baseType.containsTypeVariables) lowerTypeInfo(SCRATCH2, baseType)
      asm.box(SCRATCH, SCRATCH3, baseType.toCbc)
      saveGCMapHere()
      storeResult(e, SCRATCH3, resSig)
    }

    /** UnboxToValue/CastToConcrete (mirrors CHIRParser's arm + genUnbox):
      * passthrough unless the operand is a heap box holding a value type. */
    private def translateUnbox(e: CHIR.UnboxToValue): Unit = {
      val baseType = resolver.typeSig(e.targetTpe)
      if (baseType.isZST) return
      val v = e.value
      touch(v)
      val operandIsBox = sigOf(v).exists(s =>
        s.isTraceableReference || s.isInstanceOf[SignatureType.Box])
      val passthrough =
        (baseType.isRecord && (baseType match {
          case o: SignatureType.OptionLikeEnum => o.someType.isTypeVariable
          case _ => false
        })) || baseType.isTraceableReference ||
        !operandIsBox
      if (passthrough) {
        loadToReg(v, SCRATCH)
        storeResult(e, SCRATCH, baseType)
      } else {
        loadToReg(v, SCRATCH2)
        asm.unbox(SCRATCH, SCRATCH2, baseType.toCbc)
        storeResult(e, SCRATCH, baseType)
      }
    }

    /** RawArrayInitByValue: fill array elements with a value (mirrors
      * CHIRParser arm + lowerAJArrayFill): loop storing each element.
      * Records are copied into their inline element slots. */
    private def translateRawArrayInitByValue(e: CHIR.RawArrayInitByValue): Unit = {
      val arrType = sigOf(e.array).getOrElse(return fail("rawArrayInit: unknown array sig"))
      if (!arrType.isArray) return fail(s"rawArrayInit on non-array $arrType")
      val elem = arrType.getArrayElemType
      if (elem.isZST) return
      if (elem.isVariableSizeType) return fail("rawArrayInit of variable-size elem")
      touch(e.array, e.size, e.initValue)
      loadToReg(e.array, SCRATCH2)
      asm.nullcheck(SCRATCH2)
      // loop index in IR4, array length in IR5
      asm.movi32(IR4, 0)
      loadToReg(e.size, IR5)
      val loop = asm.newLabel
      val done = asm.newLabel
      asm.bind(loop)
      asm.bcc(BranchOp.UGE, IR4, IR5, Width.W64, done)
      if (elem.isRecord) {
        loadToReg(e.initValue, SCRATCH)
        asm.index(SCRATCH3, SCRATCH2, IR4, arrType.toCbc)
        asm.copy(SCRATCH2, SCRATCH3, SCRATCH2, SCRATCH, elem.toCbc)
        saveGCMapHere()
      } else {
        if (elem.isTraceableReference) {
          loadToReg(e.initValue, SCRATCH)
          asm.starrObj(SCRATCH2, IR4, SCRATCH)
        } else if (isFloat(elem)) {
          loadFloatToReg(e.initValue, FSCRATCH0)
          asm.starr(elem.toAsm, SCRATCH2, IR4, FSCRATCH0)
        } else {
          loadToReg(e.initValue, SCRATCH)
          asm.starr(elem.toAsm, SCRATCH2, IR4, SCRATCH)
        }
      }
      asm.addi(Width.W64, IR4, IR4, 1)
      asm.jmp(loop)
      asm.bind(done)
    }

    /** InstanceOf: type test (mirrors CHIRParser arm + genInstanceOf). */
    private def translateInstanceOf(e: CHIR.InstanceOf): Unit = {
      val tpe = resolver.typeSig(e.testType)
      touch(e.obj)
      resultOf(e).foreach(res => touch(res))
      val resSig = SignatureType.Boolean
      if (tpe.isTraceableReference) {
        loadToReg(e.obj, SCRATCH2)
        asm.isInstanceOf(SCRATCH, SCRATCH2, tpe.toCbc)
        storeResult(e, SCRATCH, resSig)
      } else {
        // non-reference test type: box the operand first (production Box arm);
        // only value-type operands reach here
        val objSig = sigOf(e.obj).getOrElse(return fail("instanceOf: unknown obj sig"))
        if (objSig.isVariableSizeType || objSig.isTraceableReference) {
          loadToReg(e.obj, SCRATCH2)
        } else {
          loadToReg(e.obj, SCRATCH)
          if (objSig.containsTypeVariables) lowerTypeInfo(SCRATCH2, objSig)
          asm.box(SCRATCH, SCRATCH2, objSig.toCbc)
          saveGCMapHere()
        }
        asm.isInstanceOf(SCRATCH, SCRATCH2, tpe.toCbc)
        storeResult(e, SCRATCH, resSig)
      }
    }

    private def translateRawArrayLiteralInit(e: CHIR.RawArrayLiteralInit): Unit = {
      val arrType = sigOf(e.array).getOrElse(return fail("literalInit: unknown array sig"))
      if (!arrType.isArray) return fail(s"literalInit on non-array $arrType")
      val elem = arrType.getArrayElemType
      if (elem.isZST) return
      if (elem.isVariableSizeType) return fail("literalInit of variable-size elem")
      touch(e.array)
      loadToReg(e.array, SCRATCH2)
      asm.nullcheck(SCRATCH2)
      e.elementValues.zipWithIndex.foreach { (v, i) =>
        if (!sigOf(v).forall(_.isZST)) {
          if (elem.isRecord) {
            // record elements live inline: compute the element address (Index)
            // and copy the record value into it (mirrors CHIRParser.arrayPut
            // needsCopy arm)
            loadToReg(v, SCRATCH)
            asm.movi32(SCRATCH3, i)
            asm.index(SCRATCH3, SCRATCH2, SCRATCH3, arrType.toCbc)
            asm.copy(SCRATCH2, SCRATCH3, SCRATCH2, SCRATCH, elem.toCbc)
            saveGCMapHere()
          } else {
            loadToReg(v, SCRATCH)
            asm.movi32(SCRATCH3, i)
            if (elem.isTraceableReference) asm.starrObj(SCRATCH2, SCRATCH3, SCRATCH)
            else if (isFloat(elem)) {
              loadFloatToReg(v, FSCRATCH0)
              asm.starr(elem.toAsm, SCRATCH2, SCRATCH3, FSCRATCH0)
            } else asm.starr(elem.toAsm, SCRATCH2, SCRATCH3, SCRATCH)
          }
        }
      }
    }

    /** Array element at [SCRATCH3] of array in SCRATCH2 -> SCRATCH (or FSCRATCH0). */
    private def loadArrayElem(elem: SignatureType): Unit = {
      if (elem.isTraceableReference) {
        asm.ldarrObj(SCRATCH, SCRATCH2, SCRATCH3)
      } else if (isFloat(elem)) {
        asm.ldarr(elem.toAsm, FSCRATCH0, SCRATCH2, SCRATCH3)
      } else {
        asm.ldarr(elem.toAsm, SCRATCH, SCRATCH2, SCRATCH3)
      }
    }

    // ------------------------------------------------------------------
    // Calls
    // ------------------------------------------------------------------

    private def translateApply(e: CHIR.Apply): Unit = {
      val f: CHIR.Func = e.callee
      val calleeMethod = findCallee(f)
      if (calleeMethod == null) return fail(s"callee not found: ${resolver.symName(f)}")

      // v13002 CHIR: Apply.args includes the receiver at the head and
      // `thisArg` is just args.head (derived), so never prepend it.
      val mak = if (calleeMethod.isStatic) MAK.STATIC
                else if (calleeMethod.isCangjieMut) MAK.MUT
                else MAK.SPECIAL
      val lparams = e.instantiatedTypeArgs.map(resolver.typeSig)
      val ref =
        if (lparams.isEmpty) new MethodReference(calleeMethod, mak, CompiledType(calleeMethod.getDeclaringClass))
        else if (!calleeMethod.hasUniversalGenericContext) return fail("generic apply: callee lacks universal generic context")
        else new InstantiatedMethodReference(calleeMethod, mak, lparams,
          SignatureType.fromSymType(calleeMethod.getDeclaringClass), None)
      emitCall(e, calleeMethod, e.args, ref, isVirtual = false)
    }

    /** Virtual/static dispatch through the CHIR vtable (mirrors CHIRParser's
      * `case e: CHIR.Invoke`). */
    private def translateInvoke(e: CHIR.Invoke): Unit = {
      if (e.instantiatedTypeArgs.nonEmpty) return fail("generic invoke")
      val methodArgVal = e.callee
      val argVals = e.args
      val isStatic = argVals.headOption.exists {
        case x: CHIR.LocalVar => x.associatedExpr match {
          case _: (CHIR.GetRTTI | CHIR.GetRTTIStatic) => true
          case _ => false
        }
        case _ => false
      }
      val (thisTypeArgVal, sourceArgVals) = if (isStatic) (argVals.headOption, argVals.tail) else (None, argVals)
      val thisType = resolver.typeSig(e.thisType) match {
        case SignatureType.ThisTypeInfo =>
          val c = method.getDeclaringClass
          if (c.isCangjieExtend) c.getCangjieExtendInfo
          else SignatureType.fromSymType(c)
        case t: SignatureType.TypeVariable =>
          val v = if (isStatic) thisTypeArgVal.get else sourceArgVals.head
          sigOf(v).getOrElse(return fail("invoke: unknown this sig"))
        case t => t
      }
      val name = resolver.symName(methodArgVal)
      val (gsig, _, _, _) = resolver.functionSig(methodArgVal.tpe, hasReceiver = !isStatic)
      val vtable = symlevel.Type.asClassType(thisType).getCHIRVTable
      if (vtable == null) return fail("invoke: no vtable")
      def findSlot(extDef: CHIRVTable.ExtDef): Option[(CHIRVTable.ExtDef, Int)] = {
        val vnum = extDef.funcTable.indexWhere(m => m.name == name && m.originalSig.instantiate(genericParamsOf(thisType), Seq.empty) == gsig)
        Option.when(vnum >= 0)((extDef, vnum))
      }
      val (extDef, vnum) = vtable.extDefs.collectFirst(Function.unlift(findSlot)).getOrElse {
        return fail(s"invoke: vtable slot not found: $name")
      }
      val refType = extDef.extType.instantiate(genericParamsOf(thisType), Seq.empty)
      val refClass = symlevel.Type.asClassType(refType)
      val calleeMethod = refClass.findDeclaredMethodOrNull(xstr(name), gsig)
      if (calleeMethod == null) return fail(s"invoke: method not found: $name")
      val mak = if (isStatic) MAK.STATIC else MAK.VIRTUAL
      val ref = new MethodReference(calleeMethod, mak, CompiledType(refType), vnum)

      emitCall(e, calleeMethod, sourceArgVals, ref, isVirtual = !isStatic)
    }

    /** Shared call emission: ABI layout checks, arg register loads, optional
      * outer-TI load, the call itself and result store.
      *
      * CHIR call args INCLUDE the receiver at the head (v13002: thisArg ==
      * args.head). The callee's start specials are consumed as follows:
      *   - RetByVal: not a CHIR arg — the caller supplies the return-buffer
      *     address (IRZ for ZST returns, which the callee never writes);
      *   - Receiver: consumes callArgs.head;
      * the remaining CHIR args map onto the ordinary parameter locations. */
    private def emitCall(e: CHIR.Expression, calleeMethod: Method, callArgs: Seq[CHIR.Value],
                         ref: MethodReference, isVirtual: Boolean): Unit = {
      val calleeMt = calleeMethod.getMethodType
      val endCount = calleeMt.endSpecialParamsCount
      val outerTiSig = if (calleeMt.hasReceiverParameter) sigOf(callArgs.head).get
                       else SignatureType.fromSymType(calleeMethod.getDeclaringClass)
      val outerTi = calleeMt.hasOuterTypeInfoParameter &&
        lowerableTypeInfo(outerTiSig)
      if (endCount != 0 && !outerTi)
        return fail(f"callee end special params (TI: end=$endCount outer=${calleeMt.hasOuterTypeInfoParameter} this=${calleeMt.hasThisTypeInfoParameter} tv=${outerTiSig.containsTypeVariables})")
      val calleeStart = calleeMt.startSpecialParamsCount
      val retSig = calleeMt.returnType
      val retIsZST = retSig.isZST
      val retIsRecord = retSig.isRecord && !retSig.isVariableSizeType
      val retIsBoxed = retSig.isVariableSizeType ||
        (calleeMt.hasRetByValParameter && {
          val abiRet = calleeMt.parameterType(calleeMt.getRetByValArgIdx)
          abiRet.isInstanceOf[SignatureType.Box] || abiRet == SignatureType.Address
        })
      if (!(calleeStart == 0 ||
            (calleeStart == 1 && calleeMt.hasReceiverParameter && !calleeMt.hasRetByValParameter) ||
            (calleeStart == 1 && calleeMt.hasRetByValParameter && !calleeMt.hasReceiverParameter &&
              (retIsZST || retIsRecord || retIsBoxed)) ||
            (calleeStart == 2 && calleeMt.hasRetByValParameter && calleeMt.hasReceiverParameter &&
              (retIsZST || retIsRecord || retIsBoxed)) ||
            (calleeStart == 2 && !calleeMt.hasRetByValParameter && retIsZST &&
              calleeMt.hasMutRecordParameter && calleeMt.hasMutObjectParameter)))
        return fail(s"callee special params start=$calleeStart (sret=${calleeMt.hasRetByValParameter} ret=${calleeMt.returnType} specials=${calleeMt.specialParameters.elements.mkString(",")})")
      val calleeAbi = DirectCBCCompiler.platform(env).abi(calleeMt)
      val locs = calleeAbi.paramLocations
      val regLocs = locs.filter(l => l.isReg && l.asReg.isInstanceOf[IR])
      // Stack (tail) parameters: the caller stores each tail arg into the
      // tail area of its own frame and passes the area's base address in the
      // tail register (production: initTailRegister in CallGenerator;
      // callee reads back via LoadTailParam = load [tailReg, n*8]).
      val tailLocs = locs.filter(l => !l.isReg)
      if (tailLocs.nonEmpty && tailLocs.size > 4) return fail("too many stack args")
      val tailBase: Option[IR] = if (tailLocs.isEmpty) None else Some {
        // Tail area = a typed record-shaped frame slot; its address goes into
        // the tail register (production: initTailRegister — lea tailReg,
        // [sp + stackParamsStartOffset]; callee reads via LoadTailParam =
        // load [tailReg, n * stackSlotSize]).
        val areaSig = SignatureType.Tuple(tailLocs.map(_ => SignatureType.Int64: SignatureType).toList)
        val ts = newTypedSlot(areaSig)
        val tailReg = DirectCBCCompiler.platform(env).tailRegister
        asm.ldstackrec(tailReg, ts)
        tailReg
      }
      // ThisTypeInfo end special: instance callees get the receiver's runtime
      // TI (asm.loadTypeInfoObj); static callees get the static TI of thisType
      // (mirrors CHIRParser.callMethod's ThisTypeInfo arm).
      val thisTi: Option[(IR, Either[IR, SignatureType])] =
        if (!calleeMt.hasThisTypeInfoParameter) None
        else if (calleeMt.hasReceiverParameter) {
          val rcvLoc = locs(calleeStart - 1).asReg.asInstanceOf[IR]
          Some((locs(calleeMt.getThisTypeInfoArgIdx).asReg.asInstanceOf[IR], Left(rcvLoc)))
        } else {
          val tiSig = if (outerTiSig.containsTypeVariables) sigOf(callArgs.head).get else outerTiSig
          Some((locs(calleeMt.getThisTypeInfoArgIdx).asReg.asInstanceOf[IR], Right(tiSig)))
        }
      // GenericFuncParams (Custom position) sit between the ordinary args and
      // the end specials — exclude them from the ordinary-arg locations.
      val genParamCount = calleeMt.hasGenericFuncParams match {
        case true => ref match {
          case imr: InstantiatedMethodReference => imr.instantiatedTypeParameters.size
          case _ => fail("generic func params on non-instantiated reference"); 0
        }
        case false => 0
      }
      val argLocs = locs.slice(calleeStart, locs.size - endCount - genParamCount)
      val firstTailIdx = argLocs.indexWhere(l => !l.isReg)
      // receiver: last start special; its loc sits right before the params
      val recvLoc: Option[IR] =
        if (calleeMt.hasReceiverParameter) Some(locs(calleeStart - 1).asReg.asInstanceOf[IR])
        else None
      // SMutRecord/SMutObject start specials: both receive the record
      // receiver's address (SMutRecArg/SMutObjectArg are address markers in
      // production; the entry side treats the SMutRecord param as the receiver).
      val mutLocs: Seq[IR] =
        if (calleeMt.hasMutRecordParameter && !calleeMt.hasReceiverParameter) {
          val base = calleeStart - (if (calleeMt.hasMutObjectParameter) 2 else 1)
          if (calleeMt.hasMutObjectParameter)
            Seq(locs(base).asReg.asInstanceOf[IR], locs(base + 1).asReg.asInstanceOf[IR])
          else Seq(locs(base).asReg.asInstanceOf[IR])
        } else Seq.empty
      val (recvVal, paramArgs) =
        if (calleeMt.hasReceiverParameter || calleeMt.hasMutRecordParameter) (Some(callArgs.head), callArgs.tail)
        else (None, callArgs)
      val realArgs = paramArgs.filter(a => sigOf(a).forall(!_.isZST))
      if (realArgs.size != argLocs.size) {
        if (sys.env.contains("DIRECT_CBC_ARGDUMP")) {
          System.err.println(s"[argdump] callee=${calleeMethod.getName} start=$calleeStart end=$endCount mut=${calleeMt.hasMutRecordParameter}/${calleeMt.hasMutObjectParameter} recv=${calleeMt.hasReceiverParameter} sret=${calleeMt.hasRetByValParameter} callArgs=${callArgs.map(a => sigOf(a).map(_.toString).getOrElse("?"))} ret=${calleeMt.returnType}")
        }
        return fail(s"arg count ${realArgs.size} vs ${argLocs.size}")
      }
      // pre-create (null-init) all value slots BEFORE clobbering arg registers
      recvVal.foreach(r => sigOf(r) match { case Some(s) if !s.isZST => slot(r, s); case _ => })
      realArgs.foreach(a => sigOf(a) match { case Some(s) if !s.isZST => slot(a, s); case _ => })
      resultOf(e).foreach(res => sigOf(res) match { case Some(s) if !s.isZST => slot(res, s); case _ => })
      // sret pointer (RetByVal): ZST returns are never written by the callee,
      // so IRZ (null) is a safe placeholder; record returns get a zeroed typed
      // slot whose address is passed to the callee
      val sretSlot: Option[StackSlot.Typed] = if (calleeMt.hasRetByValParameter) {
        val sretLoc = locs(0).asReg.asInstanceOf[IR]
        val abiRet = calleeMt.parameterType(calleeMt.getRetByValArgIdx)
        abiRet match {
          case b: SignatureType.Box =>
            // Variable-sized (boxed) return: callee writes a Box*; allocate it
            // on the heap like production (New(box) in callMethod RetByVal arm).
            asm.newobj(b.toCbc)
            asm.mov(sretLoc, IR1, reference = true)
            saveGCMapHere()
            None
          case SignatureType.Address if !retIsRecord =>
            // Type-variable sret: production stores the boxed value into a
            // freshly allocated Object; caller value = the Box* we passed.
            asm.newobj(SignatureType.Box(retSig).toCbc)
            asm.mov(sretLoc, IR1, reference = true)
            saveGCMapHere()
            None
          case _ if retIsRecord =>
            val ts = newTypedSlot(retSig)
            if (retSig.hasRefFields) asm.zerorefs(ts)
            asm.ldstackrec(sretLoc, ts) // address of the buffer
            Some(ts)
          case _ =>
            asm.mov(sretLoc, IRZ, reference = true)
            None
        }
      } else None
      recvVal.zip(recvLoc).foreach { (r, loc) => loadToReg(r, loc) }
      mutLocs.foreach { loc => recvVal.foreach(r => loadToReg(r, loc)) }
      realArgs.zip(argLocs).foreach { (a, loc) =>
        val sig = sigOf(a).get
        loc match {
          case l if l.isReg =>
            l.asReg match {
              case fr: FR => loadFloatToReg(a, fr)
              case ir: IR => loadToReg(a, ir)
            }
          case _ =>
            // stack (tail) arg: store into the tail area via raw memory store
            val i = argLocs.indexOf(loc) - firstTailIdx
            tailBase.foreach { base =>
              sig match {
                case SignatureType.Float32 =>
                  loadFloatToReg(a, FSCRATCH0)
                  asm.storeRawMemory(FSCRATCH0, base, StoreAccessKind.from(CbcTypeKind.F32), i * 8)
                case SignatureType.Float64 =>
                  loadFloatToReg(a, FSCRATCH0)
                  asm.storeRawMemory(FSCRATCH0, base, StoreAccessKind.from(CbcTypeKind.F64), i * 8)
                case s =>
                  loadToReg(a, SCRATCH)
                  asm.storeRawMemory(SCRATCH, base, StoreAccessKind.from(cbcKind(s)), i * 8)
              }
            }
        }
      }
      if (outerTi)
        lowerTypeInfo(locs(calleeMt.getOuterTypeInfoArgIdx).asReg.asInstanceOf[IR], outerTiSig)
      thisTi.foreach { (loc, src) =>
        src match {
          case Left(rcvLoc) => asm.loadTypeInfoObj(loc, rcvLoc) // receiver's runtime TI
          case Right(sig)   => lowerTypeInfo(loc, sig)          // static TI (possibly generic)
        }
      }
      // GenericFuncParams (universal-generic callee): load the TI of each
      // instantiated type parameter (mirrors callMethod's GenericFuncParams arm)
      ref match {
        case imr: InstantiatedMethodReference if calleeMt.hasGenericFuncParams =>
          imr.instantiatedTypeParameters.zipWithIndex.foreach { (t, i) =>
            val loc = locs(calleeMt.getGenericFuncParamsStartIdx(imr.instantiatedTypeParameters.size) + i)
            lowerTypeInfo(loc.asReg.asInstanceOf[IR], t)
          }
        case _ =>
      }
      val resultReg = {
        val rl = calleeAbi.resultLocation
        if (rl != null && rl.isReg && rl.asReg.isInstanceOf[IR]) rl.asReg.asInstanceOf[IR] else IR1
      }
      if (isVirtual) asm.callVirt(resultReg, ref) else asm.callDirect(resultReg, ref)
      saveGCMapHere()
      sretSlot.foreach { ts =>
        // the record value is the address of its buffer (CBC record convention)
        asm.ldstackrec(SCRATCH, ts)
        storeResult(e, SCRATCH, retSig)
      }
      resultOf(e) match {
        case Some(res) if !sigOf(res).forall(_.isZST) && sretSlot.isEmpty =>
          if (calleeMt.hasRetByValParameter && retIsBoxed) {
            // boxed sret: the value is the Box* we passed (in sretLoc register)
            val sretLoc = locs(0).asReg.asInstanceOf[IR]
            storeFromReg(res, sretLoc, sigOf(res).get)
          } else {
            storeFromReg(res, resultReg, sigOf(res).get)
          }
        case _ =>
      }
    }

    /** Whether a runtime-TI value can be materialized for this sig
      * (mirrors the arms of CHIRParser.loadTypeInfo). */
    private def lowerableTypeInfo(t: SignatureType): Boolean =
      !t.containsTypeVariables || (t match {
        case _: SignatureType.ClassTypeVariable | _: SignatureType.LocalTypeVariable => true
        case _: SignatureType.InstantiatedType | _: SignatureType.ArraySlice |
             _: SignatureType.CangjieArray | _: SignatureType.VArray |
             _: SignatureType.Tuple | _: SignatureType.CangjieEnum => true
        case _ => false
      })

    /** Materializes the runtime TypeInfo of `t` into `dst` (mirrors
      * CHIRParser.loadTypeInfo + CodeGeneratorCBC lowering):
      *  - no typevars: loadTypeInfoSig;
      *  - class/local typevar: typeArg(outer TI / own generic param);
      *  - instantiated composite: loadTypeInfoGeneric(uninstantiated) + typeArgs. */
    private def lowerTypeInfo(dst: IR, t: SignatureType): Unit = {
      if (!t.containsTypeVariables) {
        asm.loadTypeInfoSig(dst, t.toCbc)
        return
      }
      t match {
        case tv: SignatureType.ClassTypeVariable =>
          // TI of a class type parameter: extract from the outer TI
          val outerTiLoc = ownOuterTiLoc.getOrElse(
            return fail("typevar TI: method has no outer TI param"))
          asm.typeArg(outerTiLoc, tv.idx, dst)
        case tv: SignatureType.LocalTypeVariable =>
          // TI of the method's own type parameter: passed as a generic func param
          val genCount = method.getGenericInfo.constraints.size
          val loc = ownParamLocs.lift(
            method.getMethodType.getGenericFuncParamsStartIdx(genCount) + tv.idx)
            .getOrElse(return fail(s"typevar TI: no own generic param idx ${tv.idx}"))
          asm.mov(dst, loc.asReg.asInstanceOf[IR], reference = true)
        case _ =>
          asm.loadTypeInfoGeneric(dst, t.uninstantiated.toCbc)
          val params: Seq[SignatureType] = t match {
            case x: SignatureType.InstantiatedType => x.instantiatedTypeParameters
            case x: SignatureType.ArraySlice => Seq(x.elemType)
            case x: SignatureType.CangjieArray => Seq(x.elemType)
            case x: SignatureType.VArray => Seq(x.elemType)
            case x: SignatureType.Tuple => x.params
            case x: SignatureType.CangjieEnum => x.params
            case x => return fail(s"typevar TI: unexpected sig $x")
          }
          // typeArg's TI source must survive: load each arg into SCRATCH3 (a
          // volatile scratch distinct from dst/outerTiLoc) then extract
          params.zipWithIndex.foreach { (p, i) =>
            lowerTypeInfo(SCRATCH3, p)
            asm.typeArg(SCRATCH3, i, SCRATCH3)
            if (i == params.length - 1) asm.mov(dst, SCRATCH3, reference = true)
          }
      }
    }

    private def findCallee(f: CHIR.Func): Method = {
      val declSymType = f.declaringDef match {
        case Some(d) => resolver.symType(d).get
        case None => resolver.findClass(f.packageName).get
      }
      val declClass = declSymType.asInstanceOf[symlevel.ClassType]
      val modifiers = resolver.symModifiers(f)
      val hasReceiver = f.declaringDef.isDefined && !modifiers.contains(com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.STATIC)
      val (sig, _, _, _) = resolver.functionSig(f, hasReceiver)
      declClass.findDeclaredMethodOrNull(xstr(resolver.symName(f)), sig)
    }

    private def methodRef(m: Method): MethodReference = {
      val mak = if (m.isStatic) MAK.STATIC
                else if (m.isCangjieMut) MAK.MUT
                else MAK.SPECIAL
      new MethodReference(m, mak, CompiledType(m.getDeclaringClass))
    }

    // ------------------------------------------------------------------
    // Terminators
    // ------------------------------------------------------------------

    private def translateTerminator(t: CHIR.Terminator, b: CHIR.Block): Unit = t match {
      case CHIR.Exit =>
        func.retVal match {
          case Some(rv) if !sigOf(rv).exists(_.isZST) =>
            loadToReg(rv, IR1)
            asm.ret(IR1, Width.W64)
          case _ =>
            asm.ret(IRZ, Width.W64)
        }
      case g: CHIR.Goto =>
        asm.jmp(blockLabel(g.destination))
      case br: CHIR.Branch =>
        loadToReg(br.condition, SCRATCH)
        asm.bcc(BranchOp.NE, SCRATCH, IRZ, Width.W32, blockLabel(br.trueBlock))
        asm.jmp(blockLabel(br.falseBlock))
      case r: CHIR.RaiseException =>
        loadToReg(r.exceptionValue, IR1)
        asm.throwEx(IR1)
        bindBlockEnd(b)
      case t: CHIR.Terminator with CHIR.HasSuccessors if t.successors.length == 2 =>
        // Try* expression terminator: translate the operation, then jump to
        // the normal successor; the xtable region routes throws to the handler
        val normal = t.successors(0)
        t match {
          case a: CHIR.Apply => translateApply(a)
          case i: CHIR.Invoke => translateInvoke(i)
          case n: CHIR.Intrinsic => translateIntrinsic(n)
          case b2: CHIR.Binary => translateBinary(b2)
          case u: CHIR.Unary => translateUnary(u)
          case c: CHIR.NumericCast => translateNumericCast(c)
          case al: CHIR.Allocate => translateAllocate(al)
          case ra: CHIR.RawArrayAllocate => translateRawArrayAllocate(ra)
          case other => fail(s"try ${other.getClass.getSimpleName}")
        }
        if (!bail_) {
          asm.jmp(blockLabel(normal))
          bindBlockEnd(b)
        }
      case m: CHIR.MultiBranch =>
        // table switch lowered as a compare chain (production lowerSwitch uses
        // bisected ifs on CBC)
        loadToReg(m.condition, IR4)
        val pairs = m.caseValues.zip(m.normalBlocks)
        val defaultLbl = blockLabel(m.defaultBlock)
        pairs.zipWithIndex.foreach { case ((cv, blk), i) =>
          val next = if (i == pairs.length - 1) defaultLbl else asm.newLabel
          asm.bcc(BranchOp.NE, IR4, cv, Width.W64, next)
          asm.jmp(blockLabel(blk))
          if (i != pairs.length - 1) asm.bind(next)
        }
        bindBlockEnd(b)
      case other => fail(s"terminator ${other.getClass.getSimpleName}")
    }

    def send(): Unit = {
      val xgen = new XTableGenerator(method, (slot: Frame.Slot) => slot.offset)(env)
      val packed = xgen.packXInfo(new XInfo, Seq.empty)
      val exTable = ExceptionTable(exRegions.map { case (s, e, t) => ExceptionTable.RegionRef(s, e, t) }.toSeq)
      CBCFileGenerator.sendCode(
        method, segment, 0, packed, exTable, liveness.collect,
        /* tailParamCount */ 0, /* untypedStackSlotsCount */ slotTypes.length,
        /* usedNonVolIRegsMask */ 0, /* usedNonVolFRegsMask */ 0, /* maxCalleeStackArgsCount */ 0,
        /* mayHaveNativeCalls */ false,
        /* stackAllocatedTypeSigs */ typedSigs.toList, /* variableSizeTypes */ Seq.empty)
    }
  }
}

object DirectCBCCompiler {
  private var cachedPlatform: PlatformCBC = null

  def platform(env: Environment): PlatformCBC = {
    if (cachedPlatform == null) synchronized {
      if (cachedPlatform == null) {
        cachedPlatform = new PlatformCBC(com.huawei.excelsior.jet.compiler.Env.targetOS,
          com.huawei.excelsior.jet.compiler.Env.targetArch, isStandalone = true)
      }
    }
    cachedPlatform
  }
}
