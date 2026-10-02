/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.direct

import com.huawei.excelsior.jet.assembler.{Label, Segment, Width}
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
import com.huawei.excelsior.jet.compiler.symlevel.{Method, MethodReference, MethodReferenceAccessKind as MAK, SignatureType}
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
      case other => fail(s"literal ${other.getClass.getSimpleName}")
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
        def scanFatal(t: CHIR.Terminator): Unit = t match {
          case _: CHIR.TryApply => fail(s"terminator ${t.getClass.getSimpleName}")
          case _ =>
        }
        def scanFatalExpr(e: CHIR.Expression): Unit = e match {
          case _: CHIR.Terminator | _: CHIR.Constant | _: CHIR.Apply | _: CHIR.Store |
               _: CHIR.Load | _: CHIR.Binary | _: CHIR.Unary | _: CHIR.NumericCast |
               _: CHIR.StaticCast | _: CHIR.Allocate | _: CHIR.Field |
               _: CHIR.GetElementRef | _: CHIR.StoreElementRef | _: CHIR.Intrinsic |
               _: CHIR.RawArrayAllocate | _: CHIR.RawArrayLiteralInit |
               _: CHIR.Invoke | _: CHIR.Debug =>
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
            // params (OuterTI) occupy trailing locs and are ignored here.
            val mt = method.getMethodType
            val hasRcv = mt.hasReceiverParameter || mt.hasMutRecordParameter
            val rcvIdx = if (mt.hasReceiverParameter) mt.getReceiverArgIdx else mt.getMutRecordArgIdx
            val (rcv, rest) = if (hasRcv) (Some(params.head), params.tail) else (None, params)
            val start = mt.startSpecialParamsCount
            // trailing end-special locs (OuterTI) exist but carry no CHIR param
            if (start + rest.size + mt.endSpecialParamsCount != regLocs.size)
              fail(s"special params: start=$start rest=${rest.size} end=${mt.endSpecialParamsCount} regLocs=${regLocs.size}")
            else {
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
      asm.bind(blockLabel(b))
      // NOTE: the serialized CHIR keeps the terminator as the LAST element of
      // `expressions` (BlockImpl.terminator = expressions.last), so handle a
      // terminator inside the loop and stop — the terminator field mirrors it.
      val it = b.expressions.iterator
      while (it.hasNext) {
        it.next() match {
          case t: CHIR.Terminator =>
            translateTerminator(t)
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
          case e: CHIR.RawArrayAllocate => translateRawArrayAllocate(e)
          case e: CHIR.RawArrayLiteralInit => translateRawArrayLiteralInit(e)
          case _: CHIR.Debug => () // debug bookkeeping: no-op
          case e => return fail(s"expr ${e.getClass.getSimpleName}")
        }
      }
      translateTerminator(b.terminator)
    }

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
        fail(s"static cast $from -> $to")
      }
    }

    // ------------------------------------------------------------------
    // Allocation
    // ------------------------------------------------------------------

    private def translateAllocate(e: CHIR.Allocate): Unit = {
      val sig = resolver.typeSig(e.allocatedType)
      if (sig.isZST) return
      slotOfAny(e, sig) // null-init before newobj's GC point
      if (sig.isTraceableReference) {
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

    private def fieldRef(pathIdx: Long, f: symlevel.Field, refType: SignatureType, fieldType: SignatureType) =
      asm.adapter.field {
        if (f != null) symlevel.CangjieFieldReference(pathIdx, f, refType, fieldType)
        else symlevel.CangjieIndexReference(pathIdx, refType, fieldType)
      }

    /** Loads the field chain value into SCRATCH: walks each field ref. */
    /** Field hosts: traceable references (class instances) and boxed records
      * (e.g. std.core:Array<T> objects) whose chain contains only primitive
      * or reference fields — nested records would need copy semantics. */
    private def canFieldHost(sig: SignatureType, chain: => Seq[(Long, symlevel.Field, SignatureType, SignatureType)]): Boolean =
      sig.isTraceableReference ||
        (sig.isRecord && !sig.isVariableSizeType && !sig.isZST &&
          chain.forall { case (_, _, _, ft) => ft.isPrimitive || ft.isTraceableReference })

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
        case Kind.ArrayGet | Kind.ArrayGetUnchecked | Kind.ArrayGetRefUnchecked =>
          val Seq(arrV, idxV) = e.args
          val arrType = sigOf(arrV).getOrElse(return fail("arrayGet: unknown array sig"))
          if (!arrType.isArray) return fail(s"arrayGet on non-array $arrType")
          val elem = arrType.getArrayElemType
          if (elem.isZST) return
          if (elem.isVariableSizeType) return fail("arrayGet of variable-size elem")
          if (elem.isRecord) return fail("arrayGet of record elem")
          touch(arrV, idxV)
          resultOf(e).foreach(res => touch(res))
          loadToReg(arrV, SCRATCH2)
          asm.nullcheck(SCRATCH2)
          loadToReg(idxV, SCRATCH3)
          loadArrayElem(elem)
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
        case other => fail(s"intrinsic $other")
      }
    }

    private def translateRawArrayAllocate(e: CHIR.RawArrayAllocate): Unit = {
      val elem = resolver.typeSig(e.elementType)
      if (elem.isZST) return
      if (elem.isVariableSizeType) return fail("raw array of variable-size elem")
      if (elem.isRecord) return fail("raw array of record elem")
      val arrayType = SignatureType.CangjieArray(elem)
      touch(e.size)
      slotOfAny(e, arrayType) // null-init before newarr's GC point
      loadToReg(e.size, IR2)
      asm.newarr(arrayType.toCbc) // result hardcoded IR1
      saveGCMapHere()
      storeResult(e, IR1, arrayType)
    }

    private def translateRawArrayLiteralInit(e: CHIR.RawArrayLiteralInit): Unit = {
      val arrType = sigOf(e.array).getOrElse(return fail("literalInit: unknown array sig"))
      if (!arrType.isArray) return fail(s"literalInit on non-array $arrType")
      val elem = arrType.getArrayElemType
      if (elem.isZST) return
      if (elem.isRecord) return fail("literalInit of record elem")
      touch(e.array)
      loadToReg(e.array, SCRATCH2)
      asm.nullcheck(SCRATCH2)
      e.elementValues.zipWithIndex.foreach { (v, i) =>
        if (!sigOf(v).forall(_.isZST)) {
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
      if (e.instantiatedTypeArgs.nonEmpty) return fail("generic apply")
      val f: CHIR.Func = e.callee
      val calleeMethod = findCallee(f)
      if (calleeMethod == null) return fail(s"callee not found: ${resolver.symName(f)}")

      val calleeMt = calleeMethod.getMethodType
      // End special params (passed after the real params): only a single
      // OuterTypeInfo param of a non-generic declaring class is supported —
      // its value is the static TI of the declaring class.
      val endCount = calleeMt.endSpecialParamsCount
      // Mirrors CHIRParser: the callee's OuterTypeInfo is the TI of the
      // receiver's (instantiated) type, or of the declaring class for
      // receiver-less calls.
      val outerTiSig = if (e.thisArg != null) sigOf(e.thisArg).get
                       else SignatureType.fromSymType(calleeMethod.getDeclaringClass)
      val outerTi = endCount == 1 && calleeMt.hasOuterTypeInfoParameter &&
        !outerTiSig.containsTypeVariables
      if (endCount != 0 && !outerTi)
        return fail(f"callee end special params (TI: end=$endCount outer=${calleeMt.hasOuterTypeInfoParameter} this=${calleeMt.hasThisTypeInfoParameter} tv=${outerTiSig.containsTypeVariables})")
      val calleeStart = calleeMt.startSpecialParamsCount
      if (!(calleeStart == 0 || (calleeStart == 1 && calleeMt.hasReceiverParameter && !calleeMt.hasRetByValParameter)))
        return fail(s"callee special params start=$calleeStart")
      val calleeAbi = DirectCBCCompiler.platform(env).abi(calleeMt)
      val locs = calleeAbi.paramLocations
      val allArgs = (if (e.thisArg != null) Seq(e.thisArg) else Seq.empty) ++ e.args
      val regLocs = locs.filter(l => l.isReg && l.asReg.isInstanceOf[IR])
      if (locs.exists(l => !l.isReg)) return fail("stack args")
      // argLocs = the locs holding the real (CHIR) params, i.e. between the
      // start specials and the trailing end specials.
      val argLocs = regLocs.drop(calleeStart).dropRight(endCount)
      // ZST args are absent from the ABI; drop them
      val realArgs = allArgs.filter(a => sigOf(a).forall(!_.isZST))
      if (realArgs.size != argLocs.size) return fail(s"arg count ${realArgs.size} vs ${argLocs.size}")
      // Float args go to FR registers; int/ref args to IR registers.
      realArgs.zip(argLocs).foreach { (a, loc) =>
        loc.asReg match {
          case fr: FR => loadFloatToReg(a, fr)
          case ir: IR => loadToReg(a, ir)
        }
      }
      if (outerTi)
        asm.loadTypeInfoSig(regLocs(calleeMt.getOuterTypeInfoArgIdx).asReg.asInstanceOf[IR],
          outerTiSig.toCbc)
      val resultReg = {
        val rl = calleeAbi.resultLocation
        if (rl != null && rl.isReg && rl.asReg.isInstanceOf[IR]) rl.asReg.asInstanceOf[IR] else IR1
      }
      // pre-create (null-init) all value slots BEFORE clobbering arg registers
      realArgs.foreach(a => sigOf(a) match { case Some(s) if !s.isZST => slot(a, s); case _ => })
      resultOf(e).foreach(res => sigOf(res) match { case Some(s) if !s.isZST => slot(res, s); case _ => })
      asm.callDirect(resultReg, calleeMethod)
      saveGCMapHere()
      resultOf(e) match {
        case Some(res) if !sigOf(res).forall(_.isZST) =>
          storeFromReg(res, resultReg, sigOf(res).get)
        case _ =>
      }
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
      * outer-TI load, the call itself and result store. */
    private def emitCall(e: CHIR.Expression, calleeMethod: Method, callArgs: Seq[CHIR.Value],
                         ref: MethodReference, isVirtual: Boolean): Unit = {
      val calleeMt = calleeMethod.getMethodType
      val endCount = calleeMt.endSpecialParamsCount
      val outerTiSig = if (calleeMt.hasReceiverParameter) sigOf(callArgs.head).get
                       else SignatureType.fromSymType(calleeMethod.getDeclaringClass)
      val outerTi = endCount == 1 && calleeMt.hasOuterTypeInfoParameter &&
        !outerTiSig.containsTypeVariables
      if (endCount != 0 && !outerTi)
        return fail(f"callee end special params (TI: end=$endCount outer=${calleeMt.hasOuterTypeInfoParameter} this=${calleeMt.hasThisTypeInfoParameter} tv=${outerTiSig.containsTypeVariables})")
      if (calleeMt.hasThisTypeInfoParameter) return fail("callee ThisTypeInfo param")
      val calleeStart = calleeMt.startSpecialParamsCount
      if (!(calleeStart == 0 || (calleeStart == 1 && calleeMt.hasReceiverParameter && !calleeMt.hasRetByValParameter)))
        return fail(s"callee special params start=$calleeStart")
      val calleeAbi = DirectCBCCompiler.platform(env).abi(calleeMt)
      val locs = calleeAbi.paramLocations
      val regLocs = locs.filter(l => l.isReg && l.asReg.isInstanceOf[IR])
      if (locs.exists(l => !l.isReg)) return fail("stack args")
      val argLocs = regLocs.drop(calleeStart).dropRight(endCount)
      val realArgs = callArgs.filter(a => sigOf(a).forall(!_.isZST))
      if (realArgs.size != argLocs.size) return fail(s"arg count ${realArgs.size} vs ${argLocs.size}")
      realArgs.zip(argLocs).foreach { (a, loc) =>
        loc.asReg match {
          case fr: FR => loadFloatToReg(a, fr)
          case ir: IR => loadToReg(a, ir)
        }
      }
      if (outerTi)
        asm.loadTypeInfoSig(regLocs(calleeMt.getOuterTypeInfoArgIdx).asReg.asInstanceOf[IR],
          outerTiSig.toCbc)
      val resultReg = {
        val rl = calleeAbi.resultLocation
        if (rl != null && rl.isReg && rl.asReg.isInstanceOf[IR]) rl.asReg.asInstanceOf[IR] else IR1
      }
      realArgs.foreach(a => sigOf(a) match { case Some(s) if !s.isZST => slot(a, s); case _ => })
      resultOf(e).foreach(res => sigOf(res) match { case Some(s) if !s.isZST => slot(res, s); case _ => })
      if (isVirtual) asm.callVirt(resultReg, ref) else asm.callDirect(resultReg, ref)
      saveGCMapHere()
      resultOf(e) match {
        case Some(res) if !sigOf(res).forall(_.isZST) =>
          storeFromReg(res, resultReg, sigOf(res).get)
        case _ =>
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

    private def translateTerminator(t: CHIR.Terminator): Unit = t match {
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
      case other => fail(s"terminator ${other.getClass.getSimpleName}")
    }

    def send(): Unit = {
      val xgen = new XTableGenerator(method, (slot: Frame.Slot) => slot.offset)(env)
      val packed = xgen.packXInfo(new XInfo, Seq.empty)
      CBCFileGenerator.sendCode(
        method, segment, 0, packed, ExceptionTable(Seq.empty), liveness.collect,
        /* tailParamCount */ 0, /* untypedStackSlotsCount */ slotTypes.length,
        /* usedNonVolIRegsMask */ 0, /* usedNonVolFRegsMask */ 0, /* maxCalleeStackArgsCount */ 0,
        /* mayHaveNativeCalls */ false,
        /* stackAllocatedTypeSigs */ Seq.empty, /* variableSizeTypes */ Seq.empty)
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
