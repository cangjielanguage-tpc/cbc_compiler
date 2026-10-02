/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.direct

import com.huawei.excelsior.jet.assembler.{AsmEmitter, Label, Segment, Width}
import com.huawei.excelsior.jet.assembler.cbc.{CbcTypeKind, ExceptionTable, StackSlot}
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
import com.huawei.excelsior.jet.compiler.chir.{CHIR, CHIRLoader, CHIRResolver}
import com.huawei.excelsior.jet.compiler.ir.XInfo
import com.huawei.excelsior.jet.compiler.o2lib.opt.VZCModule
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule as pcO
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment as LE
import com.huawei.excelsior.jet.compiler.symlevel
import com.huawei.excelsior.jet.compiler.symlevel.{Method, MethodReference, MethodReferenceAccessKind as MAK, SignatureType}
import com.huawei.excelsior.jet.compiler.types.CompiledType

import scala.collection.mutable

/** Direct CHIR-to-CBC transliterator: bypasses the whole per-method optimizing
  * pipeline (CHIRInterpreter -> opt IR -> inline/optimize -> lowering -> regalloc
  * -> genCode) and emits isa12 bytecode straight from the CHIR body.
  *
  * M1 subset: only methods whose body uses a trivial subset of CHIR constructs are
  * handled; anything else falls back to the full pipeline, so mixing both paths in
  * one .cbc is legal (they share CBCFileGenerator).
  *
  * No liveness/GC maps are emitted (empty XTable); no register allocator: each CHIR
  * value lives in a dedicated untyped stack slot, registers are scratch only.
  */
class DirectCBCCompiler(fallback: VZCModule.CompilerInterface) extends VZCModule.CompilerInterface {
  private val env: LE = LE.getInstance

  override def enterClass(`class`: pcO.Class, stage: com.huawei.excelsior.jet.compiler.Pass): Unit =
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

  override def compileMethod(m: pcO.Method, versioned: symlevel.impl.light.VersionedMethod): Unit = {
    if (versioned != null) {
      fallback.compileMethod(m, versioned)
      return
    }
    val t0 = System.nanoTime()
    val direct = tryDirect(m)
    val t1 = System.nanoTime()
    if (direct) {
      directCount += 1
      directNanos += t1 - t0
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

    val ctx = new MethodContext(method, func, chirDef.source.toString)
    ctx.translate() && { ctx.send(); true }
  }

  /** Reverse index (chir source, chir id) -> symlevel method, built lazily. */
  private var chirIndex: mutable.HashMap[(String, Long), Method] = null

  private def index(): mutable.HashMap[(String, Long), Method] = {
    if (chirIndex == null) {
      chirIndex = mutable.HashMap.empty
      for (clazz <- env.getAllClasses; meth <- clazz.getDeclaredMethods) {
        meth.getCHIRDef.foreach { d => chirIndex((d.source.toString, d.id)) = meth }
      }
    }
    chirIndex
  }

  private class MethodContext(val method: Method, val func: CHIR.Func, val source: String)(using resolver: CHIRResolver) {
    private var bail_ : Boolean = false
    private def fail(msg: String): Unit = {
      if (sys.env.contains("DIRECT_CBC_BAIL")) System.err.println(s"DIRECT-CBC bail $method: $msg")
      bail_ = true
    }

    /** Resolve the symlevel method for a CHIR callee by declaring type + name + signature
      * (works for cross-file callees, unlike the per-file (source, id) index). */
    private def findCallee(f: CHIR.Func): Method = {
      val declSymType = f.declaringDef match {
        case Some(d) => resolver.symType(d).get
        case None => resolver.findClass(f.packageName).get
      }
      val declClass = declSymType.asInstanceOf[com.huawei.excelsior.jet.compiler.symlevel.ClassType]
      val modifiers = resolver.symModifiers(f)
      val hasReceiver = f.declaringDef.isDefined && !modifiers.contains(com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.STATIC)
      val (sig, _, _, _) = resolver.functionSig(f, hasReceiver)
      declClass.findDeclaredMethodOrNull(xstr(resolver.symName(f)), sig)
    }

    /** Signature type of a value (literals carry their own type). */
    private def sigOf(v: CHIR.Value): Option[SignatureType] = v match {
      case lv: CHIR.LocalVar => Some(resolver.typeSig(lv.tpe))
      case p: CHIR.Parameter => Some(resolver.typeSig(p.tpe))
      case g: CHIR.GlobalVar => Some(resolver.typeSig(g.tpe))
      case l: CHIR.Literal => Some(resolver.typeSig(l.tpe))
      case _ => None
    }

    // -- value model: one untyped stack slot per CHIR value
    private val slotOf = mutable.HashMap.empty[CHIR.Value, Int]
    private val slotTypes = mutable.ArrayBuffer.empty[SignatureType]

    private def slot(v: CHIR.Value, sig: SignatureType): Int = slotOf.getOrElseUpdate(v, {
      val idx = slotTypes.length
      slotTypes += sig
      idx
    })

    // -- emission
    private val segment = new Segment(method)
    private val asm = new ForkedAssembler with com.huawei.excelsior.jet.compiler.cbc.CbcSymbolAdapter

    private val blocks: Seq[CHIR.Block] = func.body.get.blocks

    // Scratch register for load-op-store.
    private val SCRATCH = IR1

    private def cbcKind(sig: SignatureType): CbcTypeKind = CbcTypeKind(sig.toAsm)

    /** Loads a value into reg. */
    private def loadToReg(v: CHIR.Value, reg: IR): Unit = v match {
      case l: CHIR.Literal => literalToReg(l, reg)
      case lv: (CHIR.LocalVar | CHIR.Parameter) =>
        val s = sigOf(lv).get
        if (s.isZST) () // nothing to load
        else asm.loadUntyped(reg, LoadAccessKind.from(cbcKind(s)), StackSlot.Untyped(slot(lv, s)))
      case other => fail(s"load of ${other.getClass.getSimpleName}")
    }

    private def literalToReg(l: CHIR.Literal, reg: IR): Unit = l match {
      case CHIR.UnitLiteral => ()
      case l: CHIR.IntLiteral => asm.movi64(reg, l.value)
      case l: CHIR.BoolLiteral => asm.movi32(reg, if (l.value) 1 else 0)
      case l: CHIR.RuneLiteral => asm.movi64(reg, l.value)
      case _: CHIR.NullLiteral => asm.mov(reg, IRZ, reference = true)
      case other => fail(s"literal ${other.getClass.getSimpleName}")
    }

    private def storeFromReg(v: CHIR.Value, reg: IR, sig: SignatureType): Unit = v match {
      case _: CHIR.Literal => () // literals are re-materialized at each use
      case lv: (CHIR.LocalVar | CHIR.Parameter) =>
        if (sig.isZST) ()
        else asm.storeUntyped(reg, StoreAccessKind.from(cbcKind(sig)), StackSlot.Untyped(slot(lv, sig)))
      case other => fail(s"store to ${other.getClass.getSimpleName}")
    }

    private def resultOf(e: CHIR.Expression): Option[CHIR.LocalVar] = e match {
      case hr: CHIR.HasResultVar => Some(hr.resultVar)
      case _ => None
    }

    def translate(): Boolean = {
      try {
        asm.withSegment(segment) {
          // spill incoming params (they arrive in ABI registers; calls clobber volatiles)
          val abi = DirectCBCCompiler.platform(env).abi(method.getMethodType)
          val params = func.params
          val regLocs = abi.paramLocations.filter(l => l.isReg && l.asReg.isInstanceOf[IR])
          val zstParams = params.filter(p => sigOf(p).exists(_.isZST))
          if (zstParams.nonEmpty) fail("ZST param")
          else if (params.size > regLocs.size) fail("too many params")
          else params.zip(regLocs).foreach { (p, loc) =>
            storeFromReg(p, loc.asReg.asInstanceOf[IR], sigOf(p).get)
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
          case e: CHIR.StaticCast => translateStaticCast(e)
          case e => return fail(s"expr ${e.getClass.getSimpleName}")
        }
      }
      translateTerminator(b.terminator)
    }

    private val labels = mutable.HashMap.empty[CHIR.Block, Label]
    private def blockLabel(b: CHIR.Block): Label = labels.getOrElseUpdate(b, asm.newLabel)

    private def translateStore(e: CHIR.Store): Unit = {
      val loc = e.location match {
        case lv: (CHIR.LocalVar | CHIR.Parameter) => lv
        case _ => return fail("store to non-local")
      }
      val s = sigOf(loc).get
      if (s.isZST) return
      loadToReg(e.value, SCRATCH)
      storeFromReg(loc, SCRATCH, s)
    }

    private def translateLoad(e: CHIR.Load): Unit = {
      val loc = e.location match {
        case lv: (CHIR.LocalVar | CHIR.Parameter) => lv
        case _ => return fail("load from non-local")
      }
      val s = sigOf(loc).get
      if (s.isZST) return
      resultOf(e) match {
        case Some(res) =>
          loadToReg(loc, SCRATCH)
          storeFromReg(res, SCRATCH, s)
        case None => fail("load without result")
      }
    }

    private def translateStaticCast(e: CHIR.StaticCast): Unit = {
      val from = sigOf(e.value)
      val to = resolver.typeSig(e.targetTpe)
      if (from.exists(_.isZST) || to.isZST) return
      if (from.contains(to)) {
        resultOf(e) match {
          case Some(res) =>
            loadToReg(e.value, SCRATCH)
            storeFromReg(res, SCRATCH, to)
          case None => fail("cast without result")
        }
      } else {
        fail(s"static cast $from -> $to")
      }
    }

    private def translateApply(e: CHIR.Apply): Unit = {
      if (e.instantiatedTypeArgs.nonEmpty) return fail("generic apply")
      val f: CHIR.Func = e.callee
      val calleeMethod = findCallee(f)
      if (calleeMethod == null) return fail(s"callee not found: ${resolver.symName(f)}")

      val calleeAbi = DirectCBCCompiler.platform(env).abi(calleeMethod.getMethodType)
      val locs = calleeAbi.paramLocations
      val allArgs = (if (e.thisArg != null) Seq(e.thisArg) else Seq.empty) ++ e.args
      val regLocs = locs.filter(l => l.isReg && l.asReg.isInstanceOf[IR])
      if (locs.exists(l => !l.isReg)) return fail("stack args")
      // ZST args are absent from the ABI; drop them
      val realArgs = allArgs.filter(a => sigOf(a).forall(!_.isZST))
      if (realArgs.size != regLocs.size) return fail(s"arg count ${realArgs.size} vs ${regLocs.size}")
      realArgs.zip(regLocs).foreach { (a, loc) =>
        loadToReg(a, loc.asReg.asInstanceOf[IR])
      }
      val resultReg = {
        val rl = calleeAbi.resultLocation
        if (rl != null && rl.isReg && rl.asReg.isInstanceOf[IR]) rl.asReg.asInstanceOf[IR] else IR1
      }
      asm.callDirect(resultReg, calleeMethod)
      resultOf(e) match {
        case Some(res) if !sigOf(res).forall(_.isZST) =>
          storeFromReg(res, resultReg, sigOf(res).get)
        case _ =>
      }
    }

    private def methodRef(m: Method): MethodReference = {
      val mak = if (m.isStatic) MAK.STATIC
                else if (m.isCangjieMut) MAK.MUT
                else MAK.SPECIAL
      new MethodReference(m, mak, CompiledType(m.getDeclaringClass))
    }
    // (kept for M2: access-kind computation if callDirect needs a MethodReference)

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
      case other => fail(s"terminator ${other.getClass.getSimpleName}")
    }

    def send(): Unit = {
      val xgen = new XTableGenerator(method, (slot: Frame.Slot) => slot.offset)(env)
      val packed = xgen.packXInfo(new XInfo, Seq.empty)
      CBCFileGenerator.sendCode(
        method, segment, 0, packed, ExceptionTable(Seq.empty), LivenessInfoCollector.empty,
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
