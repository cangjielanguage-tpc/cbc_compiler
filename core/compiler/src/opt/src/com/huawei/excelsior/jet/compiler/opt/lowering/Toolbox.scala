/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.lowering

import com.huawei.excelsior.jet.compiler.options.BoolOption.{GCSafetyChecks, GenStackTrace, IdescHigh16BitsCleaning}
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.codeemitter.BarrierKind.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.{RTConst, RTSProc, symlevel}
import com.huawei.excelsior.jet.compiler.symlevel.{Field, Method, MethodReference, SignatureType}
import com.huawei.excelsior.jet.util.{ScalaCollections, Worklist}

import scala.language.implicitConversions
import scala.collection.mutable

/**
 * Basic definitions and utilities for lowering.
 *
 * @author alexm
 */
private[lowering] trait Toolbox extends Universe {

  /** Control, memory and value. TODO: remove */
  case class TaggedState(ctrl: ControlNode, memory: MemoryNode, value: Node = null) {

    def withValue(v: Node) = TaggedState(ctrl, memory, v)
  }

  object TaggedState {
    /** No output control flow. */
    val Unreachable = TaggedState(null, null, null)
  }

  val toBeInlinedCalls = Worklist.empty[Call]

  /** Generate call and ensure that it will be eventually inlined. */
  private[lowering] def inlinedCall(target: Method)(args: Node*): Call = {
    val call = DirectCall(target)(args: _*)
    if (!isO1Compiled || target.isInlineAllAndRemove) {
      toBeInlinedCalls += call
    }
    call
  }

  /** Create cold block with one RTSCall. */
  private[lowering] def coldBlockWithRTSCall(inEdges: BlockExit*)(proc: RTSProc, args: Node*): ControlNode = {
    continue(inEdges: _*)
    ColdCodeMarker()
    RTSCall(proc)(args: _*)
  }

  private[lowering] case class SharedErrorRTSCallBlock(block: Block, call: Call) {
    // CFG sharing may lead to problems (look at InlineContextRegions and JET-10486 for more details).

    /** Append new incoming edges with given `args` to RTSCall arguments in shared block. */
    def append(inEdges: Seq[ControlNode], args: Seq[Node]): Unit = {
      val oldEdgesNumber = block.arity

      // 1. Append new incoming edges to block
      inEdges foreach block.addArg

      // 2. Create memory phi-function, or update already created
      if (block.redefinesMemory) {
        assert(call.inMemory == block)
      } else {
        call.inMemory = block
      }

      // 3. Update RTSCall args
      for ((arg, idx) <- call.invokeArgs.zipWithIndex) {
        val newArgs = Seq.fill(inEdges.size)(args(idx))
        arg match {
          case phi: Phi if phi.block == block =>
            phi.addArgs(newArgs)

          case x =>
            val phiArgs = Seq.fill(oldEdgesNumber)(x) ++ newArgs
            val tpe = ScalaCollections.uniqueValue(phiArgs.map(_.tpe)).get
            call.updateInvokeArg(idx, Phi(tpe)(block +: phiArgs :_*))
        }
      }
    }
  }

  private[lowering] lazy val errorRTSCallBlocks = new mutable.HashMap[(RTSProc, XBlock, List[Method]), SharedErrorRTSCallBlock]

  override def resetUniverse(): Unit = {
    super.resetUniverse()
    errorRTSCallBlocks.clear()
  }

  /** Create cold block with error RTSCall and Halt. Reuse blocks with the same error RTS proc.
    * Given `source` is a node, which lowering required this cold block. It used to get xHandler info. */
  protected def coldBlockWithErrorRTSCallAndHalt(inEdges: BlockExit*)(source: SpinalNode, proc: RTSProc, args: Node*): Unit = {
    val handler = if (source.hasXHandler) source.xHandler else null

    // 1. Cold blocks with the same handler but with phi-functions cannot be merged because phi-functions arguments may be different.
    // 2. If GenStackTrace is enabled then line numbers are important.
    if ((handler != null && handler.phies.nonEmpty) || env.enabled(GenStackTrace)) {
      coldBlockWithRTSCall(inEdges: _*)(proc, args: _*)
      Halt.afterRTSCall(proc, "cold block with RTSCall")()
      return
    }

    val key = (proc, handler, source.inlineContext.toRoot.map(_.method).toList)

    errorRTSCallBlocks.get(key) match {
      case Some(sharedHaltBlock) =>
        sharedHaltBlock.append(inEdges, args)
        setCurrentControl(sharedHaltBlock.block.blockEnd)

      case None =>
        val block = continue(inEdges: _*)
        ColdCodeMarker()
        val call = RTSCall(proc)(args: _*)
        Halt.afterRTSCall(proc, "generated new RTSCall block")()
        errorRTSCallBlocks(key) = SharedErrorRTSCallBlock(block, call)
    }
  }

  private[lowering] def intToAddr(i: Node) = BitFieldExtract.Extend(AddrType, I32, signExtension = true, i)

  private[lowering] def longToAddr(i: Node) = BitFieldExtract.Extend(AddrType, I64, signExtension = false, i)

  private[lowering] def ifAddrEq(addr1: Node, addr2: Node) = If(Cmp(AddrType, Condition.EQ)(addr1, addr2))

  private[lowering] def addrConst(x: Long) = IntegralConst(AddrType)(x)

  private[lowering] def addrNull = addrConst(0)

  private[lowering] def addAddrInt(addr: Node, i: Node) = Add(addr, intToAddr(i))

  private[lowering] def genRichDecompositionActions[T, U](rich: Node, richAction: (Node, Node, Node) => T, plainAction: () => U): (T, U) = {
    val (checkRich, bits, enrichment) = genCheckRich(rich)

    continue(checkRich.trueExit)
    val richValue = richAction(rich, bits, enrichment)
    val richExit = Goto()

    continue(checkRich.falseExit) // obj is null or non-rich
    val plainValue = plainAction()
    val plainExit = Goto()

    continue(richExit, plainExit)
    (richValue, plainValue)
  }

  /** Generate rich object decomposition into obj and ciao with backup path. */
  private[lowering] def genRichDecomposition(rich: Node, itype: symlevel.Type, backup: Node => Node): (Node, Node) = {
    val backupObj = ReinterpretCast(EopType.Any, TRefType)(rich)

    val ((richObj, richCIAO), backupCIAO) = genRichDecompositionActions(rich, { (_, bits, enrichment) =>
      val obj = genExtractObject(rich, bits, enrichment)
      val ciao = genMakeCIAO(itype, obj, enrichment)
      (obj, ciao)
    }, () => backup(backupObj))

    (makePhi(richObj, backupObj), makePhi(richCIAO, backupCIAO))
  }

  protected def genExtractObject(rich: Node, bits: Node, enrichment: Node): Node

  protected def genMakeCIAO(itype: symlevel.Type, plain: Node, enrichment: Node): Node

  /** Generate enrichment check. True for rich objects, false for null or non-rich objects.
    * Additionally returns untraced reference to checked object and its enrichment.
    */
  protected def genCheckRich(n: Node): (If, Node, Node)

  private[lowering] def genRunTimeTypeInfoAddr(n: BuiltInTypeInfo) = {
    GetField(RT.TypeHandle.td)(SymbolAddress.controlled(n.targetType.getTypeHandle, n.inCtrl))
  }

  private[lowering] def genRunTimeTypeInfoAddr(n: ThisTypeInfo) = {
    // TODO obtain real ThisTypeInfo instead of RTTI
    if (n.target.isRecord) {
      LConst(0) // TODO support ThisTypeInfo of records
    } else {
      GetField(RT.TypeHandle.td)(SymbolAddress.controlled(n.target.getTypeHandle, n.inCtrl))
    }
  }

  private[lowering] def genInstanceDescriptorAddr(obj: Node): Node = obj.tpe match {
    case ThinType => GetField(RT.ThinObj.td)(ReinterpretCast(ThinType, AddrType)(obj))
    case TRefType => clearHigh16BitsIfNeeded(GetField(RT.ManagedObj.td)(obj))
    case tpe => shouldNotReachHere(tpe)
  }

  private[lowering] def genThisTypeInfoAddr(obj: Node): Node = {
    val td = genInstanceDescriptorAddr(obj)
    GetField(RT.InstanceDescriptor.rtti)(td)
  }

  private[lowering] def genManagedVirtualMethodAddr(desc: Node, vnum: Int): Node = {
    GetField(RT.ManagedInstanceDescriptor.virtualMethod(vnum))(desc)
  }

  private[lowering] def genJavaVirtualMethodAddr(desc: Node, vnum: Int): Node = {
    GetField(RT.JavaInstanceDescriptor.virtualMethod(vnum))(desc)
  }

  private[lowering] def genScalaVirtualMethodAddr(desc: Node, vnum: Int): Node = {
    GetField(RT.ScalaInstanceDescriptor.virtualMethod(vnum))(desc)
  }

  private[lowering] def genCangjieVirtualMethodAddr(desc: Node, vnum: Int): Node = {
    GetField(RT.CangjieInstanceDescriptor.virtualMethod(vnum))(desc)
  }

  private[lowering] def genThinVirtualMethodAddr(desc: Node, vnum: Int): Node = {
    GetField(RT.ThinTypeHandle.virtualMethod(vnum))(desc)
  }

  protected def getIMTOffsetFromCIAO(ciao: Node): Node

  private[lowering] def genInterfaceMethodAddr(desc: Node, vnum: Int, ciao: Node): Node = {
    val imtOffset = getIMTOffsetFromCIAO(ciao)
    val methodTable = Add(desc, imtOffset)
    GetField(RT.MethodTable.virtualMethod(vnum))(methodTable)
  }

  private[lowering] def getVirtualMethodAddr(originalRef: MethodReference, obj: Node): Node = {
    val refClass = originalRef.refClass
    assert(!refClass.isThinClass || refClass.isPolyThinClass)
    val desc = genInstanceDescriptorAddr(obj)
    val vnum = originalRef.virtualMethodSlot
    if (refClass.isThinClass) {
      genThinVirtualMethodAddr(desc, vnum)
    } else if (refClass.isJavaReference) {
      genJavaVirtualMethodAddr(desc, vnum)
    } else if (refClass.isXScalaType) {
      genScalaVirtualMethodAddr(desc, vnum)
    } else if (refClass.isCangjieType) {
      genCangjieVirtualMethodAddr(desc, vnum)
    } else {
      assert(refClass.isAJManagedType)
      genManagedVirtualMethodAddr(desc, vnum)
    }
  }

  private[lowering] def genCohenLevel(desc: Node, clearLevelBit: Boolean = true): Node = {
    val level = GetField(RT.InstanceDescriptor.cohenLevel)(desc)
    if (clearLevelBit) And(level, IntegralConst(IntType)(~RTConst.CohenDisplay.LEVEL_BIT.intValue))
    else level
  }

  private[lowering] def stackAllocArrayOfInts(values: Seq[Node]): Node = {
    val array = StackAlloc.raw(values.length * IntType.size, IntType.size)
    for ((v, i) <- values.zipWithIndex) {
      UArrayPut(I32)(array, IConst(i), v)
    }
    array
  }

  private [lowering] final def copyMemory(size: Int, alignment: Int, dst: Node, src: Node): Unit = {
    var pos = 0
    var remaining = size

    for (quantum <- Seq(I64, I32, I16, I8)) {
      val sz = quantum.sizeInBytes
      if (sz <= alignment) {
        val sig = SignatureType.Primitive(quantum)
        while (remaining >= sz) {
          StoreMemory(quantum, sig, atomic = false)(Lea.Base(dst, pos),
            LoadMemory(quantum, sig, atomic = false)(Lea.Base(src, pos)))

          pos += sz
          remaining -= sz
        }
      }
    }

    assert(pos == size && remaining == 0)
  }

  private [lowering] def linkStructuredSynchronization(): Unit = {
    if (isUnstructuredLocking) return

    for (syncRegion <- all[SynchronizedRegion]) {
      withNewVar(IntType) { (assignAfter, readAfter) =>
        syncRegion.enters.toSeq foreach (en => assignAfter(en, en))
        syncRegion.exits.toSeq foreach { ex =>
          ex.lockingContext = readAfter(ex.inCtrl)
        }
      }
    }

    completeSSA()
  }

  // Erase high 16 bits if BoolOptions.IdescHigh16BitsCleaning are enabled
  protected def clearHigh16BitsIfNeeded(node: Node): Node =
    if (env.enabled(IdescHigh16BitsCleaning)) clearHigh16Bits(node) else node

  // Erase high 16 bits
  protected def clearHigh16Bits(node: Node): Node
}
