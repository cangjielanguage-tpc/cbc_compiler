/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.aliases.AliasAnalysis
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.codeemitter.BarrierKind
import BarrierKind.{STORE_STORE, STRICT_MEM}
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.symlevel.Field
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.graph.{Loop, LoopKind}
import com.huawei.excelsior.jet.util.{ScalaCollections, Worklist}

import scala.PartialFunction.{cond, condOpt}
import scala.annotation.nowarn
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Engine of memory optimizations: detects when it is possible to move loads and stores.
  *
  * @author cypok
  */
trait MemoryOptimizations extends AliasAnalysis { self: Universe =>

  protected def allowTransformationFastPath = true

  private def allowColdReloads = profile.isPGOHost
  private def mayMoveLoadOutOfLoops(get: GetMemoryOperation) = {
    env.enabled(MoveLoadsOutOfLoops) || (profile.isPGOHost && env.enabled(MoveLoadsOutOfLoopsInPGOHosts)) ||
      cond(get) {
        case get: GetField =>
          val cls = get.field.getDeclaringClass

          def isBuiltinRecord =
            cls.isArraySlice ||
              cls.getName == CangjieSymLevelMaker.STD_CORE_STRING_NAME ||
              cls.getName.startsWith(CangjieSymLevelMaker.STD_CORE_OPTION_PREFIX)

          (env.enabled(MoveRecordsOutOfLoops) && cls.isRecord) ||
            (env.enabled(MoveBuiltinRecordsOutOfLoops) && isBuiltinRecord)
      }
  }

  def optimizeMemoryReads(): Boolean = new MemoryOptimizer().optimizeMemoryReads()

  class MemoryOptimizer {

    private lazy val coldBlocks = Sets[Block].newMSet(findWarmAndColdBlocks())

    // Do not publish whole loops structure because during optimization we may create new blocks and
    // don't want to add them into bodies.
    private lazy val loopByHeader: Map[Block, Loop[Block]] = {
      val loops = cfg.loops
      Maps[Block].newImmMap(loops.iterator map { l => (l.header, l) })
    }

    private[MemoryOptimizations] def optimizeMemoryReads(): Boolean = {
      if (!env.enabled(RedundantLoadElimination)) return false

      var changed = false

      var gets = (all[GetMemoryOperation] filter optimizableMemoryRead).toList
      if (gets.isEmpty) return changed

      if (eliminateMemoryReads(gets)) {
        changed = true
        gets = gets filter (_.isCommitted)
        if (gets.isEmpty) return changed
      }

      changed
    }

    /** Try to replace get operation by some value.
      * Value may be obtained from a put operation to the same memory or
      * value may be a get operation from the same memory.
      */
    private def eliminateMemoryReads(gets: List[GetMemoryOperation]): Boolean = {
      var changed = false

      withIncrementalGCM { // for upperPoint which is used below
        // In general if g1 is replaceable by g2 and g2 is replaceable by g3, then g1 should be replaceable by g3.
        // However due to conservativeness of the IR this statement might not hold.
        // So it's safer to process gets one by one, preventing replacement of one get by already decommitted another.
        for {
          get <- gets.iterator
          sources = findGetMemoryReplacement(get)
          if sources.nonEmpty
        } {
          sources match {
            case Seq(singleSource) if allowTransformationFastPath =>
              val (_, value) = singleSource.materialize(get)
              get replaceBy value

            case _ =>
              replaceByVarAt(get, upperPoint(get)) { assignAt =>
                for (source <- sources) {
                  assignAt.tupled(source.materialize(get))
                }
              }
          }

          changed = true
          if (stats.isEnabled(StatsKind.MemOpt)) {
            stats.count(StatsKind.MemOpt, s"${get.simpleName} eliminated" +
              (if (sources.exists(_.isInstanceOf[ColdReload])) " (with reload(s) on cold paths)" else "") +
              (if (sources.exists(_.isInstanceOf[PreHeaderReload])) " (with movement out of loop)" else ""),
              get)
          }
        }
      }

      changed
    }


    /** A handle of replacement for some get memory operation. */
    private sealed abstract class ValueSource {
      /** Get point where this value is stored into memory and the value itself.
        * Note that materialization must be lazy to prevent IR changing without real need.
        */
      def materialize(originalGet: GetMemoryOperation): (UpperPoint, Node)

      protected def cloneWithNewMemory(n: HasInMemory, newMemory: Node): Node =
        Node.clonePartially(n) { case e if e.isMemory => newMemory }
    }

    /** Handle of replacement for some get memory operation which can be further replaced by some other replacements. */
    private abstract class IntermediateValueSource extends ValueSource

    private case class Get(get: GetMemoryOperation) extends IntermediateValueSource {
      def materialize(originalGet: GetMemoryOperation) =
        (upperPoint(get), get)
    }

    private case class PreHeaderReload(loop: Loop[Block]) extends IntermediateValueSource {
      def materialize(originalGet: GetMemoryOperation) = {
        val (preHeader, _) = getOrCreateLoopPreHeader(loop)
        val preBlockEnd = preHeader.blockEnd
        (preBlockEnd.inCtrl, cloneWithNewMemory(originalGet, preBlockEnd.inMemory))
      }
    }

    private case class Put(put: PutMemoryOperation) extends ValueSource {
      def materialize(originalGet: GetMemoryOperation) =
        (put, put.storedValue())
    }

    private case class IntegralValue(point: UpperPoint, v: Long, tpe: Type) extends ValueSource {
      def materialize(originalGet: GetMemoryOperation) =
        (point, IntegralConst(tpe)(v))
    }

    private case class Zero(point: UpperPoint) extends ValueSource {
      def materialize(originalGet: GetMemoryOperation) =
        (point, ZeroValueNode(originalGet.tpe))
    }

    private case class ColdReload(spoiler: MemoryNode) extends ValueSource {
      def materialize(originalGet: GetMemoryOperation) =
        (spoiler, cloneWithNewMemory(originalGet, spoiler))
    }

    private case class ClinitReload(clinit: Clinit) extends ValueSource {

      def materialize(originalGet: GetMemoryOperation) = {
        // Note that it is true only in case of conservative context-types (i.e. GetStatic is attached to the last check).
        val clinitWasInCtrl = originalGet.inCtrl == clinit

        assert(!coldBlocks(clinit.block))
        Clinit.wrapUnderInitializedTest(clinit)
        coldBlocks += clinit.block

        if (clinitWasInCtrl) {
          // After wrapping inCtrl is set to join block, so make manual copy adjusting both control and memory.
          assert(originalGet.inCtrl != clinit)
          (clinit, Node.clonePartially(originalGet) { case e if e.isControl || e.isMemory => clinit })

        } else {
          (clinit, cloneWithNewMemory(originalGet, clinit))
        }
      }
    }

    private case class PutAJArrayFill(put: AJArrayFill) extends ValueSource {
      def materialize(originalGet: GetMemoryOperation) =
        (put, put.value)
    }

    private def findGetMemoryReplacement(get: GetMemoryOperation): Seq[ValueSource] = {

      // We can't trust dominators in unreachable code
      if (get.inCtrl.block.unreachable) return Seq.empty

      val reloadUpperPoint = upperPointIgnoringMemory(get)

      def allowColdReloadAt(point: MemoryNode) =
        allowColdReloads && (reloadUpperPoint dominates point)

      /** Search for replacements of given load starting from given input memories.
        * Stop on "intermediate" results, when load could be replaced by another load (existing or created).
        * Such replacements could also be replaced during subsequent searches.
        *
        * Set of already scanned memories is shared because for every input memory there could be a single replacement
        * which is tracked only once.
        */
      def scanOnce(get: GetMemoryOperation, startMems: IterableOnce[MemoryNode], alreadyScannedMems: MemoryNode => Boolean): Option[(List[ValueSource], Iterator[MemoryNode])] = {
        val worklist = Worklist.empty[MemoryNode]
        val sources = ListBuffer.empty[ValueSource]

        worklist ++= startMems.iterator
        for (mem <- worklist.accumulate) {
          evaluateGetAtPoint(get, mem) match {
            case Some(src) =>
              sources += src

            case _ if MemoryDependencies.readCouldBeMovedAboveWrite(get, mem) =>
              val nextMems = mem match {
                case b: Block if potentialInductiveArgsMutationOnBackEdgeOf(get, b) =>
                  return None

                case b @ SuitableLoopHeaderForMove(loop) if
                    mayMoveLoadOutOfLoops(get) &&
                    // Also check that get is originally inside of this loop.
                    (loop.body contains get.inMemory.block) &&
                    // And we are allowed to reload get in pre-header.
                    (reloadUpperPoint strictDominates b) =>

                  sources += PreHeaderReload(loop)
                  // Forward edges are covered by reload, take only back edges.
                  loopBackwardEdges(loop) map memoryOnBlockEdge

                case _ =>
                  inMemories(mem)
              }
              worklist ++= nextMems filterNot alreadyScannedMems

            case _ if allowColdReloadAt(mem) && (coldBlocks contains mem.block) =>
              // We are allowed to reload memory in this cold point, move on.
              sources += ColdReload(mem)

            case _ if mem.isInstanceOf[Clinit] && allowColdReloadAt(mem) =>
              // Clinit will be moved into Siberia under initialization test,
              // reload memory at that point and continue scan along hot path.
              sources += ClinitReload(mem.asInstanceOf[Clinit])
              worklist ++= inMemories(mem) filterNot alreadyScannedMems

            case _ =>
              return None
          }
        }

        Some((sources.toList, worklist.iterator))
      }

      val scannedMems = Sets[MemoryNode].newMSet
      val allFoundSources = mutable.LinkedHashSet.empty[ValueSource]
      val allReplacedSources = mutable.Set.empty[ValueSource]

      /** Complete search for replacement of given abstract load.
        *
        * Intermediate results are like checkpoints:
        * it's good to find them but we want to go further,
        * there could be dependent nodes (take the intermediate result)
        * or there could be better replacements (discard the intermediate result).
        */
      def scanRecursively(vs: IntermediateValueSource): Unit = {
        assert(!allReplacedSources(vs))
        val (g, startMems) = vs match {
          case Get(g) =>
            (g, Seq(g.inMemory))
          case PreHeaderReload(loop) =>
            (get, loopEnterEdges(loop) map memoryOnBlockEdge filterNot scannedMems)
        }
        scanOnce(g, startMems, scannedMems) match {
          case Some((sources, mems)) =>
            scannedMems ++= mems
            allReplacedSources += vs

            val newSources = sources filterNot allFoundSources
            allFoundSources ++= newSources
            for (s @ (_s: IntermediateValueSource) <- newSources) {
              scanRecursively(s)
            }

          case None =>
        }
      }

      scanRecursively(Get(get))

      val finalSources = allFoundSources filterNot allReplacedSources
      // If get could be only reloaded in cold points above then this get is also cold. ;)
      val onlyCold = finalSources forall (_.isInstanceOf[ColdReload])
      if (onlyCold) Seq.empty else finalSources.toSeq
    }

    private object SuitableLoopHeaderForMove {
      def unapply(b: BBlock) = {
        // Ignore XBlock, because we cannot create pre-header for exceptional header.

        // No single header in irreducible loops.
        loopByHeader.get(b) filter (_.kind != LoopKind.IRREDUCIBLE)
      }
    }

    private def evaluateGetAtPoint(get: GetMemoryOperation, point: MemoryNode): Option[ValueSource] = {

      def possiblePut(put: MemoryNode, get: GetMemoryOperation): Option[ValueSource] = {
        condOpt((put, get)) {
          case (x: PutStatic, y: GetStatic) if sameField(x.field, y.field) => Put(x)

          case (x: PutField, y: GetField)
            if sameField(x.field, y.field) && x.obj == y.obj => Put(x)
          case (x: AnyNewClass, y: GetField)
            if x == y.obj && !y.field.isAJFlat => Zero(x)

          case (x: ArrayPut, y: ArrayGet)
            if x.idx == y.idx && x.array == y.array && x.accessType == y.accessType => Put(x)
          case (x: ArrayFill, y @ ArrayGet(_, _, arr, IntegralConst(idx)))
            if 0 <= idx && idx < x.size && x.array == arr && x.elemType == y.accessType => IntegralValue(x, x.storedValues(Math.toIntExact(idx)), y.tpe)
          case (x: AJArrayFill, y @ ArrayGet(_, _, arr, _))
            if x.array == arr && x.elemType == y.accessType && !y.arrayType.isRecordArray => PutAJArrayFill(x)
          case (x: AnyNewArray, y: ArrayGet)
            if x.lengths.size == 1 && x == y.array &&
              x.allocType.getArrayElemType.toAsm == y.accessType && !y.arrayType.isRecordArray => Zero(x)

          // TODO: NewString
        }
      }

      def possibleGet(mem: MemoryNode, get: GetMemoryOperation): Option[Get] = {
        mem.memoryUses collectFirst {
          case another: GetMemoryOperation
            if another.proto == get.proto && (another.valueArgs sameElements get.valueArgs) &&
              (another.inCtrl dominates get.inCtrl) && // otherwise we could break def-use dominance
              (another.inCtrl != get.inCtrl || another.inMemory != get.inMemory) => // otherwise they could be equivalent
            Get(another)
        }
      }

      possiblePut(point, get) orElse possibleGet(point, get)
    }

    private def potentialInductiveArgsMutationOnBackEdgeOf(get: GetMemoryOperation, b: Block) = {
      // Inductive arguments cannot be compared directly after passing through loop's back edge.
      //
      // E.g. ArrayGet has index-argument which could be an inductive variable and should be treated as non-equal
      // on different loop iterations.
      // Let's consider the loop:
      //   arr = new int[];
      //   for () {
      //     i = phi(0, i++)
      //     x = arr[i];
      //     arr[i] = 37
      //   }
      // If we move upward from ArrayGet we would come to NewArray (writes 0 to element) and to ArrayPut (writes 37),
      // but we should not replace ArrayGet by Phi(0, 37) because ArrayPut's i is from the previous iteration
      // and its value is different.
      //
      // It could be fixed and successfully optimized but requires a lot of effort (see unit-tests tagged ValueArgsMutation).

      (loopByHeader contains b) && !(upperPointByArgs(get, _.isValue) strictDominates b)
    }

    private def upperPointIgnoringMemory(get: GetMemoryOperation): ControlNode = {
      upperPointByArgs(get, e => !e.isMemory)
    }


    /** Returns true if memory value read by given `get` is not changed on any path of given `loop`
      * and thus `get` may be moved to some point above the loop.
      *
      * Note: control arg remains unchanged and value args are completely ignored,
      *       thus correctness of this predicate depends on correct control arg.
      */
    def canMemoryReadBeMovedOutOfLoop(get: GetMemoryOperation, loop: Loop[Block]): Boolean = {
      require(loop.kind != LoopKind.IRREDUCIBLE)
      require(!loop.header.isInstanceOf[XBlock])

      if (!(loop.body contains get.inMemory.block)) {
        return true
      }

      if (!optimizableMemoryRead(get)) {
        return false
      }

      withIncrementalGCM {
        if (potentialInductiveArgsMutationOnBackEdgeOf(get, loop.header)) {
          return false
        }
      }

      val header = loop.header
      assert(header.redefinesMemory)

      val worklist = Worklist(get.inMemory)
      for (mem <- worklist.accumulate) {
        assert(loop.body contains mem.block)

        if (hasUnreachableInMemories(mem) || !MemoryDependencies.readCouldBeMovedAboveWrite(get, mem)) {
          return false
        }

        worklist ++= (if (mem == header) {
          // Take only memories from back edges, ignore memory coming from outer loop.
          loopBackwardEdges(loop) map (_.source.block.blockEnd.inMemory)
        } else {
          inMemories(mem)
        })
      }

      // We can move load only if there is a path from get to header.
      worklist contains header
    }

    private def optimizableMemoryRead(x: GetMemoryOperation) = cond(x) {
      case x: GetStatic =>
        !x.field.isAJFlat ||
          env.enabled(MoveGetStaticFlat) ||
          (x.field.getType.isRecord && env.enabled(MoveFlatRecords))

      case x: GetField =>
        !x.field.isAJFlat ||
          env.enabled(MoveGetFieldFlat) ||
          (x.field.getType.isRecord && env.enabled(MoveFlatRecords))

      case x: ArrayGet =>
        !x.arrayType.isRecordArray ||
          env.enabled(MoveArrayGetFlat) ||
          (x.arrayType.isRecordArray && env.enabled(MoveFlatRecords))
    }

    private def inMemories(mem: MemoryNode): Iterator[MemoryNode] = {
      mem match {
        case block: Block => block.reachableMemoriesBefore
        case mem: HasInMemory => Iterator.single(mem.inMemory)
        case _ => shouldNotReachHere(mem)
      }
    }

    private def memoryOnBlockEdge(e: Edge) =
      e.source.asInstanceOf[BlockExit].memoryBefore

  }

  private def hasUnreachableInMemories(mem: MemoryNode): Boolean = cond(mem) {
    case mem: Block => mem.predBlocks exists (_.unreachable)
  }

  private def sameField(f1: Field, f2: Field): Boolean = {
    f1 == f2 || (f1.getDeclaringClass.isArraySlice && f2.getDeclaringClass.isArraySlice && f1.getName == f2.getName)
  }

  // TODO: remove when scala 3 is supported (see https://github.com/scala/bug/issues/4440)
  @nowarn("msg=The outer reference in this type test cannot be checked at run time")
  object MemoryDependencies {
    private def areMemoryDependent(write: MemoryNode, read: GetMemoryOperation, writeIsAboveRead: Boolean) = (write, read) match {
      // Do not pass through entry or entry-like blocks.
      case (x: Block, _) =>
        assert(writeIsAboveRead)
        x.isInstanceOf[XBlock] || hasUnreachableInMemories(x) || x.reachableMemoriesBefore.isEmpty


      case (x: PutStatic, y: GetStatic) => sameField(x.field, y.field)
      case (_: PutField | _: ArrayPut | _: ArrayFill | _: AJArrayFill,
            _: GetStatic) => false

      case (x, y: GetStatic) if isMovableStaticField(y.field) => x match {
        case MemBarrier(kinds) if kinds contains STRICT_MEM => true // respect only StrictMem
        case _ => false
      }

      case (x: PutField, y: GetField) => sameField(x.field, y.field) && mayAliasAt(x.obj, y.obj, x)
      case (_: PutStatic | _: ArrayPut | _: ArrayFill | _: AJArrayFill,
            _: GetField) => false

      case (x: ArrayPut, y: ArrayGet) => mayBeSameIdxs(x.idx, y.idx) && mayAliasAt(x.array, y.array, x)
      case (x: ArrayFill, y: ArrayGet) => mayAliasAt(x.array, y.array, x)
      case (x: AJArrayFill, y: ArrayGet) => mayAliasAt(x.array, y.array, x)
      case (_: PutStatic | _: PutField,
            _: ArrayGet) => false

      case (_, _: LoadMemory | _: UArrayGet) => true


      case (_: MonitorExit, _) => !writeIsAboveRead && !env.enabled(MoveLoadsThroughMonitors)

      case (_: LocalReachabilityShield, _) => false
      case (_: AnyNew | _: NewArrayCopy | _: NewArrayFill | _: NewStackAllocated | _: StrConcat | _: BoxedValue, _) => false
      case (_: NewArrayCopyRT | _: NewArrayRT, _) => false

      case (_: ConvertDomain, _) => false


      case (_: StoreMemory | _: UArrayPut | _: CAS | _: MemAtomic
          | _: StackZeroing | _: Deferred | _: ThinNew | _: Throw
          | _: LockWrapper | _: BitcodeDeferred | _: InitObj, _) => true

      case (_: AbstractCall | _: Clinit | _: PackageInit | _: PackageInitCheck | _: AJCallerClass
          | _: WriteBarrier
          | _: StoreLoadForCell
          | _: CopyStructure // TODO: rewise
          | _: InitStringRecord // TODO: rewise
          | _ : DelayedGet | _ : DelayedPut // TODO: rewise
          | _: DelayedMethodAddr // TODO: rewise
          | _: DelayedInstanceMethodVNum // TODO: rewise
          , _) =>
        // TODO: return false if read.obj does not escape
        true

      case (_: MonitorEnter, _) => writeIsAboveRead && !env.enabled(MoveLoadsThroughMonitors)

      case (MemBarrier(kinds), _) =>
        // Currently we are afraid of all other barriers.
        kinds != Set(STORE_STORE)

      case (_: DebugBreakpoint, _) => true

      case (_: Evacuate, _) => true

      case _ => shouldNotReachHere((write, read))
    }

    def readCouldBeMovedAboveWrite(read: GetMemoryOperation, write: MemoryNode): Boolean =
      !areMemoryDependent(write, read, writeIsAboveRead = true)

    def readCouldBeMovedBelowWrite(read: GetMemoryOperation, write: MemoryNode): Boolean =
      !areMemoryDependent(write, read, writeIsAboveRead = false)
  }

  private def isMovableStaticField(f: Field) = {
    assert(f.isStatic)
    // GetStatic of flat field does not access the memory, only obtains the address,
    // so should be fine to move.
    // TODO: introduce GetElementPtr and use it instead!
    f.isAJFlat || (
      env.enabled(TrustStaticFinalFields) &&
        f.isFinal &&
        // System.out & co. are mutable, ignore them.
        !f.getDeclaringClass.isSystemClass &&
        rootMethod != f.getDeclaringClass.getClinit
    )
  }

  private def mayAliasAt(x: Node, y: Node, point: ControlNode) = (x.tpe, y.tpe) match {
    case (TRefType, TRefType) => nodesMayAliasAt(x, y, point)
    case _ => true
  }

  // TODO: use value range analysis
  private def mayBeSameIdxs(idx1: Node, idx2: Node) = (idx1, idx2) match {
    case (IntegralConst(v1), IntegralConst(v2)) => v1 == v2
    case _ => true
  }

  def combineLoadMemoryAndCast(): Boolean = {
    def checkAddrInc(addr: Node, x: Int) = addr match {
      case addr: Lea if x > 0 => addr.checkDispInc(x)
      case _ => true
    }

    def addAddrConst(addr: Node, x: Int) = addr match {
      case addr: Lea => assert(addr.checkDispInc(x)); addr.withDisp(addr.disp + x)
      case _ =>
        val a = addr.tpe match {
          case AddrType => addr
          case _: RecordAddrType => ReinterpretCast(addr.tpe, AddrType)(addr)
          case _ => shouldNotReachHere(addr)
        }
        Add(a, IntegralConst(AddrType)(x))
    }

    def modifiedLoadMemory(load: LoadMemory, accessType: AsmType, addr: Node) = (load match {
      case load: LoadMemory.Normal => currentScope.inState(load.inCtrl, load.inMemory) {
        LoadMemory(accessType, load.signature, load.atomic)(addr)
      }
      case load: LoadMemory.Independent => currentScope.inState(load.inCtrl, entryMemory) {
        LoadMemory.memoryIndependent(accessType, load.signature, load.atomic)(addr)
      }
      case _ => shouldNotReachHere(load)
    }).asInstanceOf[LoadMemory]

    var changed = false

    def mayChangeAccessType(load: LoadMemory): Boolean = load.addr match {
      case _: StackAlloc => env.enabled(StackAllocSlotsAccessTypeMayDifferFromSlotType)
      case _ => true
    }

    for (load <- all[LoadMemory] if mayChangeAccessType(load)) {
      changed |= cond(ScalaCollections.singleton(load.valueUses)) {
        case Some(bfx: BitFieldExtract) if bfx.dataAligned && (bfx.offset + bfx.size <= load.accessType.width.nbits) =>

        val canShrink = bfx.sizeInBytes < load.accessType.sizeInBytes && (bfx.signExtension || bfx.sizeInBytes == U16.sizeInBytes)
        val canOffset = bfx.offset > 0 && checkAddrInc(load.addr, bfx.offsetInBytes)

        if (canShrink || canOffset) {
          withPos(load) {
            val (offsetLoad, bfxOffset) = if (canOffset) {
              (modifiedLoadMemory(load, load.accessType, addAddrConst(load.addr, bfx.offsetInBytes)), 0)
            } else {
              (load, bfx.offset)
            }

            val (shrinkLoad, bfxSize) = if (canShrink) {
              val loadTpe = ((bfx.signExtension, bfx.size): @unchecked) match {
                case (true,  8)  => I8
                case (false, 8)  => U8
                case (true,  16) => I16
                case (false, 16) => U16
                case (true,  32) => I32
              }
              (modifiedLoadMemory(offsetLoad, loadTpe, offsetLoad.addr), I32.sizeInBits)
            } else {
              (offsetLoad, bfx.size)
            }

            replaceTransitively(bfx, BitFieldExtract(bfx.tpe, bfxOffset, bfxSize, bfx.signExtension, shrinkLoad))
          }
          true
        } else {
          false
        }
      }
    }

    changed
  }
}
