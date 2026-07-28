/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.codegen

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.*
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.lowering.PreLowering
import com.huawei.excelsior.jet.compiler.opt.middle.CompensatoryRecordZeroing
import com.huawei.excelsior.jet.compiler.options.BoolOption.SmartRecordZeroing
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.util.Closure

import scala.PartialFunction.condOpt
import scala.util.boundary
import scala.util.boundary.break

/** Provides information about reference fields of local records alive at given point.
  *
  * @author arxdukalis
  * @author conwor
  */
trait RecordSlotsLiveness { self: Universe with BackEnd =>

  // TODO: perform liveness analysis on actual sub-slots
  protected type SubSlot = (FrameSlot, Int)

  /** Map from every point in IR which [[needGCMap]] to set of stack sub-slots (record slot + offset)
    * occupied by reference record fields alive at this point.
    */
  protected lazy val recordSlotsAliveAt: collection.Map[Node, Set[SubSlot]] = boundary {
    assert(env.enabled(SmartRecordZeroing))

    lazy val recordStackAllocs = allNodes.collect { case Record.Producer(sa) => sa }.toSeq
    if (recordStackAllocs.isEmpty) {
      // Don't perform analysis if current method doesn't contain stack-allocated records
      break(Map.empty)
    }

    /** Operations on records that must be specially processed while performing liveness analysis. */
    object Record {

      /** Node that ''produces'' stack-allocated record. Record may be zero-initialized (`sa.zeroed`) or uninitialized.
        *
        * When observing zero-initialized producer, liveness analysis should remove all its reference fields
        * from the set of live values.
        */
      object Producer {
        def unapply(sa: StackAlloc): Option[StackAlloc] = condOpt(sa.kind) {
          case FrameSlot.Typed(t) if t.isRecord => sa
        }
      }

      /** [[Producer]] or its transfer to the different resource. */
      object Value {
        // Can't use `Values` framework here because we need to account copies with own values.
        private lazy val stackAllocByValue = (for {
          sa <- recordStackAllocs
          v <- Closure[Node](sa)(n => n.groupedValueUses.collect { case c: Copy => c ensuring (_.transferArg == n) })
        } yield (v, sa)).toMap

        def unapply(n: Node): Option[StackAlloc] = stackAllocByValue get n
      }

      /** Call to the method ''initializing'' stack-allocated record (filling its fields with initial values).
        *
        * After such call, stack-allocated record passed to it is considered traceable by GC
        * (since all its fields hold correct references).
        *
        * Initializers are:
        *   1. Record constructor calls.
        *      For such calls, record is considered traceable __throughout whole constructor body__,
        *      since it's modified directly, without copying. Record fields that may not be initialized
        *      at the time callee gc point is triggered, are zeroed in
        *      [[CompensatoryRecordZeroing.InBackEnd.insertZeroingInRecordConstructor]].
        *   1. Calls of AJ methods marked with `@RecordInitializer` annotation. For such calls,
        *      record is also considered traceable, and zeroed using
        *      [[CompensatoryRecordZeroing.InFrontEnd.insertZeroingForAJRecordInitializers]].
        *   1. Calls of other methods "returning" record from it. Current implementation of such
        *      calls involves the allocation of "return" value in the caller frame and passing it
        *      to the callee as synthesized last argument. Callee operates with own version
        *      of record, allocated on its frame, and transfers result value to the caller by
        *      copying it to the location specified in the last parameter. Therefore, there is
        *      no need to trace record, placed in the caller frame, during the execution of
        *      callee, unless callee contains GC points in the epilogue. For such cases, to
        *      avoid the unnecessary zeroing, epilogue GC points are moved to the position
        *      before result copying (see [[PreLowering.insertGCPointsInRecordReturningMethods]]).
        */
      object Initializer {
        private def isRecordConstructor(c: Call) = c.targetRef.hasMethod && c.targetRef.method.isRecordConstructor
        private def isRecordInitializer(c: Call) = c.targetRef.hasMethod && c.targetRef.method.isRecordInitializer
        private def returnsRecord(c: Call) = c.methodType.returnType.isRecord

        def unapply(c: Call): Option[(StackAlloc, Seq[Node], Boolean)] = condOpt(c.invokeArgs) {
          case MutFunc.Offset(_: MutFunc.Host, Value(sa)) +: restArgs if isRecordConstructor(c) => (sa, restArgs, true)
          case MutFunc.OffsetCBC(_, Value(sa)) +: restArgs if isRecordConstructor(c) => (sa, restArgs, true)
          case Value(sa) +: restArgs if isRecordInitializer(c) || isRecordConstructor(c) => (sa, restArgs, true)
          case Value(sa) +: restArgs if returnsRecord(c) => (sa, restArgs, false)
        }
      }

      /** Load or store memory operation accessing reference field of record. */
      private object ReadWrite {
        def unapply(n: LoadStoreMemoryAccess): Option[(StackAlloc, Int)] = if (n.accessType == AsmType.PTR) {
          condOpt(n.addr) {
            case Lea.AnyWithBase(Value(sa), disp) => (sa, disp)
            case Value(sa) => (sa, 0)
          }
        } else {
          None
        }
      }

      /** Operation putting value to the reference field of record.
        *
        * When observing it, liveness analysis should '''exclude''' corresponding
        * reference field information from the set of live values.
        */
      object Write {
        def unapply(sm: StoreMemory) = ReadWrite.unapply(sm)
      }

      /** Operation reading value from the reference field of record.
        *
        * When observing it, liveness analysis should '''include''' corresponding
        * reference field information to the set of live values.
        */
      object Read {
        def unapply(lm: LoadMemory) = ReadWrite.unapply(lm)
      }
    }

    implicit object SubSlotSets extends Sets.Default[SubSlot]

    val engine = new LivenessEngine[SubSlot] {

      private def extractReferenceSubSlots(sa: StackAlloc): Array[SubSlot] = {
        val slot = sa.slot
        val FrameSlot.Typed(allocType) = slot.kind
        asClassType(allocType).getRefFieldOffsets.map((slot, _))
      }

      override def valuesMapping(it: IterableOnce[Node]): IterableOnce[SubSlot] = it.iterator.flatMap {
        case Record.Value(sa) => extractReferenceSubSlots(sa)
        case _ => Iterator.empty
      }

      override protected def processBlock(block: Block, output: Set[SubSlot], updateLive: (Node, Set[SubSlot]) => Unit): Set[SubSlot] = {
        var curr = output

        for (node <- CodeOrder reversedIn block) {
          var needToUpdateLive = true

          node match {
            case _: Copy | _: Lea | _: Phi | _: Proxy | _: Constraints =>

            case _: MutFunc.OffsetCBC | _: MutFunc.Offset =>

            case n: LoadStoreMemoryAccess if n.accessType != AsmType.PTR => 

            case Record.Producer(sa) =>
              // if StackAlloc is not zeroed then it is uninitialized and no slots of record should be considered alive
              val referenceSubSlots = extractReferenceSubSlots(sa)
              assert(sa.zeroed || !referenceSubSlots.exists(curr))
              curr --= referenceSubSlots

            case Record.Write(sa, offset) =>
              curr -= (sa.slot, offset)

            case Record.Read(sa, offset) =>
              curr += (sa.slot, offset)

            case Record.Initializer(sa, args, shouldTraceStackAlloc) =>
              curr ++= valuesMapping(args)

              val subSlots = extractReferenceSubSlots(sa)
              if (shouldTraceStackAlloc) {
                updateLive(node, curr ++ subSlots)
                needToUpdateLive = false
              }
              curr --= subSlots

            case _: Call =>
              curr ++= valuesMapping(node.groupedValueArgs)

            case _: CopyStructure =>
              // TODO: Consider code generation for CopyStructure nodes.
              //       If it's reasonable, properly integrate them in this analysis.
              shouldNotReachHere()

            case _ =>
              assert(valuesMapping(node.groupedValueArgs).iterator.isEmpty)
          }

          if (needToUpdateLive && needGCMap(node)) {
            updateLive(node, curr)
          }
        }

        curr
      }
    }

    engine.calcLiveness()
    engine.live filter (x => needGCMap(x._1))
  }

  protected lazy val containsRecordSlots: Boolean = recordSlotsAliveAt.nonEmpty
}
