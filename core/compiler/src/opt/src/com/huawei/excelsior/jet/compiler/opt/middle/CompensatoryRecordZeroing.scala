/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase.{Lowering, PreInline}
import com.huawei.excelsior.jet.compiler.opt.backend.codegen.XSitesToolbox
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.SmartRecordZeroing
import com.huawei.excelsior.jet.compiler.symlevel.{Method, SignatureType}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.util.graph.analysis.DataFlowAnalysis

/** Transformations inserting record zeroing in cases where
  * record is considered traceable before it is fully initialized.
  *
  * @see [[com.huawei.excelsior.jet.compiler.opt.backend.codegen.RecordSlotsLiveness RecordSlotsLiveness]]
  */
object CompensatoryRecordZeroing {

  trait InFrontEnd { self: Universe =>

    /** Insert reference field zeroing for records passed to AJ methods
      * with `@RecordInitializer` annotation or methods replaced with them.
      */
    def insertZeroingForAJRecordInitializers(): Boolean = {
      if (!env.enabled(SmartRecordZeroing)) return false

      // Necessary zeroing is already inserted before replaced method calls
      if (rootMethod.isAJReplaced) return false

      assert(currentPhase < PreInline)

      var changed = false

      allNodes foreach {
        case call: Call if call.targetRef.hasMethod && call.targetRef.method.isRecordInitializer =>
          val sa = call.invokeArgs.head match {
            case sa: StackAlloc =>
              // This pattern occurs when stack allocated record is passed to
              // method replaced by AJ method with `@RecordInitializer` annotation
              sa

            case ReinterpretCast(_: RecordAddrType, AddrType, sa: StackAlloc) =>
              // We match this pattern because all stack allocated records
              // passed to AJ methods directly are reinterpreted as addresses 
              // to satisfy AJ method signatures
              sa

            case n =>
              shouldNotReachHere(s"first argument of @RecordInitializer call should be stack allocated record, got $n")
          }

          val recordType = sa.tpe match {
            case RecordAddrType(t) => t
            case t => shouldNotReachHere(s"first argument of @RecordInitializer call should be stack allocated record, got $sa with type $t")
          }

          if (recordType.hasRefFields) {
            insertCodeBefore(call) { ZeroRefs(sa) }
            changed = true
          }

        case _ =>
      }

      if (changed) {
        dbgPrinter.debugNodes("All graph after inserting zeroing for record initializers")
      }

      changed
    }
  }

  trait InBackEnd { self: Universe with XSitesToolbox =>

    /** If current method is record constructor, obtains the set of record reference fields,
      * which may not be initialized at the time GC is running, and zeroes it.
      */
    def insertZeroingInRecordConstructor(): Boolean = {
      if (!env.enabled(SmartRecordZeroing) || !rootMethod.isRecordConstructor) return false

      assert(currentPhase > Lowering)

      val receiver = MutParam(rootMethod, rootMethodParam)
      val RecordAddrType(recordType) = receiver.tpe

      val refFieldOffsets = asClassType(recordType).getRefFieldOffsets
      if (refFieldOffsets.isEmpty) return false

      lazy val uninitializedFieldAnalysis = new DataFlowAnalysis[ControlNode](spinalCFG) {
        override type State = Set[Int]

        override def init = refFieldOffsets.toSet

        override def join(outputStates: IterableOnce[Set[Int]]) = outputStates.iterator reduce (_ union _)

        private val zeroOffset = 0

        override protected def trans(node: ControlNode, inputState: Set[Int]) = node match {
          case sm: StoreMemory if sm.accessType == AsmType.PTR =>
            sm.addr match {
              case Lea.AnyWithBase(`receiver`, disp) => inputState - disp
              case `receiver` => inputState - zeroOffset
              case _ => inputState
            }
          case _ => inputState
        }
      }

      val uninitializedFieldOffsets = all[ControlNode]
        .collect { case n if needGCMap(n) => uninitializedFieldAnalysis.in(n) }
        .fold(Set.empty)(_ union _)

      if (uninitializedFieldOffsets.isEmpty) return false

      insertCodeAfter(entryBlock) {
        for (offset <- uninitializedFieldOffsets.toSeq.sorted) {
          val fieldAddr = Lea.Base(receiver, offset)
          StoreMemory(accessType = AsmType.PTR, sig = SignatureType.fromSymType(typeProvider.getAJObjectType), atomic = false)(fieldAddr, Null())
        }
      }

      stats.count(
        StatsKind.CompensatoryZeroingForRecords,
        s"${refFieldOffsets.length} reference fields total, ${uninitializedFieldOffsets.size} fields zeroed",
        receiver)

      true
    }
  }

}
