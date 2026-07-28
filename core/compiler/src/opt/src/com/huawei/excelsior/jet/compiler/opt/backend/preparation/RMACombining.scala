/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.preparation

import com.huawei.excelsior.common.Arch.AMD64
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.{AsmType, Width}
import com.huawei.excelsior.jet.compiler.Env.targetArch
import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.{CountedLoopsRecognizer, DCEComponent}
import com.huawei.excelsior.jet.compiler.options.BoolOption.GenTDBarriers
import com.huawei.excelsior.jet.util.ScalaCollections.groupBy
import com.huawei.excelsior.jet.compiler.util.Log
import com.huawei.excelsior.jet.util.ScalaCollections

import scala.PartialFunction.cond

/** Steps with RMA addresses combining and rematerialization.
  *
  * @author conwor
  */
trait RMACombining extends CountedLoopsRecognizer { self: Universe with BackEnd with DCEComponent =>

  private def log = Log(Log.Kind.RMACombining)

  private[preparation] def createLeaForRMA(): Unit = {
    if (!lowLevelMemoryOperationsAddressesCombiningInLeaHasImpact) return
    
    log.inSession("rma combining", codeUnit) {

      lazy val loops = cfg.loops

      lazy val inductiveVariables: collection.SeqMap[Phi, Seq[InductiveVariable]] = {
        val inductive = loops.seq flatMap (findInductiveVariables(_))
        groupBy(inductive)(_.index)
      }

      def indexInRange(x: Node, upperPoint: UpperPoint): Boolean = {
        val ranges = calcValueRanges(x, upperPoint.outCtrl, inductiveVariables.getOrElse(_, Nil), extendedRanges = true)
        ranges.exists {
          case ConstValueRange(_, from, _, _) if from >= 0 => true
          case HalfSymbolicValueRange(_, from, _, _, _) if from >= 0 => true
          case _ => false
        }
      }

      for (rma <- all[RawMemoryAccess]) {
        // 1. Combine Lea from address arithmetic
        rma.addr match {
          case Lea.ArithPattern(lea) => rma.addr = lea
          case _ =>
        }

        // 2. Trying to convert Lea with extended rules, not covered by Identities and Lea.ArithPattern unapply.
        rma.addr match {
          case lea @ Lea.Scaled(base, Add(index, DWordConst(disp1)), scale, disp2) if lea.checkDispInc(disp1, scale) =>
            val canTransform = rma match {
              case _ if !lea.undefinedForNegativeIndex => true
              case node: HasInControl if indexInRange(index, node.inCtrl) => true
              case _ => false
            }

            if (canTransform) {
              log("- rma combined", lea.pos)
              stats.count(StatsKind.RMACombining, s"RMA combined", lea)
              rma.addr = Lea.Scaled(base, index, scale, disp1 * scale + disp2)
            }

          case _ =>
        }
      }
    }
  }

  // TODO: there could be more intellectual heuristics with reusing addressing modes in different RMA nodes
  // TODO: in case of no scale and not allowed displacement we can split displacement to 2 parts,
  //  one of which may be included into RMA node.
  protected def tryToRecombineLeaToGroupItWithMemoryAccess(rma: RawMemoryAccess, lea: Lea): Unit

  private[preparation] def recombineRematerializeAndGroupRMAAndLea(): Unit = {
    if (!lowLevelMemoryOperationsAddressesCombiningInLeaHasImpact) return

    assert(valueNumberingEnabled && !identityEnabled)

    // 1. If `rma` address is Lea, that could not be grouped with it, try to recombine it
    for (rma <- all[RawMemoryAccess]) {
      rma.addr match {
        case lea: Lea if !memoryAccessCanBeGroupedWithLea(rma, lea) =>
          tryToRecombineLeaToGroupItWithMemoryAccess(rma, lea)
        case _ =>
      }
    }

    disableValueNumbering()

    // 2. If `rma` address is Lea with several uses, try to rematerialize it
    for (lea <- all[Lea] if lea.valueUses.size > 1) {
      lea match {
        case Lea.Baseless(_, _, _) =>
          assert(!lea.valueUses.exists(_.isInstanceOf[RawMemoryAccess]))

        case Lea.Scaled(_, _, _, _) =>
          // Lea with index may be better to calculate in register, because its rematerialization may increase
          // RP, even if rematerialized copies will be grouped with RMA nodes.
          // TODO: move this decision to BGCM.

        case lea @ Lea.AnyWithBase(_: ExecEnv, _) if lea.valueUses.exists(_.isInstanceOf[RawMemoryAccess]) =>
          // If Lea base is EE, it will be better to rematerialize it even if there are non-RMA usages,
          // because EE live range is the whole method code, and calculation of this Lea to register will
          // increases RP for sure.
          Node.rematerializeCompletely(lea)

        case lea: Lea =>
          // If all uses are RMA nodes, Lea copies will be attached to their groups.
          Node.rematerializeConditionally(lea, cond(_) {
            case Edge(lea: Lea, rma: RawMemoryAccess) => memoryAccessCanBeGroupedWithLea(rma, lea)
          })
      }
    }

    // 3. Finally, if `rma` address is Lea with one use and it could be grouped with `rma`, do it.
    for (rma <- all[RawMemoryAccess]) {
      rma.addr match {
        case lea: Lea if (lea.uses.size == 1) && memoryAccessCanBeGroupedWithLea(rma, lea) =>
          lea.attachToGroup(rma, reason = Group.AttachReason.INLINE_ADDR_MODE)
        case _ =>
      }
    }
  }

  private[preparation] def groupLoadAndBFX(): Unit = {
    for (bfx <- all[BitFieldExtract]) {
      bfx match {
        case BitFieldExtract(0, size, _, load: LoadMemory) if load.valueUses.size == 1 &&
            bfx.dataAligned && size <= load.accessType.width.nbits &&
            (bfx.tpe == IntType || targetArch == AMD64) && // TODO: teach arm64 code emitter emit ldrsw
            load.attachedByReason(reason = Group.AttachReason.INLINE_ADDR_MODE).forall {
              case lea: Lea => isAccessTypeConformsLea(AsmType.integral(Width(size / 8), signed = true), lea) } =>
          bfx.attachToGroup(load, Group.AttachReason.LOAD_EXTEND_RESULT)

        case _ =>
      }
    }
  }

  private[preparation] def protectNodesWithTDBarriers(): Unit = withIncrementalGCM {
    if (!env.enabled(GenTDBarriers)) {
      return
    }

    if (!rootMethod.isManaged) {
      // GC code could replace .td field of object with adjustDesc value, so we could not make TD barrier in such code.
      // Luckily GC code could never be inlined in managed code (go to @afilatov if it will became wrong).
      return
    }

    // TODO: support NoTDBarrierMarker

    def protect(edge: Edge, inCtrl: ControlNode, argMayBeNull: Boolean, argMayBeRich: Boolean): Unit = {
      val src = edge.source

      edge.source = src.tpe match {
        case tpe if tpe.isTraceableRefType => TDBarrier(argMayBeNull, argMayBeRich)(inCtrl, src)

        case tpe if tpe == IntraReferenceType => src match {
          case lea @ Lea.AnyWithBase(base, disp) =>
            assert(base.tpe.isTraceableRefType)
            lea.withBaseAndDisp(TDBarrier(argMayBeNull, argMayBeRich)(inCtrl, base), disp)
        }

        case _ => return
      }
    }

    allNodes foreach {
      case CmpWithNull() =>

      case cmp: Cmp =>
        protect(cmp.lEdge, entryBlock, argMayBeNull = true, argMayBeRich = true)
        protect(cmp.rEdge, entryBlock, argMayBeNull = true, argMayBeRich = true)

      case load: LoadMemory =>
        protect(load.addrEdge, load.inCtrl, argMayBeNull = false, argMayBeRich = false)

      case store: StoreMemory =>
        protect(store.addrEdge, store.inCtrl, argMayBeNull = false, argMayBeRich = false)
        protect(store.inValue0Edge, entryBlock, argMayBeNull = true, argMayBeRich = store.signature.isInterface)

      case _ =>
    }
  }
}
