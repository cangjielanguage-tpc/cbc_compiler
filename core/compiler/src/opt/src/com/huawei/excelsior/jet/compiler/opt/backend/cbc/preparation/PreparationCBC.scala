/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.cbc.preparation

import scala.PartialFunction.cond
import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.compiler.Env.isStandalone
import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.abi.ABI.TailSlot
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.BackEndCBC
import com.huawei.excelsior.jet.compiler.opt.backend.preparation.Preparation
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot.NewOnStack
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.PerformMassiveStackZeroingForCBC
import com.huawei.excelsior.jet.compiler.symlevel.{BitcodeFieldReference, BitcodeMethodReference, Field, MethodReference, PermanentMember}

import scala.annotation.tailrec

trait PreparationCBC extends Preparation with FieldChainsCBC { self: Universe with BackEndCBC =>

  override protected def machineDependentStepsBeforeTypeChecksDisabling(): Unit = {
    if (!isStandalone) {
      step("collect field chains", collectFieldChains())
    }
  }
  
  override protected def tryToRecombineLeaToGroupItWithMemoryAccess(rma: RawMemoryAccess, lea: Lea): Unit = {}

  def rematerializeTypeTestAndCombineWithUse(): Unit = {
    for (tt <- all[TypeTest] if tt.hasValueUses && !tt.hasGroup) {
      Node.rematerializeCompletely(tt) foreach { tt =>
        tt.attachToGroup(tt.singleValueUse, Group.AttachReason.COND_BRANCH_ARG)
      }
    }
  }

  private def combineInstanceOfCmpBranch(): Unit = {
    for (cmp @ CmpAnyInstanceOf(iof, _, _) <- allNodes if cmp.isGroupRoot && iof.valueUses.size == 1) {
      cmp.valueUses.toSeq match {
        case Seq(iff: If) =>
          // TODO-REDESIGN-GROUPS
          // Can actively try to pull extra nodes up or down, but it's only an optimization
          iof.attachToGroup(iff, reason = Group.AttachReason.INSTANCE_OF_BRANCH)
          cmp.attachToGroup(iff, reason = Group.AttachReason.COND_BRANCH_ARG)
          stats.count(StatsKind.CangjieBranchIfCombining, s"combined ${iof.simpleName}, ${cmp.simpleName} and ${iff.simpleName}")

        case _ =>
      }
    }
  }

  override protected def recombineFlagProducers(): Unit = {
    step        ("group all type tests with if", rematerializeTypeTestAndCombineWithUse())
    optimizeStep("group all iof cmp with if",    combineInstanceOfCmpBranch())
  }

  // CBC does not support Load and BFX grouping in code generation.
  override def groupLoadAndBFX(): Unit = {}

  override def convertBFXToAnd(): Unit = {}

  override def insertStackAllocZeroing(): Unit = {

    def isRecord(sa: StackAlloc) = sa match {
      case StackAlloc.Local(allocType) if allocType.isRecord => true
      case StackAlloc.DebugVar(tpe, _) if tpe.isRecord => true
      case _ => false
    }

    def isNewAlloc(sa: StackAlloc) = sa.kind match {
      case FrameSlot.NewOnStack(_) => true
      case _ => false
    }

    val stackAllocs = all[StackAlloc].filterNot(x => isRecord(x) || isNewAlloc(x))
    if (env.enabled(PerformMassiveStackZeroingForCBC) /* for JET-17840 */ && stackAllocs.exists(_.zeroed)) {
      insertCodeAfter(entryBlock) { StackZeroing.Massive() }
    }

    for (sa <- all[StackAlloc] filter isRecord if sa.zeroed) {
      insertCodeAfter(entryBlock) { ZeroRefs(sa) }
    }
  }

  /** Group [[MutFunc.Offset]] and [[MutFunc.OffsetCBC]] nodes with calls
    * and [[MutFunc.Combine]] with all usages, that support MemExpr addressing
    */
  override protected def prepareMutFuncNodes(): Unit = {
    def isMutCall(n: Node) = cond(n) {
      case call: Call => call.targetRef.isCangjieMut
    }

    def supportsMemExpr(n: Node) = cond(n) {
      case _: GetField | _: BitcodeDeferred.GetField | _: UniversalGeneric.GetField | _: FieldChainRead  | _: LoadMemory  => true
      case _: PutField | _: BitcodeDeferred.PutField | _: UniversalGeneric.PutField | _: FieldChainWrite | _: StoreMemory => true
      case _: CopyStructureCBC => true
    }

    def checkUses(mut: Node, check: Node => Boolean): Unit = assert(mut.valueUses.forall(check), s"$mut, ${mut.valueUses.toList}")

    assert(all[MutFunc.HostGlobal].isEmpty) // cannot appear in CBC

    for (mut <- (all[MutFunc.OffsetCBC] ++ all[MutFunc.Offset]).toList) {
      checkUses(mut, isMutCall)
      Node.rematerializeCompletely(mut).foreach { m =>
        m.attachToGroup(m.singleUse, Group.AttachReason.MUT_FUNC_ARG)
      }
    }

    for (mut <- all[MutFunc.Combine].toList) {
      Node.rematerializeCompletely(mut).foreach { m =>
        m.singleUse match {
          case StoreMemory(StackAlloc.DebugVar(_, _)) => // TODO: split debug vars into pairs for mut func receiver value (JET-17503)
          case use if supportsMemExpr(use) => m.attachToGroup(use, Group.AttachReason.MUT_FUNC_ARG)
          case _: CopyStructure =>
          case _: Call => // TODO: check in runtime, that host is null or permanent object, after CopyStructure support (JET-17500)
          case use => shouldNotReachHere(use)
        }
      }
    }
  }

  override def prepareDerivedPtr(): Unit = {
    if (isStandalone) {
      for {
        n <- all[DerivedPtr].toList
        m <- Node.rematerializeCompletely(n)
      } {
        m.singleUse match {
          case use: InstanceFieldSeqOperation => m.attachToGroup(use, Group.AttachReason.DERIVED_PTR)
          case use: Call =>
          case use => shouldNotReachHere(use)
        }
      }
      for {
        n <- all[GetFieldSeqRef].toList
        m <- Node.rematerializeCompletely(n)
      } {
        m.singleUse match {
          case use: Call =>
          case use: Box =>
          case use: CopyStructure =>
          case use => shouldNotReachHere(use)
        }
      }
    }
  }

  override def prepareCopyStructure(): Unit = {
//    if (!isStandalone) {
//      return
//    }
//
//    def attachToCopyStructure(cs: CopyStructure, node: FloatingNode): Unit = {
//      Node.rematerializeConditionally(node, {
//        _.target == cs
//      }) foreach {
//        _.attachToGroup(cs, Group.AttachReason.COPY_STRUCTURE)
//      }
//    }
//
//    for {
//      cs <- all[CopyStructure]
//    } {
//      (cs.src, cs.dst) match {
//        case ((_: GetStaticFieldSeqRef | _: GetFieldSeqRef), (_: GetStaticFieldSeqRef | _: GetFieldSeqRef)) => {
//          shouldNotReachHere()
//        }
//        case (g: (GetStaticFieldSeqRef | GetFieldSeqRef), _) => {
//          attachToCopyStructure(cs, g)
//        }
//        case (_, g: (GetStaticFieldSeqRef | GetFieldSeqRef)) => {
//          attachToCopyStructure(cs, g)
//        }
//        case (_, _) =>
//      }
//    }
  }

  override def prepareCangjieReferenceNode(): Unit = {
    if (isStandalone) {
      for {
        n <- all[CangjieReferenceNode].toList
        m <- Node.rematerializeCompletely(n)
      } {
        m.singleUse match {
          case use: (FieldSeqOperation | CopyStructure) => m.attachToGroup(use, Group.AttachReason.CANGJIE_REFERENCE)
          case use => shouldNotReachHere(use)
        }
      }
    }
  }

  override def prepareRecordArrayGet(): Unit = {
    if (isStandalone) {
      for {
        n <- all[ArrayGet].toList
        if n.arrayType.isRecordArray
        m <- Node.rematerializeCompletely(n)
      } {
        m.singleUse match {
          case use: (InstanceFieldSeqOperation | CopyStructure) => m.attachToGroup(use, Group.AttachReason.RECORD_ARRAY_GET)
          case use => shouldNotReachHere(use)
        }
      }
    } else {
      for {
        n <- all[RecordArrayGet].toList
        m <- Node.rematerializeCompletely(n)
      } {
        m.singleUse match {
          case use: (FieldChainRead | FieldChainWrite | CopyStructure | CopyStructureCBC) => m.attachToGroup(use, Group.AttachReason.RECORD_ARRAY_GET)
          case use => shouldNotReachHere(use)
        }
      }
    }
  }
}
