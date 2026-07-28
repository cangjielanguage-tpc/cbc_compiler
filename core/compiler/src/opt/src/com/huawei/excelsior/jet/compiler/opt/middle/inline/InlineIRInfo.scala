/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.inline

import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.inline.scales.Scales
import com.huawei.excelsior.jet.compiler.opt.middle.sync.SyncParamsAnalysis
import com.huawei.excelsior.jet.util.ScalaCollections.sumBy
import com.huawei.excelsior.jet.util.graph.ordering.TopSort

import scala.PartialFunction.cond
import scala.collection.mutable

trait InlineIRInfo extends Scales with SyncParamsAnalysis { this: Universe =>

  object InlineIRInfo {

    def isScalarMethod = rootMethod.isStatic && allNodes.forall {
      // Note: BlockEnd is here only because it has memoryIn argument
      case _: BlockEnd | _: Clinit | _: PackageInit | _: PackageInitCheck | _: IDivRemOp | _: DivisorCheck => true
      case FreeSpinal() => true
      case RecordOperations() => true
      case LambdaOperations() => true
      case _: HasInMemory => false
      case _ => true
    }

    def inlinedBodyWeight: Double = {
      val scalarMethod = isScalarMethod
      def clinitsInScalar(n: Node) = scalarMethod && cond(n) { case _: Clinit | _: PackageInit | _: PackageInitCheck => true }

      sumBy(allNodes filterNot clinitsInScalar)(nodeWeight)
    }

    def bodySyncOperationsWeight: Double = sumBy(all[MonitorOperation])(nodeWeight)

    /** Context-free inlinable. */
    def isCFI(guarded: Boolean): Boolean = {
      // Note: some cases are not CFI if using guarded inline

      if (rootMethod.isNeverInline) {
        return false
      }

      // scalar without arguments
      if (rootMethod.getParamsCount == 0 && isScalarMethod) {
        return true
      }

      Return.unique match {
        // math intrinsic in std.math (potentially with non-trivial CFG)
        case Some(Return(_, _, m: MathIntrinsic)) if m.args.forall(isFreeNode) &&
          rootDeclaringClass.isCangjiePackage && rootDeclaringClass.getName == CangjieSymLevelMaker.STD_MATH_PACKAGE_NAME =>
          return true
        case _ =>
      }

      val block = entryBlock
      val entryMem = entryMemory

      !block.hasXHandlers && (block.blockEnd match {

        // empty
        case Return(WithoutFreeCtrl(`block`), mem, FreeNode()) =>
          assert(mem == entryMem)
          true

        // forwarder
        case Return(call: Call,
                    mem,
                    EOPConvert.Skipped(retVal))
          if nonFreeInCtrl(call.inCtrl) == block &&
            !guarded &&
            (retVal == call || isFreeNode(retVal)) &&
            (call.invokeArgs forall isFreeNode) =>

          assert(call.inMemory == entryMem)
          assert(mem == call)
          true

        // getter
        case Return(WithoutFreeCtrl(`block`),
                    mem,
                    EOPConvert.Skipped(GetField(_, WithoutFreeCtrl(`block`), getMem, ReceiverParam()))) =>

          assert(mem == entryMem)
          assert(getMem == entryMem)
          true

        // setter
        case Return(put @ PutFieldUnadjusted(WithoutFreeCtrl(`block`), putMem, ReceiverParam(), FreeNode()),
                    mem,
                    FreeNode())
          if !guarded =>

          assert(putMem == entryMem)
          assert(mem == put)
          true

        case _ => false
      })
    }

    /** Parameter `p` is `synchronized` if:
      * <ul>
      *   <li> There exists `MonitorEnter(p)`.
      *   <li> `p` passed as i-th parameter to function `foo` and `foo.synchronizedParams` contains `i`.
      * </ul>
      */
    def synchronizedParams: Set[Int] = {
      if (rootMethodParams.isEmpty || isUnstructuredLocking) {
        Set.empty
      } else {
        val enterParams = all[MonitorEnter] collect { case MonitorEnter(Param(i)) => i }
        val syncedParams = allSyncedParams collect { case Param(i) => i }
        (enterParams ++ syncedParams).toSet
      }
    }

    /** True if node is free for inline. */
    private def isFreeNode(node: Node) = node match {
      case EOPConvert.Skipped(_: Param | _: Constant) => true
      case _ => false
    }

    private object FreeNode {
      def unapply(n: Node): Boolean = isFreeNode(n)
    }

    private object PutFieldUnadjusted {
      def unapply(n: PutField) = Some(n.inCtrl, n.inMemory, n.obj, n.inValue0)
    }

    private object FreeSpinal {
      def unapply(n: SpinalNode): Boolean = n match {
        case _: Marker | _: AssertNode | _: PreparationCheck => true
        case n: PureCheck => n.trusted
        case _ => false
      }
    }

    private def nonFreeInCtrl(n: UpperPoint): UpperPoint = n match {
      case n @ FreeSpinal() => nonFreeInCtrl(n.inCtrl)
      case _ => n
    }

    private object WithoutFreeCtrl {
      def unapply(n: UpperPoint) = Some(nonFreeInCtrl(n))
    }

    private object RecordOperations {
      def unapply(n: Node): Boolean = n match {
        case StackAlloc.Local(t) => t.isRecord
        case n: InstanceFieldOperation => n.obj.tpe.isInstanceOf[RecordAddrType]
        case n: CopyStructure => n.structureType.isRecord
        case _ => false
      }
    }

    private object LambdaOperations {
      def unapply(n: Node): Boolean = n match {
        case n: NewStackAllocated      => n.allocType.symType.isCangjieLambdaClass
        case n: New                    => n.allocType.symType.isCangjieLambdaClass
        case n: InstanceFieldOperation => LambdaOperations.unapply(n.obj)
        case _ => false
      }
    }

    def inlinedBodyDuration: Double =
      if (hasLoops || hasNonQuickNodes) Double.PositiveInfinity
      else 0

    private def hasLoops = {
      val ts = cfg.topSort
      ts.order exists { b => b.succBlocks exists (ts.gteq(b, _)) }
    }

    private def hasNonQuickNodes =
      allNodes exists { n =>
        n.isInstanceOf[AbstractCall] || n.isInstanceOf[MonitorEnter]
      }

    def leaf: Boolean =
      !(allNodes exists (_.isInstanceOf[AbstractCall]))

    import Java.Lang.MathIntrinsic._
    def badForCBC: Boolean = allNodes exists {
      case call: Call if call.targetRef.methodType.isCJForeign =>
        // foreign functions are bad as they don't have proper CUDs in a compiled TD
        // TODO: enable (JET-16144)
        true

      case _: SynchronizedRegion | _: MonitorEnter | _: MonitorExit =>
        // JET-16213
        true

      case CallMethod(target, _, _) if !target.getDeclaringClass.hasRunTimeTypeInfo =>
        // JET-16210
        // methods of non-managed AJ from classes which do not have TD are bad
        true

      case _: MemBarrier | _: CAS | _: MemAtomic =>
        true // TODO: check support in LoweringCBC (JET-16142)

      case _: BitSwap =>
        true // TODO: support BitSwap in CBC (JET-16140)

      case _: ExecEnv =>
        true // TODO: support ExecEnv in CBC (JET-16139)

      case MathIntrinsic(D_ABS | F_ABS | D_SQRT | F_SQRT) =>
        false // Supported CBC math intrinsics

      case _: MathIntrinsic =>
        true // TODO: support missing math intrinsics (JET-17450)

      case _ => false
    }

    def alwaysEvacuatedParams: Set[Int] = {
      val res = mutable.Set.empty[Int]
      for (p <- all[Param] if !p.isReceiver && paramHasUnconditionalEscape(p)) {
        res += p.num
      }
      res.toSet
    }
  }
}
