/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.preparation

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.common.Language.CANGJIE
import com.huawei.excelsior.jet.assembler.Location.{AnyReg, MemBased}
import com.huawei.excelsior.jet.compiler.Env
import com.huawei.excelsior.jet.compiler.Env.{languagePack, targetArch}
import com.huawei.excelsior.jet.compiler.abi.ABI.{AltLocation, TailSlot}
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.{Field, MethodReferenceAccessKind}
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.ScalaCollections

import scala.PartialFunction.{cond, condOpt}
import scala.annotation.tailrec

/** Unclassified preparation steps.
  *
  * @author conwor
  */
trait SpecialSteps { self: Universe with BackEnd =>

  private[preparation] def removeRedundantCasts(): Unit = {
    object NopCast {
      def unapply(node: Cast) = cond(node) {
        case ReinterpretCast(ThinType, AddrType, _) | ReinterpretCast(AddrType, ThinType, _) |
             ReinterpretCast(AddrType | IntraReferenceType, _: RecordAddrType, _) |
             ReinterpretCast(_: RecordAddrType, AddrType | IntraReferenceType, _) |
             ReinterpretCast(_: RecordAddrType, _: RecordAddrType, _) => true
        case ReinterpretCast(EopType.Any, _: EopType, _) => true // required for rich decomposition and write barrier lowering
        case ReinterpretCast(EopType.Plain, EopType.Null, _) => true
        case ReinterpretCast(_, _: EopType.Eop, _) => true
        case ReinterpretCast(TRefType, AddrType, _) => shouldNotReachHere(s"ConcealRef should be used instead of $node")
        case ReinterpretCast(AddrType, TRefType, _) => shouldNotReachHere(s"PublishRef should be used instead of $node")
      }
    }

    def singleUseOrNull(node: Node) = ScalaCollections.singleton(node.uses).orNull

    /** Such casts could be omitted but they break type checks. */
    def redundantCast(node: Node) = condOpt(node) {
      case Shift(ArithOp.LSL, ex @ BitFieldExtract(0, size, _, _), IConst(n))
        if size + n >= typeSizeInBits(node.tpe) && singleUseOrNull(ex) == node => ex
      case node @ NopCast() => node
    }

    bulkReplace {
      allNodes flatMap redundantCast foreach {
        cast => replaceTransitively(cast, cast.arg)
      }
    }
  }

  /** Recreate [[MutFunc.Combine]] nodes with [[IntraReferenceType]] for automatic rematerialization. */
  protected def prepareMutFuncNodes(): Unit = {
    for (combine @ MutFunc.Combine(host, offset) <- all[MutFunc.Combine].toList if combine.isGroupRoot) {
      combine.replaceBy(MutFunc.Combine(host, offset, IntraReferenceType))
    }
  }

  protected def prepareDerivedPtr(): Unit = {}

  protected def prepareRecordArrayGet(): Unit = {}

  protected def insertCallArgStores(): Unit = {
    for (call <- all[Call]; edge <- call.groupedInEdges) {
      val callArgEdge = edge.target match {
        case `call` => edge
        case x =>
          assert(x.singleUse == call)
          ScalaCollections.singleElement(x.outEdges)
      }
      val abi = call.abi
      val idx = call.invokeArgIdx(callArgEdge)
      if (idx >= 0) {
        abi.paramLocations(idx) match {
          case loc: TailSlot =>
            val offs = loc.offset + abi.stackParamsStartOffset + // TODO-NEW-ABI: eliminate uses of `stackParamsStartOffset`
              // To avoid our code segment spoiling, we put stack params above it
              // and repush them before call (CodeGeneratorAmd64.beforeCallActions).
              (if (abi.spoilsCallerFrameDescriptor(rootMethod.getMethodType)) Env.stackSlotSize else 0)

            // Create slot even if it won't be written with Void value so that all tail slots would have proper offsets.
            val slot = slotForArg(edge.source.tpe, offs)
            if (!edge.source.isInstanceOf[Void]) {
              edge.source = CallArgStore(call.inCtrl, edge.source, slot)
            }
          case _: AnyReg =>
          case _: AltLocation =>
        }
      }
    }
  }

  protected def replaceNonLeaEEUses(): Unit = {
    for (ee <- all[ExecEnv]) {
      val (_, nonLeaUses) = ee.valueOutEdges.partition(_.target.isInstanceOf[Lea])
      if (nonLeaUses.nonEmpty) {
        val ee0 = Lea.Base.apply0(ee, 0)
        for (edge <- nonLeaUses.toList) {
          edge.source = ee0
        }
      }
    }
  }

  protected val unmovableNodes = Maps[ControlNode].newQMap[Sets[Node]#QSet]

  def processLocalUnmovable(): Unit = {
    assert(unmovableNodes.isEmpty)
    if (all[BeginLocalUnmovable].nonEmpty) {
      val analysis = analyzeLocalUnmovable()
      for (node <- all[ControlNode] if couldGatherLocalUnmovableAt(node)) {
        val unmovable = analysis.out(node).values
        unmovableNodes(node) = Sets[Node].newQSet(unmovable.toSeq.sortBy(_.id))
      }
    }
  }

  def localUnmovableAt(node: ControlNode): Sets[Node]#QSet = unmovableNodes.getOrElse(node, Sets[Node].newQSet)

  def copyLocalUnmovable(from: ControlNode, to: ControlNode) = {
    if (couldGatherLocalUnmovableAt(to) && unmovableNodes.contains(from)) {
      unmovableNodes(to) = unmovableNodes(from)
    }
  }
}
