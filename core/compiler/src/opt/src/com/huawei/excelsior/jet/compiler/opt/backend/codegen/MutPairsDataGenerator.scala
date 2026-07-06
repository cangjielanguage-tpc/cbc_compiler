/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.codegen

import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.Location.AnyReg
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.{FrameSlot, Immediate, Resource}
import com.huawei.excelsior.jet.compiler.opt.ir.{Resources, Universe}
import com.huawei.excelsior.jet.compiler.abi.ABI.{TailSlot}

trait MutPairsDataGenerator { self: Universe with BackEnd with CodeGenerator =>

  /** Returns true iff `node` resource contains base or derived pointer. */
  def hasMutValueProducer(n: Node): Boolean = (valueOf(n).producer match {
    case p: GetFieldSeqRef => p.tpe.isTraceableRefType
    case p: Param => p.num match {
      case _ if !rootMethod.isCangjieMut => false // no mut parameters in non-mut-function
      case _ if p.num == rootMethod.getMutRecordArgIdx => true
      case _ if p.num == rootMethod.getMutObjectArgIdx => true
      case _ => false
    }
    case _ => false
  }) && !n.resource.isInstanceOf[TailSlot]

  def needMutInfo(n: Node): Boolean = needGCMap(n)

  /** Calculates map from every point in IR which [[needMutInfo]] to set of nodes which [[hasMutValueProducer]] and live at this point. */
  private def calcMutValues(): collection.Map[Node, Set[Node]] = {
    val engine = new NodeLivenessEngine {
      override protected def valuesFilter(n: Node): Boolean = hasMutValueProducer(n)

      override protected def processBlock(block: Block, output: Set[Node], updateLive: (Node, Set[Node]) => Unit): Set[Node] = {
        var curr = output
        for (node <- CodeOrder reversedIn block if !node.isInstanceOf[Phi]) {
          assert(node.isGroupRoot)
          curr &~= node.groupedValueResults.toSet
          node match {
            case sn: SpinalNode if sn.hasXHandler => curr ++= getLive(sn.xHandler)
            case _ =>
          }

          node match {
            case c: Call => {
              curr |= c.invokeArgs.filter(arg => arg.mayHaveResource && (arg.resource match {
                case fs: FrameSlot => hasMutValueProducer(arg)
                case Immediate => false
                case _: AnyReg =>
                  // parameters passed on registers should be tracked by callee
                  false
                // no other parameters locations are expected, MatchError is intended
              })).toSet
            }
            case _ =>
          }

          if (needMutInfo(node)) {
            updateLive(node, curr)
          }

          curr |= (node.groupedValueArgs filter valuesFilter).toSet
        }
        curr
      }
    }

    engine.calcLiveness()
    engine.live filter (x => needMutInfo(x._1))
  }

  /** Calculates map from every point in IR which [[needMutInfo]] to set of mut pairs at this point. */
  protected def calcMutPairs(): collection.Map[Node, Set[(Node, Node)]] = {
    val valuesMap = calcMutValues()

    // form pairs from collected values
    valuesMap.map((keyNode, valueNodes) =>
      val pairs = Set.newBuilder[(Node, Node)]
      var baseParam: Node = null
      var derivedParams: List[Node] = List.empty

      for (value <- valueNodes) {
        valueOf(value).producer match {
          case GetFieldSeqRef(_, base) => pairs += base -> value
          case p: Param => 
            if (p.num == rootMethod.getMutRecordArgIdx)
              derivedParams ::= value
            else if (p.num == rootMethod.getMutObjectArgIdx && baseParam == null)
              baseParam = value
        }
      }

      if (rootMethod.isCangjieMut) {
        if (baseParam == null) {
          // if there is no alive base, there cannot be derived pointers
          assert(derivedParams.isEmpty)
        } else {
          for (derived <- derivedParams) {
            pairs += baseParam -> derived
          }
        }
      } else {
        // Mut pairs can only be created by GetFieldSeqRef
      }

      keyNode -> pairs.result()
    )
  }

}
