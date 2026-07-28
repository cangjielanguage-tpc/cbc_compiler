/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.util.Maps
import com.huawei.excelsior.jet.util.{Closure, Worklist}

trait PhiWebsTranslation { self: Universe =>

  /** Transforms phies with `phiType` into new phies with translated arguments and results. Each phi web will be
    * replaced by several webs (number of which depends on nature of real arguments of phi-web).
    * <p>
    * This transformation is done in following steps:
    *
    * 1. For each phi create several proxies (for each new web)
    *
    * 2. For each phi create several phies with updated arguments:
    *    - instead of phi arg use its proxy;
    *    - otherwise instead of arg use `argsTranslator(arg)`;
    *
    * 3. For each phi:
    *    - replace it by `resultBuilder(phies)`;
    *    - replace its proxies by associated phies.
    */
  private abstract class PhiWebsTranslator {
    def phiType: Type
    def message: String

    def numberOfWebs(node: Node): Int
    def webTypes(index: Int): Type
    def argsTranslator(node: Node, index: Int): Node
    def resultBuilder(phies: Seq[Node]): Node

    def translate(): Boolean = {
      val phies = all[Phi].filter(_.tpe == phiType)
      if (phies.isEmpty) return false

      // Reverse is heuristic for better iteration (phies with bigger ids tend to be "lower" in phi web)
      val ws = Worklist.empty[Phi]
      phies foreach ws.prepend

      for (keyPhi <- ws.drain) {
        // 1. Collect all phies and other arguments used in keyPhi web
        val args = Closure[Node](keyPhi) {
          case phi: Phi => phi.valueArgs
          case _ => Iterator.empty
        }

        val (originalWeb, other) = args partitionMap {
          case x: Phi => Left(x)
          case x => Right(x)
        }
        ws.removeAll(originalWeb)
        assert(other.nonEmpty)
        val sample = other.head

        val websNumber = numberOfWebs(sample)
        val proxies = Seq.fill(websNumber)(Maps[Phi].newMMap[Proxy])
        val webs = Seq.fill(websNumber)(Maps[Phi].newQMap[Node])
        val types = Seq.tabulate(websNumber)(webTypes)

        // 2. Create several proxies (for each new web)
        for (phi <- originalWeb; i <- 0 until websNumber) {
          proxies(i)(phi) = Proxy(types(i))(phi.block)
        }

        // 3. Create new webs
        for (phi <- originalWeb; i <- 0 until websNumber) {
          val updatedArgs = phi.argsSeq map {
            case phi: Phi => proxies(i)(phi)
            case arg => argsTranslator(arg, i)
          }
          webs(i)(phi) = Phi(types(i))(phi.block +: updatedArgs: _*)
        }

        // 4. Bulk replace
        bulkReplace {
          for (phi <- originalWeb) {
            val proxiesRow = proxies.map(_.apply(phi))
            val phiesRow = webs.map(_.apply(phi))
            val result = resultBuilder(phiesRow)
            replaceTransitively(phi, result)
            for ((proxy, newPhi) <- proxiesRow zip phiesRow) {
              replaceTransitively(proxy, newPhi)
            }
          }
        }
      }

      dbgPrinter.debugNodes(message)

      true
    }
  }

  def eliminateConditionPhies(): Boolean = {
    new PhiWebsTranslator {
      override def phiType = ConditionType
      override def message = "All graph after condition phies eliminated"

      override def numberOfWebs(node: Node) = 1
      override def webTypes(index: Int) = IntType
      override def argsTranslator(node: Node, index: Int) = CondVal(node)

      override def resultBuilder(phies: Seq[Node]) = phies match {
        case Seq(phi) => Cmp(IntType, Condition.NE)(phi, IConst(0))
      }

    }.translate()
  }

  def eliminateIntraReferencePhies(): Boolean = {
    new PhiWebsTranslator {
      override def phiType = IntraReferenceType
      override def message = "All graph after intra references phies eliminated"

      override def numberOfWebs(node: Node) = 2

      override def webTypes(index: Int) = index match {
        case 0 => TRefType
        case 1 => AddrType
      }

      override def argsTranslator(node: Node, index: Int) = (node, index) match {
        case (Lea.AnyWithBase(base, _), 0) => base
        case (lea: Lea, 1) => lea.withoutBase(AddrType)
        case _ => shouldNotReachHere()
      }

      override def resultBuilder(phies: Seq[Node]) = phies match {
        case Seq(base: Node, offset: Node) => Lea.Scaled(base, offset, 1)
      }

    }.translate()
  }

}
