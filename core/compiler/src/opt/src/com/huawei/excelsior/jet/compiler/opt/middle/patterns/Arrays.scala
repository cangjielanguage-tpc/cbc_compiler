/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.patterns

import com.huawei.excelsior.jet.compiler.{Stats, StatsKind}
import com.huawei.excelsior.jet.compiler.symlevel.MethodReferenceAccessKind.STATIC
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.ArrayFillAggregation
import com.huawei.excelsior.jet.compiler.symlevel.{Method, SignatureType, Type as SymType}

import scala.PartialFunction.cond

trait Arrays { self: Universe =>

  object CopyOf {
    private def isCopyOf(m: Method) = m.getName == "copyOf" && m.getParamsCount == 2
    private def isCopyOfRange(m: Method) = m.getName == "copyOfRange" && m.getParamsCount == 3

    def unapply(call: Call) : Option[(Node, Node, Node, SymType, Boolean)] = call match {
      case CallMethod(method, STATIC, args) if method.getDeclaringClass.getName == "java/util/Arrays" =>
        val (src, from, to, isRange) = args match {
          case Seq(srcArr, length) if isCopyOf(method) => (srcArr, IConst(0), length, false)
          case Seq(srcArr, fromIdx, toIdx) if isCopyOfRange(method) => (srcArr, fromIdx, toIdx, true)
          case _ => return None
        }

        stats.count(StatsKind.NewArrayCopy, s"copyOf/copyOfRange replaced", call)
        Some((src, from, to, method.getParamType(0).symType, isRange))

      case _ =>
        None
    }
  }

  def tryReplaceArrayCopyOf(call: Call): Boolean = {
    cond(call) {
      case CopyOf(src, from, to, allocType, isRange) =>
        replaceByCode(call) {
          NullCheck(src)
          NewArrayCopyRT(allocType, isRange)(src, from, to)
        }

        true
    }
  }

  def optimizeArraysCopyOf(): Boolean = {
    var changed = false
    for (x <- all[Call]) {
      changed |= tryReplaceArrayCopyOf(x)
    }
    changed
  }

  def collectArrayAggregate(arrayType: SignatureType, arrObj: Node, idx: Node, value: Node): Boolean = {
    if (!env.enabled(ArrayFillAggregation)) {
      return false
    }

    (idx, value) match {
      case (IntegralConst(index), IntegralConst(v)) if currentCtrl.uses.isEmpty =>

        def collectArrayFill(prev: SpinalNode, values: Seq[Long]) = {
          strikeOut(prev)
          ArrayFill(arrayType, values)(arrObj)
        }

        currentCtrl match {
          case arrayPut: ArrayPut if (arrayPut.array == arrObj) && (index == 1) =>
            (arrayPut.idx, arrayPut.storedValue()) match {
              case (IntegralConst(0), IntegralConst(v0)) =>
                collectArrayFill(arrayPut, Seq(v0, v))
                true

              case _ => false
            }

          case arrayFill: ArrayFill if (arrayFill.array == arrObj) && (arrayFill.size == index) =>
            assert(arrayFill.elemType == arrayType.getArrayElemType.toAsm)
            assert(currentMemory == arrayFill)
            // Note: here inValues0 is used instead of storedValues to avoid re-adjustment of already adjusted values
            collectArrayFill(arrayFill, arrayFill.inValues0 :+ v)
            true

          case _ => false
        }

      case _ => false
    }
  }
}
