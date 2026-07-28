/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.lowering.cbc

import com.huawei.excelsior.jet.compiler.opt.lowering.PreLowering
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ClassType
import com.huawei.excelsior.jet.compiler.types.References.UpperBounded

import scala.PartialFunction.condOpt

trait PreLoweringCBC extends PreLowering {

  override def optimizeEnriches(): Unit = if (useEnrichedPointers) {
    for (enrich <- all[Enrich]) {
      enrich.enrichment match {
        case _: (InterfaceCastCBC | WeakCast) =>
          condOpt(nodeType(enrich.obj)) {
            case UpperBounded(rcvType: ClassType, _) if rcvType implements SignatureType.fromSymType(enrich.interfaceType) =>
              val enrichCBC = EnrichCBC(rcvType.symType, enrich.interfaceType)(enrich.obj)
              enrich.replaceBy(enrichCBC)
          }
        case _ =>
      }
    }
  }
}
