/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cbc

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.symlevel.{Method, SignatureType}

import scala.collection.mutable

class OffHeapMemoryMultisetBuilder(private val gen: LegacyCBCFileGenerator) {
  import gen.sigIndex
  import gen.sigIndex.ftc

  def build(m: Method, universalVariables: Seq[SignatureType]): Seq[Int] = {
    assert(m.hasUniversalGenericContext)

    // TODO prevent duplicates with sigIndex.ftc
    val ftcString = if (m.isUniversalGeneric) {
      ftc.ftcString(m)
    } else {
      ftc.ftcStringRef(m).map(_.ftcString)
    }

    val universalVariableTypesInPool = ftcString.map(_.getSymbolsInSegment(ftc.SegmentKind.VST)).getOrElse(Seq.empty)

    /** The order of signatures of universal variables (VSTs) indexed in FTC pool may differ from the order in frame stack slots.
      * The final order of segments' sizes in OHM Multiset is based on the order of corresponding VSTs in FTC pool.
      * By filling the segments' sizes we check that every signature of universal variable is present in FTC pool.
      */
    val res = mutable.LinkedHashMap.from(universalVariableTypesInPool.map(s => (s, 0)))

    for (universalVar <- universalVariables) {
      res.updateWith(universalVar.asInstanceOf[ftc.FTCSymbol]) {
        case Some(count) => Some(count + 1)
        case None => shouldNotReachHere(s"universal variable type $universalVar must be of VST type and located in FTC string")
      }
    }

    res.values.toSeq
  }
}
