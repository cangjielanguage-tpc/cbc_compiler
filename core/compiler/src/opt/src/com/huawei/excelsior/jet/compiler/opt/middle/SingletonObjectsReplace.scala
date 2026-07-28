/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import com.huawei.excelsior.jet.util.Worklist

trait SingletonObjectsReplace {
  self: Universe =>

  def replaceSingletonObjects(): Boolean = {
    val objectsToReplace = allNodes.collect {
      case n: New if !n.allocType.isInstanceOf[SignatureType.Box] && n.allocType.symType.isSingletonObject => n
    }.toSeq

    for (n <- objectsToReplace) {
      replaceByCode(n)(SingletonObject(n.allocType.symType)())
    }
    
    objectsToReplace.nonEmpty
  }
}
