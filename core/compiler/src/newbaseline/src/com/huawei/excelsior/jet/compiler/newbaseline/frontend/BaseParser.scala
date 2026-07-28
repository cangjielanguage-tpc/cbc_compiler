/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.frontend

import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.bytecode.Slots
import com.huawei.excelsior.jet.compiler.bytecode.parsing.BlockDataFlowParser
import com.huawei.excelsior.jet.compiler.symlevel.Method

import scala.collection.mutable.ArrayBuffer

/** Base data-flow parser for all baseline parsers.
  */
abstract class BaseParser[V](method: Method, block: Block, _slots: Slots)
  extends BlockDataFlowParser[V](_slots, block.stackHeightAtStart, false, null) {

  protected val cp = method.getDeclaringClass.getClassConstantPool

  protected implicit val typeProvider: TypeProvider = cp.getTypeProvider

  def iterateBytecode(): Unit = {
    iterateBytecode(method.codeAttribute, block.startBC, block.endBC)
  }
}
