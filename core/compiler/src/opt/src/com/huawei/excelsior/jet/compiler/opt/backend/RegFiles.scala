/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

trait RegFiles { self: Universe with BackEnd =>

  /** Register files enumeration. */
  enum RegFile:
    case IREG, FREG

  import RegFile.*

  val allRegFiles: Seq[RegFile] = Seq(IREG, FREG)

  /** Returns true, iff `node` value result is not allocated in any of existing register files. */
  def inSpecialFile(node: Node): Boolean = node match {
    case _: ExecEnv | _: CallArgStore => true

    // Void corresponds to Cangjie unit value, it can be used as parameter in call, meaning it should be
    // taken into account during register allocation.
    case _: Void => false

    case _ => false
  }

  /** Returns register file of `node` result, or null, iff `node` does not have value result.  */
  def regFileOf(node: Node): RegFile = {
    assert(node.producesValue)
    if (inSpecialFile(node)) return null
    if (node.isFP) FREG else IREG
  }

  /** Returns register file of `resource`.  */
  def regFileOf(resource: Resource): RegFile = resource match {
    case _ if resource.isIReg => IREG
    case _ if resource.isFReg => FREG
    case _ => shouldNotReachHere()
  }

  /** Returns resources set of `file`. */
  def regSetOf(file: RegFile): ResourceSet = file match {
    case IREG => allIRegsSet
    case FREG => allFRegsSet
  }

  /** Returns limit size of `file`. */
  def limitOfFile(file: RegFile): Int = regSetOf(file).size
}
