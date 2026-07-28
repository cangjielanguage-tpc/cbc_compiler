/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.adler32

import xscala.vm.VMDependent

object Adler32VMDependent extends VMDependent[Adler32VMDependent] {
  override def get: Adler32VMDependent = {
    val instance = super.get
    instance.init()
    instance
  }
}

protected trait Adler32VMDependent {
  private[adler32] def init(): Unit
  def adler32Update(adler: Int, bytes: Array[Byte], len: Int): Int
}
