/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.Symbol

import java.util.Arrays
import scala.util.hashing.MurmurHash3

class RawData(val data: Array[Byte], val alignment: Int) extends Symbol {
  override def equals(obj: Any) = obj match {
    case ref: AnyRef if this eq ref => true
    case that: RawData => Arrays.equals(this.data, that.data) && this.alignment == that.alignment
    case _ => false
  }

  override def hashCode() = (MurmurHash3.bytesHash(data), alignment).##
}
