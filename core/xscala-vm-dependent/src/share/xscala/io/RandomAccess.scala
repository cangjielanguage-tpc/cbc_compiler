/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

object RandomAccess {
  def apply(path: Path, readOnly: Boolean): RandomAccess = {
    IOVMDependent.get.createRandomAccessFile(path.toString, readOnly)
  }
}


abstract class RandomAccess extends DataInput with DataOutput {
  var cursor: Long
  var size: Long

  override def available: Int = ((size - cursor) min Int.MaxValue).toInt

  override def skip(n: Int): Int = {
    if (n <= 0) return 0

    val pos = cursor
    val newpos = (cursor + n).min(size)
    cursor = newpos

    (newpos - pos).toInt
  }

  override def close(): Unit = {}
}
