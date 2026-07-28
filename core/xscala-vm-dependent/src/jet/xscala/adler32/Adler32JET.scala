/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.adler32

import xscala.adler32.Adler32JET.*

import scala.annotation.static

final class Adler32JET extends Adler32VMDependent {
  private[adler32] def init(): Unit = {}
  def adler32Update(adler: Int, bytes: Array[Byte], len: Int): Int = adler32Update0(adler, bytes, len)
}

private object Adler32JET {
  @native @static def adler32Update0(adler: Int, bytes: Array[Byte], len: Int): Int
}