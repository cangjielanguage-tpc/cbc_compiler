/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.adler32

import xscala.adler32.Adler32VMDependent

class Adler32 {
  var adler = 1
  def getValue: Int = adler
  def update(buf: Array[Byte], len: Int): Unit = {
    if (buf == null) {
      throw new NullPointerException
    }
    if (len < 0 || buf.length < len) {
      throw new ArrayIndexOutOfBoundsException
    }
    adler = Adler32VMDependent.get.adler32Update(adler, buf, len)
  }
}
