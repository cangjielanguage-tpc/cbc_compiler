/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import java.io.IOException

trait InputStream extends AutoCloseable {
  def read(): Int
  def read(buffer: Array[Byte]): Int = read(buffer, 0, buffer.length)
  def read(buffer: Array[Byte], offset: Int, length: Int): Int = {
    if ((offset | length | (offset + length) | (buffer.length - (offset + length))) < 0) {
      throw new IndexOutOfBoundsException()
    } else if (length == 0) {
      return 0
    }

    var i = 0
    try {
      var b: Int = 0
      while (i < length && { b = read(); b >= 0}) {
        buffer(offset + i) = b.toByte
        i += 1
      }
    } catch {
      case e: IOException =>
        if (i > 0) {
          return i
        }
        throw e
    }

    if (i > 0) i else -1
  }

  def skip(n: Int): Int = {
    var i = 0
    while (i < n && read() != -1) {
      i += 1
    }
    i
  }

  def available(): Int
}

trait OutputStream extends AutoCloseable {
  def write(b: Int): Unit
  def write(buffer: Array[Byte]): Unit = write(buffer, 0, buffer.length)
  def write(buffer: Array[Byte], offset: Int, length: Int): Unit = {
    if ((offset | length | (offset + length) | (buffer.length - (offset + length))) < 0) {
      throw new IndexOutOfBoundsException()
    } else if (length == 0) {
      return
    }

    var i = 0
    while (i < length) {
      write(buffer(i))
      i += 1
    }
  }

  def flush(): Unit = {}
}
