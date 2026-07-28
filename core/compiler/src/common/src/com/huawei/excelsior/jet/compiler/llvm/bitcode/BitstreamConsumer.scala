/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.llvm.bitcode

trait BitstreamConsumer {
  def setContext(ctx: Bitstream.Context): Unit

  // true if parse bitstream
  def magic(magicValue: Int): Boolean

  // true if parse block
  def enterBlock(id: Int): Boolean

  def endBlock(id: Int): Unit

  def record(code: Int, operandsCount: Int, hasBlob: Boolean): Unit

  def endOfStream(): Unit = {}

  def blockInfoAllowed = true
}