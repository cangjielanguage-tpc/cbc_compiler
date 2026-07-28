/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.fake

import com.huawei.excelsior.jet.compiler.RTConst
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.FakeRTConstResolver.MOCK_VALUE

import scala.collection.mutable

class FakeRTConstResolver extends RTConst.Resolver {

  private val valueCache = mutable.HashMap.empty[RTConst, Int]
  def setIntValue(const: RTConst, value: Int): Unit = valueCache.put(const, value)

  private val offsetCache = mutable.HashMap.empty[RTConst, Int]
  def setOffset(const: RTConst, value: Int): Unit = offsetCache.put(const, value)

  override def alignment(host: RTConst.Host) = MOCK_VALUE
  override def size(host: RTConst.Host) = MOCK_VALUE

  override def intValue(const: RTConst) = valueCache.getOrElse(const, MOCK_VALUE)
  override def longValue(const: RTConst) = MOCK_VALUE
  override def addrValue(const: RTConst) = MOCK_VALUE
  override def offset(const: RTConst) = offsetCache.getOrElse(const, MOCK_VALUE)
}

object FakeRTConstResolver {
  private val MOCK_VALUE = 37
}