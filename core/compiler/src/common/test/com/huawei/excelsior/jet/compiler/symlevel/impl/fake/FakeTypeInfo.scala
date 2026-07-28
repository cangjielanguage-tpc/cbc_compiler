/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.symlevel.impl.fake

import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis

class FakeTypeInfo(name: String) extends FakeSymbol(name) with TypeHandleSymbol 
  with InstanceDescriptorSymbol {
  override def tpe: Type = shouldNotCallThis()
}