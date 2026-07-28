/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.indy

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, MethodType}

case class LambdaInfo(
  // Class where lambda is defined
  capturingClass: ClassType,

  // Single Abstract Method (SAM) reference
  samClass: ClassType,
  samMethodName: XString,
  samMethodType: MethodType,

  // Implementation MethodHandle
  impl: MethodHandle,

  // `samMethodType` with all generic arguments specialized for the instantiation site
  instantiatedMethodType: MethodType
)
