/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.ir

/** Describes consecutive block of code in interval `[startOffset, endOffset)` identified by [[id]].
  * Several blocks might have same id, in which case they form a disjoint marked region.
  *
  * @author ijorch
  */
case class MarkedRegion(id: Int, startOffset: Int, endOffset: Int)