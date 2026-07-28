/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.indy

import com.huawei.excelsior.jet.common.XString

/** Determines CHIR entity location by source file name and id of that entity in the file. */
case class CHIRDef(source: XString, id: Int)
