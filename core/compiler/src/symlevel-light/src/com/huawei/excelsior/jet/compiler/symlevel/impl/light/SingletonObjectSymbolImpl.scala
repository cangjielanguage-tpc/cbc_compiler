/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.jet.compiler.symlevel.SingletonObjectSymbol
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcOModule}

class SingletonObjectSymbolImpl(tpe: TypeImpl) extends SymbolImpl(tpe.o2object.singletonObject) with SingletonObjectSymbol
