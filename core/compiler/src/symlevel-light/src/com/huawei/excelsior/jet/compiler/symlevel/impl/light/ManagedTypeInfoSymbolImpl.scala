/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.jet.compiler.o2lib.fe.pc
import com.huawei.excelsior.jet.compiler.symlevel.ManagedTypeInfoSymbol

abstract class ManagedTypeInfoSymbolImpl(tpe: TypeImpl, obj: pc.Symbol) extends TypeInfoSymbolImpl(tpe, obj) with ManagedTypeInfoSymbol
