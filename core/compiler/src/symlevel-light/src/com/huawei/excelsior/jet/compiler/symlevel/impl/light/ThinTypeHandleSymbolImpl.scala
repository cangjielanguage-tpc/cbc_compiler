/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.jet.compiler.symlevel.ThinTypeHandleSymbol

class ThinTypeHandleSymbolImpl(tpe: TypeImpl) extends TypeInfoSymbolImpl(tpe, tpe.asClass.thinTypeInfo.thinTypeHandle) with ThinTypeHandleSymbol
