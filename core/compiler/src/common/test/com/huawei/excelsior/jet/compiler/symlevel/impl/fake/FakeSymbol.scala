/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.symlevel.impl.fake

import com.huawei.excelsior.jet.codeemitter.SymbolInfo.AccessKind

/** Symbol with access kind which is used through [[FakeEnvironment.accessKind]].
  *
  * @author cypok
  */
class FakeSymbol(name: String = "Symbol", var accessKind: AccessKind = AccessKind.DIRECT)
  extends com.huawei.excelsior.jet.assembler.FakeSymbol(name)