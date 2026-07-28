/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.starter

import com.huawei.excelsior.common.LanguagePack
import com.huawei.excelsior.jet.compiler.Env.languagePack

object LanguagePackConfig {
  def init(): Unit = {
    assert(languagePack == LanguagePack.JAVA)
  }
}
