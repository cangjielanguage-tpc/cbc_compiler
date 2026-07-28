/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.dotty.plugins

import com.huawei.excelsior.dotty.plugins.phases.*
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}

class JavaFriendlyEnums extends StandardPlugin {
  val name: String = "Java-friendly enums"
  override val description: String = "Allows to use Scala enums in Java switch statements"

  def init(options: List[String]): List[PluginPhase] =
    CompleteAnnotatedEnums() :: ReintroduceStaticEnumFields() :: Nil
}
