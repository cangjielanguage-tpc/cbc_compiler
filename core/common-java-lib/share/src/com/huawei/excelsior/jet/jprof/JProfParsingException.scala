/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.jprof

/** JProf parsing exception.
  *
  * @author xappymah
  */
class JProfParsingException(message: String, fileName: String = "<unknown jprof file>")
  extends RuntimeException(s"$message (file '$fileName')")