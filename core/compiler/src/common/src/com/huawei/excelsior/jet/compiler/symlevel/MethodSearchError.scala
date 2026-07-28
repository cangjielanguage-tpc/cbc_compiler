/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.common.Language.JAVA
import com.huawei.excelsior.jet.compiler.Env.languagePack
import com.huawei.excelsior.jet.compiler.RTSProc

enum MethodSearchError:
  case ABSTRACT_METHOD
  case ILLEGAL_ACCESS
  case INCOMPATIBLE_CLASS_CHANGE

  def rtsProc: RTSProc = {
    if (languagePack.supports(JAVA)) {
      import com.huawei.excelsior.jet.compiler.symlevel.MethodSearchError.*
      this match {
        case ABSTRACT_METHOD => RTSProc.JR_ThrowAbstractMethodError0
        case ILLEGAL_ACCESS => RTSProc.JR_ThrowIllegalAccessError0
        case INCOMPATIBLE_CLASS_CHANGE => RTSProc.JR_ThrowIncompatibleClassChangeError0
      }
    } else {
      RTSProc.VMT_fatalErrorStub
    }
  }

object MethodSearchError:
  val COUNT = values.size
