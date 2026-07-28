/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.Location.*
import com.huawei.excelsior.jet.assembler.Width

trait Register extends AnyReg {
  def idx: Int
}

object Register {
  enum IR extends Register with IReg {
    case IRZ, IR1, IR2, IR3, IR4, IR5, IR6, IR7,
         IR8, IR9, IR10, IR11, IR12, IR13

    override def width = Width.W64
    def nonVolIdx: Int = (ordinal - IR.firstNonVol.ordinal).ensuring(_ >= 0)
    def idx = ordinal
  }

  object IR {
    private val firstNonVol = IR8
    val count = IR.values.length
  }

  enum FR extends Register with FReg {
    case FR0, FR1, FR2, FR3, FR4, FR5, FR6, FR7,
         FR8, FR9, FR10, FR11, FR12, FR13, FR14, FR15

    override def width = Width.W64
    def nonVolIdx: Int = (ordinal - FR.firstNonVol.ordinal).ensuring(_ >= 0)
    def idx = ordinal
  }

  object FR {
    private val firstNonVol = FR8
    val count = FR.values.length
  }

}
