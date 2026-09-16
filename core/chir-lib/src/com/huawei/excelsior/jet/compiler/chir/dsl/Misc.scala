package com.huawei.excelsior.jet.compiler.chir.dsl

import com.huawei.excelsior.jet.compiler.chir.CHIR

trait HasResultVar extends CHIR.HasResultVar {
  var resultTpe: CHIR.Type = _
  var resultVar: CHIR.LocalVar = _
}
