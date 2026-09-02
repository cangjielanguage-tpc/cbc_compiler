/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.jet.compiler.{Environment, Stage}
import com.huawei.excelsior.jet.compiler.cangjie.CHIRSymLevelBuilder
import com.huawei.excelsior.jet.compiler.symlevel.Type

import scala.collection.mutable
import scala.ref.SoftReference

object CHIRLoader {

  def load(builder: CHIRSymLevelBuilder, source: String): Unit = builder.env.stage(Stage.CangjieModuleParsing) {
    val resolver = getCHIRResolver(source)(builder.env)
    CHIRBuilder.parse(builder, resolver)
  }

  private val parsedCHIR = mutable.HashMap.empty[String, scala.ref.Reference[CHIRResolver]]

  def getCHIRResolver(source: String)(implicit env: Environment): CHIRResolver = {
    parsedCHIR.get(source).flatMap(_.get).getOrElse {
      // TODO pass fbs version
      implicit val pkg: CHIR.Package = CHIR.newPackage(source)
      val resolver = CHIRResolver()
      parsedCHIR.put(source, new SoftReference(resolver))
      resolver
    }
  }
}
