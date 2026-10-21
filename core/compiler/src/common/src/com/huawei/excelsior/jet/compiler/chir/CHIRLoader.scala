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
import com.huawei.excelsior.jet.compiler.options.NumOption.CHIRVersion

import scala.collection.mutable
import scala.ref.SoftReference

object CHIRLoader {
  private val parsedCHIR = mutable.HashMap.empty[String, scala.ref.Reference[CHIRResolver]]
}

class CHIRLoader(source: String) {
  protected def packageGen(env: Environment): CHIR.Package = CHIR.newPackage(source, env.valueOf(CHIRVersion))
  def resolver(env: Environment): CHIRResolver = {
    CHIRLoader.parsedCHIR.get(source).flatMap(_.get).getOrElse {
      val pkg: CHIR.Package = packageGen(env)
      val resolver = CHIRResolver(pkg, env)
      CHIRLoader.parsedCHIR.put(source, new SoftReference(resolver))
      resolver
    }
  }

  def load(builder: CHIRSymLevelBuilder): Unit = builder.env.stage(Stage.CangjieModuleParsing) {
    val chirResolver = resolver(builder.env)
    CHIRBuilder.parse(builder, chirResolver)
  }
}

class IntrinsicsCHIRLoader extends CHIRLoader(CHIRPatchGeneratorFactory.intrinsicsSourceName) {
  override protected def packageGen(_env: Environment): CHIR.Package = CHIR.intrinsicsPackage
}