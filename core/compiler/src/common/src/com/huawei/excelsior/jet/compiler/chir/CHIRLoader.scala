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
  private val resolvers = mutable.HashMap.empty[String, scala.ref.Reference[CHIRResolver]]
}

/** The cbc engine helper package chir (cbcengine-helper.chir). It is always appended
  * to the compilation scope. */
object EngineHelperChir {
  val sourceName: String = "cbcengine-helper.chir"

  /** Explicit CBCENGINE_HELPER_CHIR override wins; otherwise the helper chir
    * is looked up next to cbc-intrinsics.cbc in the toolchain. */
  def resolveFile(): Option[java.io.File] = {
    val path = Option(System.getenv("CBCENGINE_HELPER_CHIR"))
      .orElse(Option(System.getenv("CANGJIE_HOME")).map(home => s"$home/tools/lib/$sourceName"))
    path.map(new java.io.File(_))
  }
}

class CHIRLoader(source: String) {
  protected def genPackage(env: Environment): CHIR.Package = CHIR.newPackage(source, env.valueOf(CHIRVersion))

  def getResolver(env: Environment): CHIRResolver = {
    CHIRLoader.resolvers.get(source).flatMap(_.get).getOrElse {
      val pkg: CHIR.Package = genPackage(env)
      val resolver = CHIRResolver(pkg, env)
      CHIRLoader.resolvers.put(source, new SoftReference(resolver))
      resolver
    }
  }

  def load(builder: CHIRSymLevelBuilder): Unit = builder.env.stage(Stage.CangjieModuleParsing) {
    val chirResolver = getResolver(builder.env)
    CHIRBuilder.parse(builder, chirResolver)
  }
}

class IntrinsicsCHIRLoader extends CHIRLoader(IntrinsicsPackage.intrinsicsSourceName) {
  override protected def genPackage(_env: Environment): CHIR.Package = CHIR.newIntrinsicsPackage
}