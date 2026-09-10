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

  def load(builder: CHIRSymLevelBuilder, source: String): Unit = builder.env.stage(Stage.CangjieModuleParsing) {
    val resolver = getCHIRResolver(source)(builder.env)
    CHIRBuilder.parse(builder, resolver)
  }

  private val parsedCHIR = mutable.HashMap.empty[String, scala.ref.Reference[CHIRResolver]]

  def getCHIRResolver(source: String)(implicit env: Environment): CHIRResolver = {
    parsedCHIR.get(source).flatMap(_.get).getOrElse {
      val pkg: CHIR.Package = CHIR.newPackage(source, env.valueOf(CHIRVersion))
      val cjEntryId = pkg.values.length
      implicit val delegate: CHIR.Package = new CHIR.Package {
        private lazy val cjEntry = pkg.getFunc("user.main").map(CHIRCjEntryGenerator(pkg, cjEntryId, _).gen())

        def name: String = pkg.name
        def typeDefs: Iterator[CHIR.CustomTypeDef] = pkg.typeDefs
        def values: Iterator[CHIR.Value] = pkg.values ++ cjEntry.map(Iterator.single).getOrElse(Iterator.empty)
        def function(idx: Int): CHIR.Func = if idx != cjEntryId then pkg.function(idx) else cjEntry.get
        def packageInitFunc: CHIR.Func = pkg.packageInitFunc
        def packageInitLiteralFunc: CHIR.Func = pkg.packageInitLiteralFunc
        def getCustomType(identifier: String): Option[CHIR.CustomType] = pkg.getCustomType(identifier)
        def getFunc(identifier: String): Option[CHIR.Func] = if identifier != CHIRCjEntryGenerator.name then pkg.getFunc(identifier) else cjEntry
      }

      val resolver = CHIRResolver()
      parsedCHIR.put(source, new SoftReference(resolver))
      resolver
    }
  }
}
