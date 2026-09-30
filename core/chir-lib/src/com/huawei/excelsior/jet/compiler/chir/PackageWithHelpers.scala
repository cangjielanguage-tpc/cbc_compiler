package com.huawei.excelsior.jet.compiler.chir

import scala.collection.mutable

object PackageWithHelpers {
  def apply(pkg: CHIR.Package): PackageWithHelpers = {
    val helperGenerators = CHIRHelperGenerator(pkg).generateHelpers

    val helpersById: mutable.HashMap[Int, CHIR.Func] = mutable.HashMap()
    val helpersByName: mutable.HashMap[String, CHIR.Func] = mutable.HashMap()

    val startingId: Int = pkg.values.length

    val ids = startingId until (startingId + helperGenerators.length)
    val helpers = helperGenerators.zip(ids).map((helperGen, id) => helperGen(id))

    for (helper <- helpers) {
      helpersById(helper.id.toInt) = helper
      helpersByName(helper.name) = helper
    }

    new PackageWithHelpers(pkg, helpers, helpersById, helpersByName)
  }
}

class PackageWithHelpers(pkg: CHIR.Package, helpers: Seq[CHIR.Func], helpersById: mutable.HashMap[Int, CHIR.Func], helpersByName: mutable.HashMap[String, CHIR.Func]) extends CHIR.Package {
  def values: Iterator[CHIR.Value] = pkg.values ++ helpers.iterator

  def function(idx: Int): CHIR.Func = if (helpersById.contains(idx)) {
    helpersById(idx)
  } else {
    pkg.function(idx)
  }

  def getFunc(identifier: String): Option[CHIR.Func] = if (helpersByName.contains(identifier)) {
    Option(helpersByName(identifier))
  } else {
    pkg.getFunc(identifier)
  }

  // Forwarders
  def getDef(identifier: String): Option[CHIR.CustomTypeDef] = pkg.getDef(identifier)
  def name: String = pkg.name
  def packageInitFunc: CHIR.Func = pkg.packageInitFunc
  def packageInitLiteralFunc: CHIR.Func = pkg.packageInitLiteralFunc
  def typeDefs: Iterator[CHIR.CustomTypeDef] = pkg.typeDefs
}
