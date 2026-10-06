package com.huawei.excelsior.jet.compiler.chir

import scala.collection.mutable

object PatchedPackage {
  def apply(pkg: CHIR.Package): PatchedPackage = {
    val helperGenerators = CHIRPatchGeneratorFactory(pkg).generateHelpers

    val helpersById: mutable.HashMap[Int, CHIR.Func] = mutable.HashMap()
    val helpersByName: mutable.HashMap[String, CHIR.Func] = mutable.HashMap()

    val startingId: Int = pkg.values.length

    val ids = startingId until (startingId + helperGenerators.length)
    val helpers = helperGenerators.zip(ids).map((helperGen, id) => helperGen.generatePatch(id))

    new PatchedPackage(pkg, helpers)
  }
}

class PatchedPackage(pkg: CHIR.Package, helpers: Seq[CHIR.Func]) extends CHIR.Package {
  private val helpersById = helpers.map(h => (h.id.toInt, h)).toMap
  private val helpersByName = helpers.map(h => (h.name, h)).toMap

  def values: Iterator[CHIR.Value] = pkg.values ++ helpers.iterator

  def function(idx: Int): CHIR.Func = helpersById.getOrElse(idx, pkg.function(idx))

  def getFunc(identifier: String): Option[CHIR.Func] = helpersByName.get(identifier).orElse(pkg.getFunc(identifier))

  // Forwarders
  def getDef(identifier: String): Option[CHIR.CustomTypeDef] = pkg.getDef(identifier)
  def name: String = pkg.name
  def packageInitFunc: CHIR.Func = pkg.packageInitFunc
  def packageInitLiteralFunc: CHIR.Func = pkg.packageInitLiteralFunc
  def typeDefs: Iterator[CHIR.CustomTypeDef] = pkg.typeDefs
}
