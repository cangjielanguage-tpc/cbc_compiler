package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis

object AugmentedPackage {
  def apply(pkg: CHIR.Package): AugmentedPackage = {
    val funcGenerators = Seq(
      CHIRCJEntryGenerator(pkg)
    )

    val startingId: Int = pkg.values.length

    val ids = startingId until (startingId + funcGenerators.length)
    val generatedFuncs = funcGenerators.zip(ids).flatMap((generator, id) => generator.generateFunc(id))

    new AugmentedPackage(pkg, generatedFuncs)
  }
}

class AugmentedPackage(pkg: CHIR.Package, generatedFuncs: Seq[CHIR.Func]) extends CHIR.Package {
  def newGoto(destination: CHIR.Block, debugLoc: Option[CHIR.DebugLocation]): CHIR.Goto = shouldNotCallThis("This synthetic package does not support serialized edits")
  def write(output: java.nio.file.Path): Unit = shouldNotCallThis("This synthetic package does not support serialized edits")
  def expressions(function: CHIR.Func): Seq[CHIR.Expression] = shouldNotCallThis("This synthetic package does not support serialized edits")
  def isInternal(function: CHIR.Func): Boolean = shouldNotCallThis("This synthetic package does not support serialized edits")
  def insert(block: CHIR.Block, operation: CHIR.Operation, before: Option[CHIR.Expression]): CHIR.LocalVar = shouldNotCallThis("This synthetic package does not support serialized edits")
  def replace(expression: CHIR.Expression, operation: CHIR.Operation): Unit = shouldNotCallThis("This synthetic package does not support serialized edits")
  def setOptimized(): Unit = shouldNotCallThis("This synthetic package does not support serialized edits")

  private val generatedFuncsById = generatedFuncs.map(h => (h.id.toInt, h)).toMap
  private val generatedFuncsByName = generatedFuncs.map(h => (h.name, h)).toMap

  def values: Iterator[CHIR.Value] = pkg.values ++ generatedFuncs.iterator

  def function(idx: Int): CHIR.Func = generatedFuncsById.getOrElse(idx, pkg.function(idx))

  def getFunc(identifier: String): Option[CHIR.Func] = generatedFuncsByName.get(identifier).orElse(pkg.getFunc(identifier))

  // Forwarders
  def getDef(identifier: String): Option[CHIR.CustomTypeDef] = pkg.getDef(identifier)
  def name: String = pkg.name
  def packageInitFunc: CHIR.Func = pkg.packageInitFunc
  def packageInitLiteralFunc: CHIR.Func = pkg.packageInitLiteralFunc
  def typeDefs: Iterator[CHIR.CustomTypeDef] = pkg.typeDefs
}
