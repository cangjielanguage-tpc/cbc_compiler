package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.common.CodeHelpers

object PatchedPackage {
  def apply(pkg: CHIR.Package): PatchedPackage = {
    new PatchedPackage(pkg, Seq(
      CHIRThrowPatchGenerator(pkg, ThrowPatch.symbolResolutionErrorPatch),
      CHIRThrowPatchGenerator(pkg, ThrowPatch.abstractMethodErrorPatch),
      CHIRCJEntryGenerator(pkg)
    ))
  }
}

class PatchedPackage(pkg: CHIR.Package, patchGenerators: Seq[CHIRPatchGenerator]) extends CHIR.Package {
  private val startingId: Int = pkg.values.length
  private val ids = startingId until (startingId + patchGenerators.length)
  private val patches = patchGenerators.zip(ids).flatMap((generator, id) => generator.generatePatch(id))

  private val patchesById = patches.map(h => (h.id.toInt, h)).toMap
  private val patchesByName = patches.map(h => (h.name, h)).toMap

  def values: Iterator[CHIR.Value] = pkg.values ++ patches.iterator

  def function(idx: Int): CHIR.Func = patchesById.getOrElse(idx, pkg.function(idx))

  def getFunc(identifier: String): Option[CHIR.Func] = patchesByName.get(identifier).orElse(pkg.getFunc(identifier))

  // Forwarders
  def getDef(identifier: String): Option[CHIR.CustomTypeDef] = pkg.getDef(identifier)
  def name: String = pkg.name
  def packageInitFunc: CHIR.Func = pkg.packageInitFunc
  def packageInitLiteralFunc: CHIR.Func = pkg.packageInitLiteralFunc
  def typeDefs: Iterator[CHIR.CustomTypeDef] = pkg.typeDefs

  def valueId(value: CHIR.Value): Long = pkg.valueId(value)
  def expressionId(expression: CHIR.Expression): Long = pkg.expressionId(expression)
  def definitionId(definition: CHIR.CustomTypeDef): Long = pkg.definitionId(definition)
  def newGoto(destination: CHIR.Block, debugLoc: Option[CHIR.DebugLocation] = None): CHIR.Goto =
    pkg.newGoto(destination, debugLoc)
  def write(output: java.nio.file.Path): Unit = {
    if (patches.nonEmpty) CodeHelpers.shouldNotReachHere(
      "Compiler-only synthetic patches are not serialized; load the original package with CHIR.read")
    pkg.write(output)
  }

  // APIs currently called only by tests in this repository (excluding forwarders).
  def typeId(tpe: CHIR.Type): Long = pkg.typeId(tpe)
  def newBlock(isLandingPad: Boolean = false): CHIR.Block = pkg.newBlock(isLandingPad)
  def newBlockGroup(blocks: Seq[CHIR.Block], entryBlock: CHIR.Block): CHIR.BlockGroup =
    pkg.newBlockGroup(blocks, entryBlock)
  def newBranch(condition: CHIR.Value, trueBlock: CHIR.Block, falseBlock: CHIR.Block,
                         debugLoc: Option[CHIR.DebugLocation] = None): CHIR.Branch =
    pkg.newBranch(condition, trueBlock, falseBlock, debugLoc)

}
