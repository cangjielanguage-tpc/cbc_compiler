package com.huawei.excelsior.jet.compiler.chir

object PatchedPackage {
  def apply(pkg: CHIR.Package): PatchedPackage = {
    val patchGenerators = Seq(
      CHIRThrowPatch(pkg, ThrowPatch.symbolResolutionErrorPatch),
      CHIRThrowPatch(pkg, ThrowPatch.abstractMethodErrorPatch),
      CHIRCJEntryGenerator(pkg)
    )

    val startingId: Int = pkg.values.length

    val ids = startingId until (startingId + patchGenerators.length)
    val generators = patchGenerators.zip(ids).flatMap((generator, id) => generator.generatePatch(id))

    new PatchedPackage(pkg, generators)
  }
}

class PatchedPackage(pkg: CHIR.Package, patches: Seq[CHIR.Func]) extends CHIR.Package {
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
}
