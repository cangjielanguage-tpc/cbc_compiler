package com.huawei.excelsior.jet.compiler.chir

import java.nio.file.Path

/** Editing contracts for package-owned CHIR nodes.
  *
  * These interfaces extend the read interfaces in [[CHIR]]. An editable package
  * can therefore be passed to existing readers and compiler passes unchanged.
  * Implementations must retain every field of their versioned serialization
  * model, including fields that the CHIR read interfaces do not expose.
  *
  * [[v13002.EditablePackageImpl]] implements these contracts with an unpacked
  * FlatBuffers backing model. Existing read-only PackageImpl loaders remain
  * available for callers that do not need edits.
  */
object EditableCHIR {
  def read(input: Path, version: Int = CHIR.defaultVersion): Package = {
    require(version == CHIR.defaultVersion, "Editable CHIR currently supports v13002 only")
    v13002.EditablePackageImpl.read(input)
  }

  trait Owned {
    def ownerPackage: Package
  }

  trait Package extends CHIR.Package {
    def formatVersion: Int

    override def function(idx: Int): Func
    override def getFunc(identifier: String): Option[Func]

    /** Return an editable view of the same registered entity, without copying
      * its body or allocating a new serialized ID. Reject foreign nodes.
      */
    def edit(function: CHIR.Func): Func
    def edit(blockGroup: CHIR.BlockGroup): BlockGroup
    def edit(block: CHIR.Block): Block

    /** IDs are 1-based and belong to separate serialized entity tables.
      * Lookups must use registered identity, not names or structural equality.
      * Reject unregistered nodes rather than silently assigning them IDs.
      */
    def valueId(value: CHIR.Value): Long
    def expressionId(expression: CHIR.Expression): Long
    def typeId(tpe: CHIR.Type): Long
    def definitionId(definition: CHIR.CustomTypeDef): Long

    /** Factories register nodes immediately. A new block or expression can be
      * temporarily unattached while a transformation constructs its graph.
      * All referenced nodes must belong to this package.
      */
    def newBlock(isLandingPad: Boolean = false): Block
    def newBlockGroup(blocks: Seq[Block], entryBlock: Block): BlockGroup
    def newGoto(
      destination: CHIR.Block,
      debugLoc: Option[CHIR.DebugLocation] = None
    ): CHIR.Goto
    def newBranch(
      condition: CHIR.Value,
      trueBlock: CHIR.Block,
      falseBlock: CHIR.Block,
      debugLoc: Option[CHIR.DebugLocation] = None
    ): CHIR.Branch

    /** Remove unreachable blocks from this function's body groups. Preserve
      * stable entity IDs; unreferenced serialized entries may remain allocated.
      * Update affected ownership and predecessor relationships.
      */
    def removeUnreachableBlocks(function: Func): Unit

    /** Move a non-member function's body to a function with one extra leading
      * receiver parameter. Remap parameter and generic-type references in memory;
      * preserve the destination signature and turn the source into a declaration.
      * Generic bounds must match after substitution. Composite destination types
      * must already exist. Nested groups, lambdas, spawn and self references are
      * unsupported. Both functions must belong to this package.
      */
    def moveBody(source: Func, destination: Func): Unit

    /** Report invalid references, ownership, control flow and types. Merely
      * being serializable by FlatBuffers does not imply valid CHIR.
      */
    def validate(): Seq[ValidationIssue]

    /** Serialize the current graph using the versioned backing model. Preserve
      * fields not exposed by CHIR, and the format's file-identifier convention.
      * Validate first; reject an invalid graph before replacing the output file.
      */
    def write(output: Path): Unit
  }

  trait Func extends CHIR.Func with Owned {
    override def body: Option[BlockGroup]

    /** Replace the body and return-local reference together. Keep the function's
      * signature and parameter identities. Update group ownership and counters.
      * Reject a body owned by another function: moving/cloning a body requires
      * explicit parameter and generic-type remapping, not just this assignment.
      * A missing body represents a declaration, not an empty implementation.
      */
    def replaceBody(body: Option[BlockGroup], retVal: Option[CHIR.LocalVar]): Unit
  }

  trait BlockGroup extends CHIR.BlockGroup with Owned {
    override def blocks: Seq[Block]
    override def entryBlock: Block

    /** The entry must be in blocks. Maintain block-parent links and affected
      * predecessors. Reject duplicate blocks or incompatible existing owners.
      * Graph edits may be staged; validate before writing.
      */
    def replaceBlocks(blocks: Seq[Block], entryBlock: Block): Unit
  }

  trait Block extends CHIR.Block with Owned {
    /** Replace the terminator, maintaining expression ownership and successor
      * predecessor lists. Preserve the other expressions and their identities.
      * The terminator must be registered and not attached to another block.
      */
    def replaceTerminator(terminator: CHIR.Terminator): Unit

    /** Replace the complete ordered instruction sequence. The first sequence
      * must contain no terminators. Maintain ownership, predecessors and result
      * local/associated-expression relationships; reject foreign nodes.
      */
    def replaceExpressions(
      nonTerminators: Seq[CHIR.Expression],
      terminator: CHIR.Terminator
    ): Unit
  }

  enum EntityTable {
    case Types, Values, Expressions, Definitions
  }

  final case class EntityRef(table: EntityTable, id: Long)

  final case class ValidationIssue(message: String, entity: Option[EntityRef] = None)
}
