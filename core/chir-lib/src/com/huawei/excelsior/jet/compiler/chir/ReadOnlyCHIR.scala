package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import java.nio.file.Path

// Implementation adapters for synthetic nodes and schema readers without editing support.
// Consumers continue to use the common CHIR interfaces.
private[chir] trait ReadOnlyPackage extends CHIR.Package {
  private def unsupported: Nothing = shouldNotCallThis("This CHIR package does not support serialized edits")
  def newGoto(destination: CHIR.Block, debugLoc: Option[CHIR.DebugLocation]): CHIR.Goto = unsupported
  def write(output: Path): Unit = unsupported
}

private[chir] trait ReadOnlyFunc extends CHIR.Func {
  private def unsupported: Nothing = shouldNotCallThis("This CHIR function does not support serialized edits")
  def ownerPackage: CHIR.Package = unsupported
  def replaceAttributes(attributes: Seq[CHIR.Attribute]): Unit = unsupported
  def moveBody(destination: CHIR.Func): Unit = unsupported
  def removeBody(): Unit = unsupported
  def replaceBody(body: Option[CHIR.BlockGroup], retVal: Option[CHIR.LocalVar]): Unit = unsupported
}

private[chir] trait ReadOnlyBlockGroup extends CHIR.BlockGroup {
  def ownerPackage: CHIR.Package = shouldNotCallThis("Read-only CHIR block group")
  def replaceBlocks(blocks: Seq[CHIR.Block], entryBlock: CHIR.Block): Unit =
    shouldNotCallThis("Read-only CHIR block group")
}

private[chir] trait ReadOnlyBlock extends CHIR.Block {
  def ownerPackage: CHIR.Package = shouldNotCallThis("Read-only CHIR block")
  def replaceTerminator(terminator: CHIR.Terminator): Unit = shouldNotCallThis("Read-only CHIR block")
  def replaceExpressions(nonTerminators: Seq[CHIR.Expression], terminator: CHIR.Terminator): Unit =
    shouldNotCallThis("Read-only CHIR block")
}

private[chir] trait ReadOnlyGlobalVar extends CHIR.GlobalVar {
  def replaceAttributes(attributes: Seq[CHIR.Attribute]): Unit = shouldNotCallThis("Read-only CHIR global")
}
