package com.huawei.excelsior.jet.compiler.chir.v13002

import com.huawei.excelsior.jet.compiler.chir.EditableCHIR

final class EditableBlockGroupImpl private[v13002] (
  val ownerPackage: EditablePackageImpl,
  private[v13002] val id: Long
) extends EditableCHIR.BlockGroup {
  private def table = ownerPackage.groupTable(id)
  def blocks: Seq[EditableCHIR.Block] = EditableStorage.ids(table.getBlocks).toSeq.map(ownerPackage.blockView)
  def entryBlock: EditableCHIR.Block = ownerPackage.blockView(table.getEntryBlock)
  def replaceBlocks(blocks: Seq[EditableCHIR.Block], entryBlock: EditableCHIR.Block): Unit =
    ownerPackage.replaceBlocks(id, blocks, entryBlock)
}
