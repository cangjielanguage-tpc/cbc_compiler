/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.newbaseline.backend

import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.compiler.bytecode.Slots
import com.huawei.excelsior.jet.compiler.newbaseline.DEBUG_PRINT
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.{Locations, NodeType}
import com.huawei.excelsior.jet.compiler.newbaseline.frontend.{Block, GlobalLiveness}

class GlobalInfo(slots: Slots, val blocksCount: Int, liveness: GlobalLiveness,
                 hasHandlers: Boolean, val structuredLocking: Boolean) {

  private val blocksStartLocations = new Array[Array[Location]](blocksCount)
  private val blocksEndLocations = new Array[Array[Location]](blocksCount)
  private val handlersStartLocations = if (hasHandlers) new Array[Location](slots.totalCount) else null

  private val blocksStartTypes = new Array[Array[NodeType]](blocksCount)

  def isSlotAliveAtBlockStart(slotIdx: Int, block: Block) = liveness.isSlotAliveAtBlockStart(slotIdx, block)

  def isSlotAliveAtBlockEnd(slotIdx: Int, block: Block) = liveness.isSlotAliveAtBlockEnd(slotIdx, block)

  def isSlotAliveAtHandler(slotIdx: Int, block: Block) = liveness.isSlotAliveAtHandler(slotIdx, block)

  private def locationsForBlock(locationsMap: Array[Array[Location]], block: Block) = {
    val locs = locationsMap(block.id)
    if (locs != null) {
      locs
    } else {
      val locsNew = new Array[Location](slots.totalCount)
      locationsMap(block.id) = locsNew
      locsNew
    }
  }

  /** @return true if something has changed */
  private def setLocationAt(locations: Array[Location], slotIdx: Int, loc: Location) = {
    assert(loc != null)
    val prevLoc = locations(slotIdx)
    if (prevLoc == null) {
      assert(Locations.isInvalid(loc) || !locations.contains(loc), "same location for different slots")
      locations(slotIdx) = loc
      true
    } else {
      assert(prevLoc == loc)
      false
    }
  }

  /** @return true if something has changed */
  private def setLocationForAt(locationsMap: Array[Array[Location]], block: Block, slotIdx: Int, loc: Location) = {
    setLocationAt(locationsForBlock(locationsMap, block), slotIdx, loc)
  }

  def locationAtBlockStart(block: Block, slotIdx: Int) = {
    assert(isSlotAliveAtBlockStart(slotIdx, block))
    locationsForBlock(blocksStartLocations, block)(slotIdx)
  }

  def locationAtBlockEnd(block: Block, slotIdx: Int) = {
    assert(isSlotAliveAtBlockEnd(slotIdx, block))
    locationsForBlock(blocksEndLocations, block)(slotIdx)
  }

  def locationAtHandlersStart(slotIdx: Int) = {
    val loc = locationAtHandlersStartIfAny(slotIdx)
    assert(loc != null)
    loc
  }

  /** May return `null` if this slot is not alive in any handler. */
  def locationAtHandlersStartIfAny(slotIdx: Int) = handlersStartLocations(slotIdx)

  def setLocationAtBlockStart(block: Block, slotIdx: Int, loc: Location): Unit = {
    if (DEBUG_PRINT) {
      println(s"  Global regalloc: start of $block, ${slots.slotToString(slotIdx)} - $loc")
    }
    assert(isSlotAliveAtBlockStart(slotIdx, block))
    if (setLocationForAt(blocksStartLocations, block, slotIdx, loc)) {
      for {
        inEnd <- block.inputs
        inBlock = inEnd.block
        if isSlotAliveAtBlockEnd(slotIdx, inBlock)
      } {
        setLocationAtBlockEnd(inBlock, slotIdx, loc)
      }
    }
  }

  def setLocationAtBlockEnd(block: Block, slotIdx: Int, loc: Location): Unit = {
    if (DEBUG_PRINT) {
      println(s"  Global regalloc: end   of $block, ${slots.slotToString(slotIdx)} - $loc")
    }
    assert(isSlotAliveAtBlockEnd(slotIdx, block))
    if (setLocationForAt(blocksEndLocations, block, slotIdx, loc)) {
      for (outBlock <- block.end.outputs if isSlotAliveAtBlockStart(slotIdx, outBlock)) {
        setLocationAtBlockStart(outBlock, slotIdx, loc)
      }
    }
  }

  def setLocationAtHandlersStart(slotIdx: Int, loc: Location) = {
    if (DEBUG_PRINT) {
      println(s"  Global regalloc: start of handlers ${slots.slotToString(slotIdx)} - $loc")
    }
    assert(handlersStartLocations != null)
    setLocationAt(handlersStartLocations, slotIdx, loc)
  }

  private def typesForBlock(block: Block) = {
    val types = blocksStartTypes(block.id)
    if (types != null) {
      types
    } else {
      val typesNew = new Array[NodeType](slots.totalCount)
      blocksStartTypes(block.id) = typesNew
      typesNew
    }
  }

  def typeAtBlockStart(block: Block, slotIdx: Int) = {
    assert(isSlotAliveAtBlockStart(slotIdx, block))
    typesForBlock(block)(slotIdx)
  }

  def setTypeAtBlockStart(block: Block, slotIdx: Int, `type`: NodeType): Unit = {
    if (DEBUG_PRINT) {
      println(s"  Global type: start of $block, ${slots.slotToString(slotIdx)} - ${`type`}")
    }
    assert(isSlotAliveAtBlockStart(slotIdx, block))
    assert(`type` != null)

    val types = typesForBlock(block)
    val prevType = types(slotIdx)
    if (prevType == null) {
      types(slotIdx) = `type`
    } else {
      assert(prevType eq `type`)
    }
  }

  def setTypeAtBlockEnd(block: Block, slotIdx: Int, `type`: NodeType): Unit = {
    if (DEBUG_PRINT) {
      println(s"  Global type: end of $block, ${slots.slotToString(slotIdx)} - ${`type`}")
    }
    assert(isSlotAliveAtBlockEnd(slotIdx, block))

    for (outBlock <- block.end.outputs if isSlotAliveAtBlockStart(slotIdx, outBlock)) {
      setTypeAtBlockStart(outBlock, slotIdx, `type`)
    }
  }

  def setTypeAtHandler(block: Block, slotIdx: Int, `type`: NodeType): Unit = {
    if (DEBUG_PRINT) {
      println(s"  Global type: handler of $block, ${slots.slotToString(slotIdx)} - ${`type`}")
    }
    assert(isSlotAliveAtHandler(slotIdx, block))

    for (handler <- block.handlers if isSlotAliveAtBlockStart(slotIdx, handler)) {
      setTypeAtBlockStart(handler, slotIdx, `type`)
    }
  }
}