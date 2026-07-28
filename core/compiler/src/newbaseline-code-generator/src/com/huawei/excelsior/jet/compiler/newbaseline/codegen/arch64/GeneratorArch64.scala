/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.codegen.arch64

import com.huawei.excelsior.jet.assembler.Location.{AnyReg, IReg, MemBased, mem}
import com.huawei.excelsior.jet.assembler.{AsmType, Symbol}
import com.huawei.excelsior.jet.codeemitter.CodeEmitter
import com.huawei.excelsior.jet.compiler.bytecode.{BytecodeTypeKind, ConvertOp}
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.{GlobalLocations, Locations, Node, Nodes}
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.{GenerationContext, Generator}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import com.huawei.excelsior.jet.compiler.{Environment, SymbolLinker}

import scala.annotation.nowarn

@nowarn("msg=match may not be exhaustive")
abstract class GeneratorArch64 protected(val _env: Environment, val _symbolLinker: SymbolLinker, val _context: GenerationContext,
                                         val _emit: CodeEmitter, val _globalLocations: GlobalLocations, val _locations: Locations,
                                         val _nodes: Nodes, val _xSites: Generator.XSiteCreator, val _enableOptimizedEnrichGeneration: Boolean)
  extends Generator(_env, _symbolLinker, _context, _emit, _globalLocations, _locations, _nodes, _xSites, _enableOptimizedEnrichGeneration) {

  final protected def bindToAnyFreeReg(n: Node, t: AsmType): AnyReg =
    if (t.isFloatingPoint) nodes.bindToAnyFreeFReg(n) else nodes.bindToAnyFreeIReg(n)

  final protected def bindToAnyFreeReg(n: Node, t: BytecodeTypeKind): AnyReg =
    bindToAnyFreeReg(n, t.toAsm)

  final protected def bindToAnyFreeReg(n: Node, t: SignatureType): AnyReg =
    bindToAnyFreeReg(n, t.toAsm)

  final protected def loadToReg(n: Node, t: AsmType): AnyReg =
    if (t.isFloatingPoint) nodes.loadToFReg(n) else nodes.loadToIReg(n)

  final protected def loadToReg(n: Node, t: BytecodeTypeKind): AnyReg =
    loadToReg(n, t.toAsm)

  final protected def loadToReg(n: Node, t: SignatureType): AnyReg =
    loadToReg(n, t.toAsm)

  protected def signExtendIntToLong(dst: IReg, src: IReg): Unit

  override final def genConvertLong(op: ConvertOp, arg: Node, result: Node): Unit = {
    val argLoc = nodes.loadToIRegAndReleaseIfNotUsedLater(arg)
    val resultLoc = nodes.bindToAnyFreeIReg(result)
    op match {
      case ConvertOp.I2L => signExtendIntToLong(resultLoc, argLoc)
      case ConvertOp.L2I => emit.mov32(resultLoc, argLoc) // just truncate
    }
  }

  override final def readStaticField(sigType: SignatureType, isAtomic: Boolean, field: Symbol, result: Node): Unit = {
    val resultLoc = bindToAnyFreeReg(result, sigType)
    emit.load(resultLoc, mem(sigType.toAsm, field))
    genDepriveIfNeeded(result, sigType)
  }

  override final def writeStaticField(sigType: SignatureType, isAtomic: Boolean, field: Symbol, value: Node): Unit = {
    val enrichedValue = if (sigType.isInterface) {
      genEnrichedCopyAndReleaseIfNotUsedLater(value, sigType)
    } else {
      value
    }
    val enrichedValueLoc = loadToReg(enrichedValue, sigType)
    emit.store(mem(sigType.toAsm, field), enrichedValueLoc)
    nodes.releaseLocIfNotUsedLater(enrichedValue)
  }

  override def readInstanceField(sigType: SignatureType, isAtomic: Boolean, obj: Node, fieldOffset: Int, result: Node, releaseObject: Boolean = true): Unit = {
    val fieldAddr = instanceFieldAddr(nodes.loadToIReg(obj), sigType.toAsm, fieldOffset)
    if (releaseObject) nodes.releaseLocIfNotUsedLater(obj)
    val resultLoc = bindToAnyFreeReg(result, sigType)
    emit.load(resultLoc, fieldAddr)
    genDepriveIfNeeded(result, sigType)
  }

  override final def writeInstanceField(sigType: SignatureType, isAtomic: Boolean, obj: Node, fieldOffset: Int, value: Node): Unit = {
    val enrichedValue = if (sigType.isInterface) {
      genEnrichedCopy(value, sigType)
    } else {
      value
    }
    val enrichedValueLoc = loadToReg(enrichedValue, sigType)
    val objectReg = nodes.loadToIReg(obj)
    val fieldAddr = instanceFieldAddr(objectReg, sigType.toAsm, fieldOffset)
    emit.store(fieldAddr, enrichedValueLoc)
    nodes.releaseLocIfNotUsedLater(value, enrichedValue, obj)
  }

  override final def readArrayElem(kind: BytecodeTypeKind, array: Node, index: Node, result: Node): Unit = {
    val arrayReg = nodes.loadToIReg(array)
    val indexReg = nodes.loadToIReg(index)
    nodes.releaseLocIfNotUsedLater(array, index)
    val resultLoc = bindToAnyFreeReg(result, kind)
    emit.load(resultLoc, arrayElemAddr(arrayReg, indexReg, kind))
  }

  override final def writeArrayElem(kind: BytecodeTypeKind, array: Node, index: Node, value: Node): Unit = {
    val arrayReg = nodes.loadToIReg(array)
    val indexReg = nodes.loadToIReg(index)
    val valueLoc = loadToReg(value, kind)
    emit.store(arrayElemAddr(arrayReg, indexReg, kind), valueLoc)
    nodes.releaseLocIfNotUsedLater(value, index, array)
  }
}
