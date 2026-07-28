/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.wrappers

import com.huawei.excelsior.jet.assembler.{Label, Location}
import com.huawei.excelsior.jet.assembler.Location.{FReg, IReg}
import com.huawei.excelsior.jet.codeemitter.CodeEmitter
import com.huawei.excelsior.jet.compiler.{Domain, Environment, RTConst}
import com.huawei.excelsior.jet.compiler.abi.{ABI, Frame}
import com.huawei.excelsior.jet.compiler.ir.{BytecodeOffset, InlineContext, XInfo, XSiteKind}
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.Generator.XSitesWithoutHandler
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.{GlobalLocations, Locations, Node, Nodes}
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.{Generator, MethodGenerator}
import com.huawei.excelsior.jet.compiler.symlevel.{Method, MethodReference}

abstract class GeneratorContext(val env: Environment, val wrapper: Method) {

  private[wrappers] val symbolLinker = env.getSymbolLinker(wrapper)
  private[wrappers] var globalLocations: GlobalLocations = _
  private[wrappers] var frame: Frame[? <: IReg, ? <: FReg, ? <: ABI[?, ?]] = _
  private[wrappers] var emit: CodeEmitter = _
  private[wrappers] var nodes: Nodes = _
  private[wrappers] var locations: Locations = _
  private[wrappers] var gen: Generator = _
  private[wrappers] var xSiteCreator: XSitesWithoutHandler = _

  init()
  gen.setCurrentBytecodeOffset(BytecodeOffset.SYNTHETIC)

  protected def init(): Unit

  final protected def initGeneration0(globalLocations: GlobalLocations): Unit = {
    this.globalLocations = globalLocations
    this.frame = globalLocations.frame
    emit.setUp()
    locations = new Locations(globalLocations, emit)
    nodes = new Nodes(locations, emit, frame)
    locations.nodes = nodes
    xSiteCreator = new XSitesWithoutHandler
  }

  def finishGenerationExceptFrameDrop(): Unit = {
    val body = emit.tearDown()
    emit.setUp(wrapper)
    frame.makeLayout(Frame.Mode.FULL)
    frame.genBuildAndAdjustParams(wrapper.hasFrameDescriptor)
    emit.appendCode(body)
  }

  def finishGenerationWithReturnAndSendCode(): Unit = {
    finishGenerationExceptFrameDrop()
    frame.genDestroy(true)
    tearDownAndSendCode()
  }

  def tearDownAndSendCode(): Unit = {
    emit.alignStart(RTConst.MethodInfoFrameDescriptor.CODE_ALIGNMENT.intValue)
    val code = emit.freeze().tearDown()
    MethodGenerator.sendMethodCode(env, wrapper, code, frame, xSiteCreator.xinfo)
  }
}

